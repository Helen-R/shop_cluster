source("../auxiliary/mylib.R")
source("../auxiliary/slackme.R")
mylib(c("data.table", "reshape2", "dplyr", "ggplot2", "ggfortify"))
# shop.ls <- list.files("data/raw_input/") %>% gsub(".csv", "", .) %>% 
#     strsplit(., "_") %>% lapply(., "[", 2) %>% unlist() %>% as.integer
for (shop.id in c(11, 14, 2131, 360)) {
# shop.id <- 11
dt.ts <- fread(sprintf("data/raw_input/TgAll_%s.csv", shop.id), fill=TRUE)
setnames(dt.ts, colnames(dt.ts), c("SalesOrderSlaveId"
                                   ,"DateId"
                                   ,"MemberId"
                                   ,"TradesOrderSlaveId"
                                   ,"SupplierId"
                                   ,"ShopId"
                                   ,"SalePageId"
                                   ,"TradesOrderGroupCode"
                                   ,"TrackSourceTypeDef"
                                   ,"TrackChannelTypeDef"
                                   ,"TrackDeviceTypeDef"
                                   ,"TradesOrderCode"
                                   ,"TradesOrderSlaveCode"
                                   ,"SaleProductTitle"
                                   ,"SaleProductSKUPropertyNameSet"
                                   ,"IsMajor"
                                   ,"IsGift"
                                   ,"Quantity"
                                   ,"UnitPrice"
                                   ,"PromotionDiscount"
                                   ,"ECouponDiscount"
                                   ,"SalesOrderSlaveTotalPayment"
                                   ,"SalesOrderSlaveDateTime"
                                   ,"SalesOrderSlaveStatusDef"
                                   ,"SalesOrderSlaveStatusUpdatedDateTime"
                                   ,"PayProfileTypeDef"
                                   ,"ShippingProfileTypeDef"
                                   ,"TemperatureTypeDef"
                                   ,"UpdatedDateTime"
                                   ,"SourceId"
                                   ,"ECouponId"
                                   ,"IsSalePageGift"
                                   ,"SalesOrderReceiverId"
                                   ,"CreatedDateTime"
                                   ,"StatusDef"
                                   ,"UnifiedUserId"
                                   ,"BeforeCrmShopMemberCardLevel"
                                   ,"CrmShopMemberCardLevel"
                                   ,"LocationId"))

dt.ts[TrackSourceTypeDef=="Web", TrackSourceTypeDef:=paste(TrackSourceTypeDef, TrackDeviceTypeDef, sep="_")]
# deal with time
col.time <- colnames(dt.ts)[grep("[Tt]ime", colnames(dt.ts))]
dt.ts[, (col.time):=lapply(.SD, function(t) {as.POSIXct(t, format = "%Y-%m-%d %H:%M:%OS", tz = "GMT")}), .SDcol=col.time]
setorder(dt.ts, TradesOrderGroupCode, SalesOrderSlaveDateTime)
dt.ts[, order.datetime:=SalesOrderSlaveDateTime[1], by = .(TradesOrderGroupCode)]
dt.ts[, order.date:=as.Date(order.datetime)]

slackme("datetime done", st.tm)

# channel type: Mall/Brand (?)
# dt.ts[, lapply(.SD, min), by = TrackChannelTypeDef, .SDcols=c("CreatedDateTime","UpdatedDateTime","SalesOrderSlaveStatusUpdatedDateTime","SalesOrderSlaveDateTime")]

# # check if every order has same SalesOrderSlaveDateTime (Ans: No)
# dt.ts[, .(N=uniqueN(SalesOrderSlaveDateTime)), by = .(TradesOrderGroupCode)] %>% subset(., N==max(N))

dt.tg.status <- dt.ts[, .N, by = .(MemberId, TradesOrderGroupCode, StatusDef)]
dt.tg.status[, n.status:=.N, by = .(MemberId, TradesOrderGroupCode)]
setorder(dt.tg.status, MemberId, TradesOrderGroupCode)
dt.tg <- dcast.data.table(dt.tg.status, MemberId + TradesOrderGroupCode + n.status ~ StatusDef, 
                          value.var = "N", fill = 0)
# add OrderDateTime and OrderStatus on tg level
dt.tg.time <- dt.ts[, .(order.datetime=SalesOrderSlaveDateTime[1]), by = .(TradesOrderGroupCode)]
dt.tg[dt.tg.time, order.datetime:=order.datetime, on = "TradesOrderGroupCode"]

# add n.order / nth.order
setorder(dt.tg, MemberId, order.datetime)
dt.tg[, n.order.overall:=.N, by = MemberId]
dt.tg[, nth.order.overall:=1:.N, by = MemberId]

# # partial cancellation propotion between first and return customer
# nrow(dt.tg[n.status>1&n.order.overall==1]) / nrow(dt.status[n.status>1])
# nrow(dt.tg[n.status>1&nth.order.overall==1&n.order.overall>1]) / nrow(dt.status[n.status>1&n.order.overall>1])
# nrow(dt.tg[n.status>1&n.order.overall!=nth.order.overall]) / nrow(dt.status[n.status>1])

# add statusdef by tg level
tg.status.money <- dt.ts[, .(tg.status=StatusDef[1],
                             gross.sales=sum(SalesOrderSlaveTotalPayment),
                             promo.discnt=sum(PromotionDiscount),
                             ecoup.discnt=sum(ECouponDiscount)), by = .(TradesOrderGroupCode)]
dt.tg <- tg.status.money[dt.tg, on = "TradesOrderGroupCode"]
dt.tg[n.status>1, tg.status:=NA]

# # last.order
# dt.status[, `:=` (last.order:=shift(.SD))]

dt.tg.finish <- dt.tg[tg.status=="Finish"]
setorder(dt.tg.finish, MemberId, order.datetime)
dt.tg.finish[, n.order:=.N, by = MemberId]
dt.tg.finish[, nth.order:=1:.N, by = MemberId]
# nth days
dt.tg.finish[, order.date:=as.Date(order.datetime)]
dt.dly <- dt.tg.finish[, .(n.daily.order=.N, min.nth.order=nth.order[1],
                           gross.sales=sum(gross.sales),
                           promo.discnt=sum(promo.discnt),
                           ecoup.discnt=sum(ecoup.discnt)), 
                       by = .(MemberId, order.date)]
dt.dly[, `:=` (n.daily.order=.N,
               nth.daily.order=1:.N,
               last.order.date=shift(order.date, 1L)), by = MemberId]
dt.dly[, recency.days:=as.integer(order.date - last.order.date)]
# plot(dly[, .N, by = .(r)][!is.na(r)])

setorder(dt.dly, order.date, nth.daily.order)
save(dt.dly, file = sprintf("data/intermediates/dt.dly_%s.RData", shop.id))
dtn.ts <- dt.dly[dt.ts, on = .(MemberId, order.date)]
slackme(sprintf("%s dt.dly done", shop.id), st.tm)
}
# # check if order date got it right
# dtn[, order.date.id:=as.integer(format(order.date, "%Y%m%d"))]
# # dtn[DateId!=order.date.id, .(DateId, order.date)]

dtn.ts[dt.tg.finish, `:=` (tg.status=tg.status,
                           n.order.overall=n.order.overall,
                           nth.order.overall=nth.order.overall,
                           n.order=n.order,
                           nth.order=nth.order), on = .(TradesOrderGroupCode)]


#### TODO: add rules to deal with Cancel, Fail, Overdue
dtn.ts.finish <- dtn.ts[tg.status=="Finish"]
# daily sales
dly.sales <- dtn.ts.finish[, .(gross.sales=sum(SalesOrderSlaveTotalPayment),
                               promo.discnt=sum(PromotionDiscount),
                               ecoup.discnt=sum(ECouponDiscount)), by = .(order.date)]
# fill missing dates with 0
min.order.date <- min(dly.sales$order.date)
date.seq <- data.table(order.date=seq(min.order.date, max(dly.sales$order.date), by = "day"))
dly.sales <- dly.sales[date.seq, on = "order.date"][is.na(gross.sales), gross.sales:=0][is.na(promo.discnt), promo.discnt:=0][is.na(ecoup.discnt), ecoup.discnt:=0]
setorder(dly.sales, order.date)
interval.first.or.return <- c(0, 1, 5, 10)
m <- max(dtn.ts.finish$nth.daily.order)
if (length(which(m < interval.first.or.return))==0) interval.first.or.return <- c(interval.first.or.return, m)
interval.active.or.seal.cust <- c(0, 7, 30, 90, 365)
m <-  max(dtn.ts.finish$recency.days, na.rm = T)
if (length(which(m < interval.active.or.seal.cust))==0) interval.active.or.seal.cust <- c(interval.active.or.seal.cust, m)
first.or.return.cust.labels <- c("first", "return", "heavy", "vip")
active.or.seal.cust.labels <- c("day", "week", "month", "quater", "year")
dtn.ts.finish[, first.or.return.cust:=cut(nth.daily.order, interval.first.or.return, labels = first.or.return.cust.labels)]
dtn.ts.finish[, active.or.seal.cust:=cut(recency.days, interval.active.or.seal.cust, labels = active.or.seal.cust.labels)]
# # discount vs. gross.sales
# plot(dly.sales[, .(gross.sales, promo.discnt)])
# plot(dly.sales[, .(gross.sales, promo.discnt+ecoup.discnt)])
# plot(dly.sales[, .(gross.sales, ecoup.discnt)])
slackme("done", st.tm)
# dtn.ts.finish1 <- dtn.ts.finish
# dtn.ts.finish <- dtn.ts.finish[order.date>=as.Date('2016-06-01')]
dtn.first.return <- dtn.ts.finish[, .(gross.sales=sum(SalesOrderSlaveTotalPayment),
                                      promo.discnt=sum(PromotionDiscount),
                                      ecoup.discnt=sum(ECouponDiscount)), 
                                  by = .(order.date, first.or.return.cust, 
                                         TrackSourceTypeDef)]
track.source.labels <- unique(dt.ts$TrackSourceTypeDef)
# fill missing dates with 0
date.seq1 <- data.table(expand.grid(order.date=date.seq$order.date, 
                                    first.or.return.cust=first.or.return.cust.labels,
                                    TrackSourceTypeDef=track.source.labels))
dtn.first.return <- dtn.first.return[date.seq1, on = c("order.date", "first.or.return.cust", "TrackSourceTypeDef")][is.na(gross.sales), gross.sales:=0][is.na(promo.discnt), promo.discnt:=0][is.na(ecoup.discnt), ecoup.discnt:=0]
setorder(dtn.first.return, first.or.return.cust, order.date)
ggplot(dtn.first.return, aes(x = order.date, y = gross.sales, colour=first.or.return.cust)) + 
    geom_line() + facet_grid(. ~ TrackSourceTypeDef)
ggsave(sprintf("plots/plot1_%s.png", shop.id), width = 12, height = 2.5)
ggplot(dtn.first.return, aes(x = order.date, y = gross.sales, colour=first.or.return.cust)) + 
    geom_line() + facet_grid(first.or.return.cust ~ TrackSourceTypeDef)
ggsave(sprintf("plots/plot1-1_%s.png", shop.id), width = 12, height = 10)

dtn.return.recency <- dtn.ts.finish[, .(gross.sales=sum(SalesOrderSlaveTotalPayment),
                                        promo.discnt=sum(PromotionDiscount),
                                        ecoup.discnt=sum(ECouponDiscount)), 
                                    by = .(order.date, active.or.seal.cust, 
                                           TrackSourceTypeDef)][!is.na(active.or.seal.cust)]
date.seq2 <- data.table(expand.grid(order.date=date.seq$order.date, 
                                    active.or.seal.cust=active.or.seal.cust.labels,
                                    TrackSourceTypeDef=track.source.labels))
dtn.return.recency <- dtn.return.recency[date.seq2, on = c("order.date", "active.or.seal.cust", "TrackSourceTypeDef")][is.na(gross.sales), gross.sales:=0][is.na(promo.discnt), promo.discnt:=0][is.na(ecoup.discnt), ecoup.discnt:=0]
setorder(dtn.return.recency, active.or.seal.cust, order.date)
ggplot(dtn.return.recency, aes(x = order.date, y = gross.sales, colour=active.or.seal.cust)) + 
    geom_line() + facet_grid(active.or.seal.cust ~ TrackSourceTypeDef)
ggsave(sprintf("plots/plot2_%s.png", shop.id), width = 12, height = 10)
# }
# TODO: 存圖
# 看其他家
# 試跑在gcp上
# Time Series (window mean)
# 切 2016-06-30(?)
# identify peaks (with/without test)

# x <- dtn.return.recency[active.or.seal.cust=="quater"&TrackSourceTypeDef=="Web"&TrackDeviceTypeDef=="Mobile"]$gross.sales
# x <- dtn.first.return[first.or.return.cust=="return"&TrackSourceTypeDef=="Web"&TrackDeviceTypeDef=="Mobile"]$gross.sales
min.year <- year(min.order.date)
min.month <- month(min.order.date)
min.mday <- mday(min.order.date)
x <- dly.sales$gross.sales
# xtimeseries <- ts(x,frequency = 7,start=c(2016,20))
xtimeseries <- ts(x,frequency = 365,start=c(min.year,min.month,min.mday))
decomposed <- decompose(xtimeseries)
png(filename=sprintf("plots/plot3_%s.png", shop.id))
plot(decomposed)
dev.off()
decomposed <- stl(xtimeseries, s.window = "periodic")
png(filename=sprintf("plots/plot3-1_%s.png", shop.id))
plot(decomposed)
dev.off()

# TODO: (daily done)
# seasonal derivatives (classification)
# (weekly, quaterly, monthly, annually) (data points not enough of periodic?)
dt.sales <- dcast.data.table(dtn.first.return, order.date~first.or.return.cust+TrackSourceTypeDef, 
                             value.var = "gross.sales")
# dt.sales <- dt.sales[, -"order.date", with = F]
# ts.sales <- ts(dt.sales, frequency = 365, start = c(min.year,min.month,min.mday))
#xxx trend <- stl(ts.sales)
trend <- dtn.first.return[, .(trend=unclass(stl(ts(gross.sales,frequency = 365,
                                           start=c(min.year,min.month,min.mday)), s.window = "periodic")$time.series[, "trend"])),
                          by = .(first.or.return.cust, TrackSourceTypeDef)]
trend[, order.date:=date.seq$order.date, by = .(first.or.return.cust, TrackSourceTypeDef)]
trend.ttl <- unclass(decomposed$time.series[, "trend"])
# trend1 <- dcast.data.table(trend, order.date~first.or.return.cust+TrackSourceTypeDef, value.var = "trend")
# trend2 <- trend[, .(sum=sum(trend, na.rm = T)), by = order.date]
# trend2[, total:=trend.ttl]
# unclass(trend2$sum)[1] == unclass(trend2$total)[1]
trend.ttl <- data.table(first.or.return.cust="total",TrackSourceTypeDef="total", 
                        trend=trend.ttl, order.date=date.seq$order.date)
trend1 <- rbind(trend, trend.ttl)
setorder(trend1, order.date)
ggplot(trend, aes(x = order.date, y = trend, colour=first.or.return.cust)) + 
    geom_line() + facet_grid(first.or.return.cust~TrackSourceTypeDef)
ggsave(sprintf("plots/plot4_%s.png", shop.id), width = 12, height = 10)
# }

# product
dt.product <- dtn.ts.finish[, .(Qty=sum(Quantity), N=.N, 
                                sale.start=min(order.date), sale.end=max(order.date),
                                sale.periods=max(order.date)-min(order.date),
                                sales.dates=uniqueN(DateId)), by = .(SalePageId)]
setorder(dt.product, -N)
setorder(dt.product, sale.start, -N)

dtn.ts.finish[, `:=` (nth.sale.days=as.integer(order.date-min(order.date))+1), by = .(SalePageId)]


tmp <- dtn.ts.finish[, .(N=.N, Qty=sum(Quantity)), by = .(SalePageId, nth.sale.days)]
setorder(tmp, SalePageId, nth.sale.days)
tmp[, `:=` (ttl.N=sum(N),
            cum.N=cumsum(N)), by = SalePageId]

tmp1 <- dtn.ts.finish[, .(N=.N, Qty=sum(Quantity)), by = .(SalePageId, nth.sale.days)]
setorder(tmp1, SalePageId, nth.sale.days)
tmp1[, `:=` (ttl.N=sum(N),
             cum.N=cumsum(N)), by = SalePageId]


# tmp <- dt.status[is.na(StatusDef)][, .(Cancel, CreditCheckFail, Fail, Finish, Overdue, 
#                                        WaitingToCreditCheck, WaitingToPay, WaitingToShipping)]
# tmp <- unique(tmp)
# 
# dt.status[, .N, by = StatusDef]
# dt.status[StatusDef=="Fail" & nth.order==n.order, .N]
# 
# dt.status[StatusDef=="WaitingToShipping", .(max(CreatedDateTime), min(CreatedDateTime))]
# 
# tmp1 <- dt.status[StatusDef=="WaitingToShipping", .(as.Date(CreatedDateTime))]
# plot(tmp1[, .N, by = V1] %>% subset(N>9))
# plot(tmp1[V1>as.Date("2017-07-15"), .N, by = V1] %>% subset(N>13))
# tmp1[V1>as.Date("2017-07-15"), .N, by = V1] %>% subset(N>13) %>% summary()
# 
# dtn[, n.order.done := sum(StatusDef=="Finish"), by = .(MemberId)]
# dtn.alldone <- dtn[n.order.done==n.order]
# dtn1 <- dtn[n.order.done!=n.order]
# 
# dt.status[, .N, by = .(MemberId, StatusDef)]

