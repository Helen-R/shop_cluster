source("../auxiliary/mylib.R")
source("../auxiliary/slackme.R")
mylib(c("data.table", "reshape2", "dplyr", "plyr", "ggplot2", "ggfortify"))
shop.id <- "all"
dt.dly <- fread("data/raw_input/DataForRFM.csv")
setnames(dt.dly, colnames(dt.dly), c("DateId", "ShopId", "MemberId", "TotalPayment", 
                                   "PromotionDiscount", "ECouponDiscount", 
                                   "TotalDiscount", "TgCounts", "Quantity", 
                                   "CreatedDateTime"))
# deal with time
col.time <- colnames(dt.dly)[grep("[Tt]ime", colnames(dt.dly))]
dt.dly[, (col.time):=lapply(.SD, function(t) {as.POSIXct(t, format = "%Y-%m-%d %H:%M:%OS", 
                                                        tz = "GMT")}), .SDcol=col.time]
dt.dly[, order.date:=as.Date(as.character(DateId), format="%Y%m%d", origin = "1970-01-01")]
slackme("datetime done", st.tm)
setorder(dt.dly, ShopId, MemberId, order.date)
dt.dly[, `:=` (n.days.order=.N,
               nth.daily.order=1:.N,
               last.order.date=shift(order.date, 1L)), by = .(ShopId, MemberId)]
dt.dly[, recency.days:=as.integer(order.date - last.order.date)]
# plot(dly[, .N, by = .(r)][!is.na(r)])
setorder(dt.dly, order.date, nth.daily.order)
save(dt.dly, file = sprintf("data/intermediates/dt.dly_%s.RData", shop.id))
slackme(sprintf("%s dt.dly done", shop.id), st.tm)

dly.sales <- dt.dly[, .(gross.sales=sum(TotalPayment),
                               promo.discnt=sum(PromotionDiscount),
                               ecoup.discnt=sum(ECouponDiscount)), by = .(ShopId, order.date)]
# fill missing dates with 0
setorder(dly.sales, ShopId, order.date)
min.max.date <- dly.sales[, .(min.dt=min(order.date),
              max.dt=max(order.date)), by = ShopId]
date.seq <- lapply(min.max.date$ShopId, function(shop.id) {
    min.dt <- min.max.date[ShopId==shop.id]$min.dt
    max.dt <- min.max.date[ShopId==shop.id]$max.dt
    data.frame(ShopId=shop.id, order.date=seq.Date(min.dt, max.dt, "day"))
}) %>% ldply(.)
dly.sales <- dly.sales[date.seq, on = c("ShopId", "order.date")][is.na(gross.sales), gross.sales:=0][is.na(promo.discnt), promo.discnt:=0][is.na(ecoup.discnt), ecoup.discnt:=0]
setorder(dly.sales, order.date)
interval.first.or.return <- c(0, 1, 5, 10)
m <- max(dt.dly$nth.daily.order, na.rm = T)
if (length(which(m < interval.first.or.return))==0) interval.first.or.return <- c(interval.first.or.return, m)
interval.active.or.seal.cust <- c(0, 7, 30, 90, 365)
m <-  max(dt.dly$recency.days, na.rm = T)
if (length(which(m < interval.active.or.seal.cust))==0) interval.active.or.seal.cust <- c(interval.active.or.seal.cust, m)
first.or.return.cust.labels <- c("first", "return", "heavy", "vip")
active.or.seal.cust.labels <- c("day", "week", "month", "quater", "year")
dt.dly[, first.or.return.cust:=cut(nth.daily.order, interval.first.or.return, labels = first.or.return.cust.labels)]
dt.dly[, active.or.seal.cust:=cut(recency.days, interval.active.or.seal.cust, labels = active.or.seal.cust.labels)]
dt.dly.stats <- dt.dly[, .(st.dt = min(order.date), 
                           ed.dt = max(order.date),
                           duration = as.integer(max(order.date)-min(order.date))), by = ShopId]
# dt.dly.stats[duration>365*2, .N]
# # discount vs. gross.sales
# plot(dly.sales[, .(gross.sales, promo.discnt)])
# plot(dly.sales[, .(gross.sales, promo.discnt+ecoup.discnt)])
# plot(dly.sales[, .(gross.sales, ecoup.discnt)])
slackme("done", st.tm)
