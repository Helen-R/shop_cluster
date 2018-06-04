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
# setorder(dt.dly, order.date, nth.daily.order)
save(dt.dly, file = sprintf("data/intermediates/dt.dly_%s.RData", shop.id))
slackme(sprintf("%s dt.dly done", shop.id), st.tm)



# product dt.dly
shop.id <- 14
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

unique(dt.ts$StatusDef)
# dt.tg.status <- dt.ts[, .N, by = .(SalePageId, MemberId, StatusDef)]
# dt.tg.status[, n.status:=.N, by = .(MemberId, TradesOrderGroupCode)]
dt.ts <- dt.ts[!StatusDef %in% c("Cancel", "Fail", "CreditCheckFail")]
dt.prod <- dt.ts[, .(Qty=sum(Quantity),
                     gross.sales=sum(SalesOrderSlaveTotalPayment),
                     promo.discnt=sum(PromotionDiscount),
                     ecoup.discnt=sum(ECouponDiscount)), by = .(ShopId, order.date, SalePageId, MemberId)]
dt.dly <- dt.prod[, .(N=.N,
                      gross.sales=sum(gross.sales),
                      promo.discnt=sum(promo.discnt),
                      ecoup.discnt=sum(ecoup.discnt),
                      Qty=sum(Qty)), by = .(ShopId, order.date, SalePageId)]

setorder(dt.dly, ShopId, SalePageId, order.date)
dt.dly[, `:=` (n.days.order=.N,
               nth.daily.order=1:.N,
               last.order.date=shift(order.date, 1L)), 
       by = .(ShopId, SalePageId)]
dt.dly[, recency.days:=as.integer(order.date - last.order.date)]
# plot(dly[, .N, by = .(r)][!is.na(r)])
# setorder(dt.dly, order.date, nth.daily.order)
save(dt.dly, file = sprintf("data/intermediates/dt.dly_prod_%s.RData", shop.id))
slackme(sprintf("%s product dt.dly done", shop.id), st.tm)

