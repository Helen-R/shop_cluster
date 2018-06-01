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
