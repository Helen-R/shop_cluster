if (!"mylib" %in% ls()) source("../auxiliary/mylib.R")
if (!"slackme" %in% ls()) source("../auxiliary/slackme.R")
mylib(c("data.table", "magrittr", "googleCloudStorageR", "plyr", "ggplot2", "anomalize"))

shop.id <- "all"
fnm <- sprintf("data/intermediates/dt.dly_%s.RData", shop.id)   # dt.dly
load(fnm)
setnames(dt.dly, "TotalPayment", "gross.sales")
suffix <- "ts_all"
if(!dir.exists("plots/ts_all/")) dir.create("plots/ts_all/")

setorder(dt.dly, ShopId, MemberId, order.date)
if (nrow(dt.dly[is.na(n.days.order)|is.na(nth.daily.order)]) > 0) {
    dt.dly[, `:=` (n.days.order=.N,
                   nth.daily.order=1:.N,
                   last.order.date=shift(order.date, 1L)), by = .(ShopId, MemberId)]
}
# daily sales----
dly.sales <- dt.dly[, .(gross.sales=sum(gross.sales),
                        promo.discnt=sum(PromotionDiscount),
                        ecoup.discnt=sum(ECouponDiscount)), by = .(ShopId, order.date)]
## fill missing dates with 0
setorder(dly.sales, ShopId, order.date)
min.max.date <- dly.sales[, .(min.dt=min(order.date),
                              max.dt=max(order.date)), by = ShopId]
date.seq <- lapply(min.max.date$ShopId, function(shop.id) {
    min.dt <- min.max.date[ShopId==shop.id]$min.dt
    max.dt <- min.max.date[ShopId==shop.id]$max.dt
    data.frame(ShopId=shop.id, order.date=seq.Date(min.dt, max.dt, "day"))
}) %>% ldply(.)
dly.sales.raw <- dly.sales[date.seq, on = c("ShopId", "order.date")][is.na(gross.sales), gross.sales:=0][is.na(promo.discnt), promo.discnt:=0][is.na(ecoup.discnt), ecoup.discnt:=0]
setorder(dly.sales.raw, order.date)

dt.dly.stats <- dt.dly[, .(st.dt = min(order.date), 
                           ed.dt = max(order.date),
                           duration = as.integer(max(order.date)-min(order.date))), by = ShopId]

shop.ls <- dt.dly.stats[duration > 365*2]$ShopId
slackme("ts data prep done", st.tm)


shop.id <- 14
dly.sales <- dly.sales.raw[ShopId == shop.id] 
dly.sales %<>%  
    as_tbl_time(., index = order.date) %>% 
    time_decompose(gross.sales, method="stl", frequency = "6 month", trend = "12 months") %>% 
    anomalize(remainder, metho="iqr") %>% 
    time_recompose() 
View(dly.sales)
plot_anomaly_decomposition(dly.sales)
plot_anomalies(dly.sales, time_recomposed = TRUE)

dly.sales <- dly.sales.raw[ShopId == shop.id] 
# dly.sales %>% filter_time("2018-02" ~ "2018-03") %>% View()
dly.sales %<>% as_tbl_time(., index = order.date) %>% 
    filter_time("2017-12" ~ "2018-03") %>% 
    time_decompose(gross.sales, method="stl", frequency = "auto", trend = "1 month") %>% 
    anomalize(remainder, metho="iqr") %>% 
    time_recompose() 
plot_anomaly_decomposition(dly.sales)
plot_anomalies(dly.sales, time_recomposed = TRUE)
dly.sales %>% View()

# # calculate time series decomposition by shop
# slackme("start", st.tm)
# result.lss <- mclapply(shop.ls, function (shop.id) {
#     return(tryCatch({dly.sales <- dly.sales.raw[ShopId == shop.id] 
#     min.order.date <- min(dly.sales$order.date)
#     max.order.date <- max(dly.sales$order.date)
#     min.year <- year(min.order.date)
#     min.yday <- yday(min.order.date)
#     x <- dly.sales$gross.sales
#     # xtimeseries <- ts(x,frequency = 7,start=c(2016,20))
#     xtimeseries <- ts(x,frequency = 365,start=c(min.year,min.yday))
#     # decomposed <- decompose(xtimeseries)
#     decomposed <- stl(xtimeseries, s.window = "periodic")
#     # slackme(sprintf("Shop %s done", shop.id), st.tm)
#     # png(filename=sprintf("plots/ts_all/plot3_%s.png", shop.id))
#     # plot(decomposed)
#     # dev.off()
#     obj <- list(c(shop.id, as.character(min.order.date), as.character(max.order.date)), x, decomposed)
#     obj
#     }, error=function(e) sprintf("%s error", shop.id)))
# }, mc.preschedule = FALSE, mc.cores = 4)
# slackme("total ts plot done", st.tm)
# 
# system(sprintf("zip %s plots/%s/*", suffix, suffix))
# save(result.lss, file = file.path("data/intermediates/", sprintf("%s.result.lss.RData", suffix)))
# 
# default.bucket <- "helen-ml-4standard"
# Sys.setenv("GCS_AUTH_FILE" = "/home/helen/gcs.oauth",
#            "GCS_DEFAULT_BUCKET" = default.bucket)
# library(googleCloudStorageR)
# gcs_auth()
# gcs_global_bucket(default.bucket)
# # gcs_list_objects()
# gcs_upload(sprintf("%s.zip", suffix), name = sprintf("%s.zip", suffix))
# 
# file.copy("ts.decomposed.R", sprintf("scripts/ts.decomposed.%s.R", suffix), overwrite = T)
# slackme("ts_all done", st.tm)
 

# first or return sales / frequency / recency----
interval.first.or.return <- c(0, 1, 5, 10)
m <- max(dt.dly$nth.daily.order, na.rm = T)
if (length(which(m < interval.first.or.return))==0) 
    interval.first.or.return <- c(interval.first.or.return, m)
interval.active.or.seal.cust <- c(0, 7, 30, 180, 365)
m <-  max(dt.dly$recency.days, na.rm = T)
if (length(which(m < interval.active.or.seal.cust))==0) 
    interval.active.or.seal.cust <- c(interval.active.or.seal.cust, m)
first.or.return.cust.labels <- c("first", "return")
freq.class.cust.labels <- c("first", "return", "heavy", "vip")
active.or.seal.cust.labels <- c("day", "week", "month", "0.5year", "year")
dt.dly[, first.or.return.cust:=ifelse(nth.daily.order==1,
                                      first.or.return.cust.labels[1], 
                                      first.or.return.cust.labels[2])]
dt.dly[, freq.class.cust:=cut(nth.daily.order, interval.first.or.return, 
                                   labels = freq.class.cust.labels)]
dt.dly[, active.or.seal.cust:=cut(recency.days, interval.active.or.seal.cust, 
                                  labels = active.or.seal.cust.labels)]
dt.dly.stats.first.return <- dt.dly[, .(st.dt = min(order.date), 
                                        ed.dt = max(order.date),
                                        duration = as.integer(max(order.date)-min(order.date))), 
                                    by = .(ShopId, first.or.return.cust)]
shop.ls.first.return <- dt.dly.stats.first.return[, .(test=sum(duration> 365 * 2)), by = ShopId][test==2, .(ShopId)] %>% unlist()
# # discount vs. gross.sales
# plot(dly.sales[, .(gross.sales, promo.discnt)])
# plot(dly.sales[, .(gross.sales, promo.discnt+ecoup.discnt)])
# plot(dly.sales[, .(gross.sales, ecoup.discnt)])

dly.first.return <- dt.dly[, .(gross.sales=sum(gross.sales),
                               promo.discnt=sum(PromotionDiscount),
                               ecoup.discnt=sum(ECouponDiscount)), 
                           by = .(ShopId, order.date, first.or.return.cust)]
stopifnot(nrow(dly.first.return[is.na(first.or.return.cust)])==0)
# fill missing dates with 0
setorder(dly.first.return, ShopId, first.or.return.cust, order.date)
min.max.date.raw <- dly.first.return[, .(min.dt=min(order.date),
                                     max.dt=max(order.date)), 
                                 by = .(ShopId, first.or.return.cust)]
date.seq1 <- lapply(first.or.return.cust.labels, function (labs) {
    min.max.date <- min.max.date.raw[first.or.return.cust==labs]
    date.seq.raw <- mclapply(min.max.date$ShopId, function(shop.id) {
        min.dt <- min.max.date[ShopId==shop.id]$min.dt
        max.dt <- min.max.date[ShopId==shop.id]$max.dt
        data.frame(ShopId=shop.id, order.date=seq.Date(min.dt, max.dt, "day"))
    }) %>% ldply(.)
    date.seq.raw$first.or.return.cust <- labs
    return(date.seq.raw)
}) %>% ldply(.) %>% data.table()
date.seq1[, first.or.return.cust:=factor(first.or.return.cust,labels = first.or.return.cust.labels)]
dly.first.return.raw <- dly.first.return[date.seq1, on = c("order.date", "first.or.return.cust", "ShopId")][is.na(gross.sales), gross.sales:=0][is.na(promo.discnt), promo.discnt:=0][is.na(ecoup.discnt), ecoup.discnt:=0]
setorder(dly.first.return.raw, ShopId, first.or.return.cust, order.date)

shop.id <- 14
dly.sales <- dly.first.return.raw[ShopId == shop.id] %>% group_by(., first.or.return.cust)
setorder(dly.sales, order.date, first.or.return.cust)
dly.sales %<>%  
    as_tbl_time(., index = order.date) %>% 
    time_decompose(gross.sales, method="stl", frequency = "6 months", trend = "12 months") %>% 
    anomalize(remainder, metho="iqr") %>% 
    time_recompose() 
View(dly.sales)
plot_anomaly_decomposition(filter(dly.sales, first.or.return.cust=="first") %>% ungroup())
plot_anomaly_decomposition(filter(dly.sales, first.or.return.cust=="return") %>% ungroup())
plot_anomalies(dly.sales, time_recomposed = TRUE)

dly.sales <- dly.first.return.raw[ShopId == shop.id] %>% group_by(., first.or.return.cust)
setorder(dly.sales, order.date, first.or.return.cust)
# dly.sales %>% filter_time("2018-02" ~ "2018-03") %>% View()
dly.sales %<>%  
    as_tbl_time(., index = order.date) %>% 
    filter_time("2017-12" ~ "2018-03") %>% 
    time_decompose(gross.sales, method="stl", frequency = "auto", trend = "7 days") %>% 
    anomalize(remainder, metho="iqr") %>% 
    time_recompose() 
# plot_anomaly_decomposition(dly.sales)
plot_anomalies(dly.sales, time_recomposed = TRUE)
dly.sales %>% View()

dly.return.raw <- dt.dly[first.or.return.cust=="return", 
                         .(gross.sales=sum(gross.sales),
                           promo.discnt=sum(PromotionDiscount),
                           ecoup.discnt=sum(ECouponDiscount)), 
                         by = .(ShopId, order.date, freq.class.cust, 
                                active.or.seal.cust)]

slackme("ts data prep done again", st.tm)

# # calculate time series decomposition by shop
# slackme("start", st.tm)
# result.lss.first.return <- mclapply(shop.ls.first.return, function (shop.id) {
#     return(tryCatch({dtn.first.return <- dly.first.return.raw[ShopId==shop.id]
#     ggplot(dtn.first.return, aes(x = order.date, y = gross.sales, colour=first.or.return.cust)) +
#         geom_line() + facet_grid(. ~ first.or.return.cust)
#     ggsave(sprintf("plots/ts_all/plot1_%s.png", shop.id), width = 12, height = 2.5)
#     
#     dly.sales <- dtn.first.return[first.or.return.cust == "first"]
#     min.order.date <- min(dly.sales$order.date)
#     max.order.date <- max(dly.sales$order.date)
#     min.year <- year(min.order.date)
#     min.yday <- yday(min.order.date)
#     x <- dly.sales$gross.sales
#     # xtimeseries <- ts(x,frequency = 7,start=c(2016,20))
#     xtimeseries <- ts(x,frequency = 365,start=c(min.year,min.yday))
#     # decomposed <- decompose(xtimeseries)
#     decomposed <- stl(xtimeseries, s.window = "periodic")
#     png(filename=sprintf("plots/ts_all/plot2-1_%s.png", shop.id))
#     plot(decomposed)
#     dev.off()
#     
#     dly.sales <- dtn.first.return[first.or.return.cust == "return"]
#     min.order.date <- min(dly.sales$order.date)
#     max.order.date <- max(dly.sales$order.date)
#     min.year <- year(min.order.date)
#     min.yday <- yday(min.order.date)
#     x2 <- dly.sales$gross.sales
#     # xtimeseries <- ts(x,frequency = 7,start=c(2016,20))
#     xtimeseries2 <- ts(x,frequency = 365,start=c(min.year,min.yday))
#     # decomposed <- decompose(xtimeseries)
#     decomposed2 <- stl(xtimeseries2, s.window = "periodic")
#     slackme(sprintf("Shop %s done", shop.id), st.tm)
#     png(filename=sprintf("plots/ts_all/plot2-2_%s.png", shop.id))
#     plot(decomposed2)
#     dev.off()
# 
#     obj <- list(c(shop.id, as.character(min.order.date), as.character(max.order.date)), 
#                 x, decomposed, x2, decomposed2)
#     
#     dly.return <- dly.return.raw[ShopId==shop.id]
#     
#     ggplot(dly.return, aes(x = order.date, y = gross.sales, colour=freq.class.cust)) +
#         geom_line() + facet_grid(freq.class.cust ~ active.or.seal.cust)
#     ggsave(sprintf("plots/ts_all/plot4_%s.png", shop.id), width = 12, height = 10)
# 
#     slackme(sprintf("Shop %s done", shop.id), st.tm)
#     
#     obj
#     }, error=function(e) sprintf("%s error", shop.id)))
# }, mc.preschedule = FALSE, mc.cores = 4)
# 
# 
# system(sprintf("zip %s plots/%s/*", suffix, suffix))
# save(result.lss.first.return, file = file.path("data/intermediates/", sprintf("%s.result.lss.first.return.RData", suffix)))
# 
# default.bucket <- "helen-ml-4standard"
# Sys.setenv("GCS_AUTH_FILE" = "/home/helen/gcs.oauth",
#            "GCS_DEFAULT_BUCKET" = default.bucket)
# library(googleCloudStorageR)
# gcs_auth()
# gcs_global_bucket(default.bucket)
# # gcs_list_objects()
# gcs_upload(sprintf("%s.zip", suffix), name = sprintf("%s-1.zip", suffix))
# 
# file.copy("ts.decomposed.R", sprintf("scripts/ts.decomposed.%s.R", suffix), overwrite = T)
# slackme("ts_all done", st.tm)

################################################################################

lapply(result.lss.first.return, "[[", 1) %>% lapply(., "[", 1) %>% "==" ("360") %>% which()

x <- result.lss.first.return[[89]][2]
st.dt <- result.lss.first.return[[89]][[1]][2]
ed.dt <- result.lss.first.return[[89]][[1]][3]
data.frame(date=seq.Date(as.Date(st.dt), as.Date(ed.dt), "day"), y = x)

# TODO: v存圖
# v看其他家
# v試跑在gcp上
# vTime Series (window mean)
# 切 2016-06-30(?)
# videntify peaks (with/without test)

# x <- dtn.return.recency[active.or.seal.cust=="quater"&TrackSourceTypeDef=="Web"&TrackDeviceTypeDef=="Mobile"]$gross.sales
# x <- dtn.first.return[first.or.return.cust=="return"&TrackSourceTypeDef=="Web"&TrackDeviceTypeDef=="Mobile"]$gross.sales

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
ggsave(sprintf("plots/ts_all/plot4_%s.png", shop.id), width = 12, height = 10)
# }