if (!"mylib" %in% ls()) source("../auxiliary/mylib.R")
if (!"slackme" %in% ls()) source("../auxiliary/slackme.R")
mylib(c("data.table", "magrittr", "googleCloudStorageR", "plyr", "ggplot2", 
        "anomalize", "purrr", "tibbletime", "parallel"))

shop.id <- "all"
fnm <- sprintf("data/intermediates/dt.dly_%s.RData", shop.id)   # dt.dly
load(fnm)
setnames(dt.dly, "TotalPayment", "gross.sales")

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
dly.sales.raw <- dly.sales[date.seq, on = c("ShopId", "order.date")][
    is.na(gross.sales), gross.sales:=0][
        is.na(promo.discnt), promo.discnt:=0][
            is.na(ecoup.discnt), ecoup.discnt:=0]
setorder(dly.sales.raw, order.date)

dt.dly.stats <- dt.dly[, .(st.dt = min(order.date), 
                           ed.dt = max(order.date),
                           duration = as.integer(max(order.date)-min(order.date))), 
                       by = ShopId]

shop.ls <- dt.dly.stats[duration > 365*2]$ShopId
slackme("ts data prep done", st.tm)

# shop.id <- 14
# dly.sales <- dly.sales.raw[ShopId == shop.id] 
# # dly.sales %>% filter_time("2018-02" ~ "2018-03") %>% View()
# dly.sales %<>% as_tbl_time(., index = order.date) %>% 
#     filter_time("2017-12" ~ "2018-03") %>% 
#     time_decompose(gross.sales, method="stl", frequency = "auto", trend = "1 month") %>% 
#     anomalize(remainder, metho="iqr") %>% 
#     time_recompose() 
# plot_anomaly_decomposition(dly.sales)
# plot_anomalies(dly.sales, time_recomposed = TRUE)
# dly.sales %>% View()
# 
# dly.sales <- dly.sales.raw[ShopId == shop.id] 
# dly.sales %<>%  
#     as_tbl_time(., index = order.date) %>% 
#     time_decompose(gross.sales, method="stl", frequency = "6 month", trend = "12 months") %>% 
#     anomalize(remainder, metho="iqr") %>% 
#     time_recompose() 
# View(dly.sales)
# plot_anomaly_decomposition(dly.sales)
# plot_anomalies(dly.sales, time_recomposed = TRUE)







# daily sales----
dly.sales.all.raw <- dt.dly[, .(gross.sales=sum(gross.sales),
                                promo.discnt=sum(PromotionDiscount),
                                ecoup.discnt=sum(ECouponDiscount)), by = .(order.date)]
## fill missing dates with 0
setorder(dly.sales.all.raw, order.date)
min.dt <- min(dly.sales.all.raw$order.date)
max.dt <- max(dly.sales.all.raw$order.date)
duration <- as.integer(max.dt - min.dt) + 1
if (duration!=nrow(dly.sales.all.raw)) {
    print(sprintf("duration vs. nrow: %s / %s", duration, nrow(dly.sales.all.raw)))
    date.seq <- data.table(order.date=seq.Date(min.dt, max.dt, "day"))
    dly.sales.all.raw <- dly.sales.all.raw[date.seq, on = "order.date"][is.na(gross.sales), gross.sales:=0][is.na(promo.discnt), promo.discnt:=0][is.na(ecoup.discnt), ecoup.discnt:=0]
}
stopifnot(duration==nrow(dly.sales.all.raw))
setorder(dly.sales.all.raw, order.date)
dt.dly.stats <- dly.sales.all.raw[, .(st.dt = min(order.date), 
                                      ed.dt = max(order.date),
                                      duration = as.integer(max(order.date)-min(order.date))+1)]
# dly.sales.all.raw %>% filter_time("2018-02" ~ "2018-03") %>% View()
dly.sales.all <- as_tbl_time(dly.sales.all.raw, index = order.date) %>% 
    filter_time("2017-12" ~ "2018-03") %>% 
    time_decompose(gross.sales, method="stl", frequency = "auto", trend = "1 month") %>% 
    anomalize(remainder, metho="iqr") %>% 
    time_recompose() 
plot_anomaly_decomposition(dly.sales.all)
ggsave("plots/ts.decomposed.shop.all.png", width=6, height=6)
plot_anomalies(dly.sales.all, time_recomposed = TRUE)
ggsave("plots/ts.anomalies.shop.all.png")
dly.sales.all %>% View()
save(dly.sales.all, file="data/processed/dly.sales.all.RData")
save(dly.sales.all.raw, file="data/processed/dly.sales.all.raw.RData")


suffix <- "ts_anomaly_short_term_2018-02-28_1month"
if(!dir.exists(sprintf("plots/%s/", suffix))) dir.create(sprintf("plots/%s/", suffix))
if(!dir.exists(sprintf("data/processed/%s/", suffix))) dir.create(sprintf("data/processed/%s/", suffix))

dly.sales1 <- as_tbl_time(dly.sales.raw, index = order.date) %>% 
    filter_time("2017-12" ~ "2018-03")
shop.ls <- unique(dly.sales1$ShopId)
print(sprintf("ts decomposing %s shops in total...", length(shop.ls)))
# shop.id <- 14
# shop.ls <- c(1773, 1772, 1769, 1764)
slackme("start", st.tm)
result.lss <- mclapply(shop.ls, function (shop.id) {
    if (which(shop.id==shop.ls)%%100==1) 
        slackme(sprintf("%s (%s/%s) ts decompose start", 
                        shop.id, which(shop.id==shop.ls), 
                        length(shop.ls)), 
                st.tm)
    return(tryCatch({dly.sales <- filter(dly.sales1, ShopId == shop.id)
    dly.sales %<>% as_tbl_time(., index = order.date) %>% 
        time_decompose(gross.sales, method="stl", frequency = "auto", trend = "1 month") %>% 
        anomalize(remainder, metho="iqr") %>% 
        time_recompose() 
    plot_anomaly_decomposition(dly.sales)
    ggsave(sprintf("plots/%s/ts.decomposed.shop.%s.png", suffix, shop.id), width=6, height=6)
    plot_anomalies(dly.sales, time_recomposed = TRUE)
    ggsave(sprintf("plots/%s/ts.anomalies.shop.%s.png", suffix, shop.id), width=6, height=4)
    dly.sales$ShopId <- shop.id
    save(dly.sales, file=sprintf("data/processed/%s/dly.sales.shop%s.RData", suffix, shop.id))
    sprintf("%s done", shop.id)
    }, error=function(e) sprintf("%s error (%s)", shop.id, nrow(dly.sales))))
}, mc.preschedule = FALSE, mc.cores = 4)
slackme(sprintf("ts decompsed with %s shops done", length(shop.ls)), st.tm)

system(sprintf("zip %s plots/%s/*", suffix, suffix))

default.bucket <- "helen-ml-4standard"
Sys.setenv("GCS_AUTH_FILE" = "/home/helen/gcs.oauth",
           "GCS_DEFAULT_BUCKET" = default.bucket)
library(googleCloudStorageR)
gcs_auth()
gcs_global_bucket(default.bucket)
# gcs_list_objects()
gcs_upload(sprintf("%s.zip", suffix), name = sprintf("%s.zip", suffix))

# file.copy("ts.decomposed.anomaly.short.term.R", sprintf("scripts/%s.R", suffix), overwrite = T)
# save(result.lss.first.return, file = file.path("data/intermediates/", sprintf("%s.result.lss.first.return.RData", suffix)))
slackme("ts decomposed done", st.tm)

# assemble ts components per shop
fls <- list.files(sprintf("data/processed/%s", suffix), ".RData", full.names = TRUE)
dtb <- NULL
for (f in fls) {
    load(f)
    dtb <- rbind(dtb, dly.sales)
}
dt <- data.table(dtb)
dt <- dt[order.date==as.Date("2018-02-28") & anomaly == "Yes"]
setorder(dt, -observed)
dt[, diff:=observed - trend]
dt1 <- dt[, .(ShopId, remainder, observed, trend, diff)]
dt1[, `:=` (cum.diff.percentage=round(cumsum(remainder)*100/sum(diff), 1),
            percentage=round(remainder*100/sum(remainder), 1),
            percentage2=round(remainder*100/observed, 1))]
View(dt1)
plot.ecdf(dt1$diff)
dt1[, .(sum(remainder), sum(diff))]
dt2 <- filter(dly.sales.all, order.date==as.Date("2018-02-28")) %>% data.table
dt2[, .(observed-trend)]

# discount analysis (all)
# promotion discount
dly.sales.all <- as_tbl_time(dly.sales.all.raw, index = order.date) %>% 
    filter_time("2017-12" ~ "2018-03") %>% 
    time_decompose(promo.discnt, method="stl", frequency = "auto", trend = "1 month") %>% 
    anomalize(remainder, metho="iqr") %>% 
    time_recompose() 
plot_anomaly_decomposition(dly.sales.all)
ggsave("plots/ts.decomposed.shop.all_promo.discnt.png", width=6, height=6)
plot_anomalies(dly.sales.all, time_recomposed = TRUE)
ggsave("plots/ts.anomalies.shop.all_promo.discnt.png")
# ecoupon discount
dly.sales.all <- as_tbl_time(dly.sales.all.raw, index = order.date) %>% 
    filter_time("2017-12" ~ "2018-03") %>% 
    time_decompose(ecoup.discnt, method="stl", frequency = "auto", trend = "1 month") %>% 
    anomalize(remainder, metho="iqr") %>% 
    time_recompose() 
plot_anomaly_decomposition(dly.sales.all)
ggsave("plots/ts.decomposed.shop.all_ecoup.discnt.png", width=6, height=6)
plot_anomalies(dly.sales.all, time_recomposed = TRUE)
ggsave("plots/ts.anomalies.shop.all_ecoup.discnt.png")

# shop with anomaly (1 month)
# ecoupon discount
suffix <- "ts_anomaly_short_term_2018-02-28_1month_ecoup"
if(!dir.exists(sprintf("plots/%s/", suffix))) dir.create(sprintf("plots/%s/", suffix))
if(!dir.exists(sprintf("data/processed/%s/", suffix))) dir.create(sprintf("data/processed/%s/", suffix))
shop.ls <- dt$ShopId
slackme("start", st.tm)
result.lss <- mclapply(shop.ls, function (shop.id) {
    if (which(shop.id==shop.ls)%%100==1) 
        slackme(sprintf("%s (%s/%s) ts decompose start", 
                        shop.id, which(shop.id==shop.ls), 
                        length(shop.ls)), 
                st.tm)
    return(tryCatch({dly.sales <- filter(dly.sales1, ShopId == shop.id)
    dly.sales %<>% as_tbl_time(., index = order.date) %>% 
        time_decompose(ecoup.discnt, method="stl", frequency = "auto", trend = "1 month") %>% 
        anomalize(remainder, metho="iqr") %>% 
        time_recompose() 
    plot_anomaly_decomposition(dly.sales)
    ggsave(sprintf("plots/%s/ts.decomposed.shop.%s.png", suffix, shop.id), width=6, height=6)
    plot_anomalies(dly.sales, time_recomposed = TRUE)
    ggsave(sprintf("plots/%s/ts.anomalies.shop.%s.png", suffix, shop.id), width=6, height=4)
    dly.sales$ShopId <- shop.id
    save(dly.sales, file=sprintf("data/processed/%s/dly.sales.shop%s.RData", suffix, shop.id))
    sprintf("%s done", shop.id)
    }, error=function(e) sprintf("%s error (%s)", shop.id, nrow(dly.sales))))
}, mc.preschedule = FALSE, mc.cores = 4)
slackme(sprintf("ts decompsed with %s shops done", length(shop.ls)), st.tm)
# promotion discount
suffix <- "ts_anomaly_short_term_2018-02-28_1month_promo"
if(!dir.exists(sprintf("plots/%s/", suffix))) dir.create(sprintf("plots/%s/", suffix))
if(!dir.exists(sprintf("data/processed/%s/", suffix))) dir.create(sprintf("data/processed/%s/", suffix))
shop.ls <- dt$ShopId
slackme("start", st.tm)
result.lss <- mclapply(shop.ls, function (shop.id) {
    if (which(shop.id==shop.ls)%%100==1) 
        slackme(sprintf("%s (%s/%s) ts decompose start", 
                        shop.id, which(shop.id==shop.ls), 
                        length(shop.ls)), 
                st.tm)
    return(tryCatch({dly.sales <- filter(dly.sales1, ShopId == shop.id)
    dly.sales %<>% as_tbl_time(., index = order.date) %>% 
        time_decompose(promo.discnt, method="stl", frequency = "auto", trend = "1 month") %>% 
        anomalize(remainder, metho="iqr") %>% 
        time_recompose() 
    plot_anomaly_decomposition(dly.sales)
    ggsave(sprintf("plots/%s/ts.decomposed.shop.%s.png", suffix, shop.id), width=6, height=6)
    plot_anomalies(dly.sales, time_recomposed = TRUE)
    ggsave(sprintf("plots/%s/ts.anomalies.shop.%s.png", suffix, shop.id), width=6, height=4)
    dly.sales$ShopId <- shop.id
    save(dly.sales, file=sprintf("data/processed/%s/dly.sales.shop%s.RData", suffix, shop.id))
    sprintf("%s done", shop.id)
    }, error=function(e) sprintf("%s error (%s)", shop.id, nrow(dly.sales))))
}, mc.preschedule = FALSE, mc.cores = 4)
slackme(sprintf("ts decompsed with %s shops done", length(shop.ls)), st.tm)
# shop with anomaly (7 days)
# ecoupon discount
suffix <- "ts_anomaly_short_term_2018-02-28_7days_ecoup"
if(!dir.exists(sprintf("plots/%s/", suffix))) dir.create(sprintf("plots/%s/", suffix))
if(!dir.exists(sprintf("data/processed/%s/", suffix))) dir.create(sprintf("data/processed/%s/", suffix))
shop.ls <- dt$ShopId
slackme("start", st.tm)
result.lss <- mclapply(shop.ls, function (shop.id) {
    if (which(shop.id==shop.ls)%%100==1) 
        slackme(sprintf("%s (%s/%s) ts decompose start", 
                        shop.id, which(shop.id==shop.ls), 
                        length(shop.ls)), 
                st.tm)
    return(tryCatch({dly.sales <- filter(dly.sales1, ShopId == shop.id)
    dly.sales %<>% as_tbl_time(., index = order.date) %>% 
        time_decompose(ecoup.discnt, method="stl", frequency = "auto", trend = "7 days") %>% 
        anomalize(remainder, metho="iqr") %>% 
        time_recompose() 
    plot_anomaly_decomposition(dly.sales)
    ggsave(sprintf("plots/%s/ts.decomposed.shop.%s.png", suffix, shop.id), width=6, height=6)
    plot_anomalies(dly.sales, time_recomposed = TRUE)
    ggsave(sprintf("plots/%s/ts.anomalies.shop.%s.png", suffix, shop.id), width=6, height=4)
    dly.sales$ShopId <- shop.id
    save(dly.sales, file=sprintf("data/processed/%s/dly.sales.shop%s.RData", suffix, shop.id))
    sprintf("%s done", shop.id)
    }, error=function(e) sprintf("%s error (%s)", shop.id, nrow(dly.sales))))
}, mc.preschedule = FALSE, mc.cores = 4)
slackme(sprintf("ts decompsed with %s shops done", length(shop.ls)), st.tm)
# promotion discount
suffix <- "ts_anomaly_short_term_2018-02-28_7days_promo"
if(!dir.exists(sprintf("plots/%s/", suffix))) dir.create(sprintf("plots/%s/", suffix))
if(!dir.exists(sprintf("data/processed/%s/", suffix))) dir.create(sprintf("data/processed/%s/", suffix))
shop.ls <- dt$ShopId
slackme("start", st.tm)
result.lss <- mclapply(shop.ls, function (shop.id) {
    if (which(shop.id==shop.ls)%%100==1) 
        slackme(sprintf("%s (%s/%s) ts decompose start", 
                        shop.id, which(shop.id==shop.ls), 
                        length(shop.ls)), 
                st.tm)
    return(tryCatch({dly.sales <- filter(dly.sales1, ShopId == shop.id)
    dly.sales %<>% as_tbl_time(., index = order.date) %>% 
        time_decompose(promo.discnt, method="stl", frequency = "auto", trend = "7 days") %>% 
        anomalize(remainder, metho="iqr") %>% 
        time_recompose() 
    plot_anomaly_decomposition(dly.sales)
    ggsave(sprintf("plots/%s/ts.decomposed.shop.%s.png", suffix, shop.id), width=6, height=6)
    plot_anomalies(dly.sales, time_recomposed = TRUE)
    ggsave(sprintf("plots/%s/ts.anomalies.shop.%s.png", suffix, shop.id), width=6, height=4)
    dly.sales$ShopId <- shop.id
    save(dly.sales, file=sprintf("data/processed/%s/dly.sales.shop%s.RData", suffix, shop.id))
    sprintf("%s done", shop.id)
    }, error=function(e) sprintf("%s error (%s)", shop.id, nrow(dly.sales))))
}, mc.preschedule = FALSE, mc.cores = 4)
slackme(sprintf("ts decompsed with %s shops done", length(shop.ls)), st.tm)

file.copy("ts.decomposed.anomaly.short.term.R", sprintf("scripts/%s.R", ".discount"), overwrite = T)


# error checking (ans: data points less than 21 days)
shop.ls <- unlist(result.lss)[grep("error", unlist(result.lss))] %>% 
    gsub(" error", "", .) %>% as.integer()
slackme("start", st.tm)
result.lss <- mclapply(shop.ls, function (shop.id) {
    # slackme(sprintf("%s ts decompose start", shop.id), st.tm)
    return(tryCatch({dly.sales <- filter(dly.sales1, ShopId == shop.id)
    dly.sales %<>% as_tbl_time(., index = order.date) %>% 
        time_decompose(gross.sales, method="stl", frequency = "auto", trend = "7 days") %>% 
        anomalize(remainder, metho="iqr") %>% 
        time_recompose() 
    plot_anomaly_decomposition(dly.sales)
    ggsave(sprintf("plots/%s/ts.decomposed.shop.%s.png", suffix, shop.id), width=6, height=6)
    plot_anomalies(dly.sales, time_recomposed = TRUE)
    ggsave(sprintf("plots/%s/ts.anomalies.shop.%s.png", suffix, shop.id), width=6, height=4)
    dly.sales$ShopId <- shop.id
    save(dly.sales, file=sprintf("data/processed/dly.sales.shop%s.Rdata", shop.id))
    sprintf("%s done", shop.id)
    }, error=function(e) sprintf("%s error (%s)", shop.id, nrow(dly.sales))))
}, mc.preschedule = FALSE, mc.cores = 4)
slackme(sprintf("ts decompsed with %s shops done", length(shop.ls)), st.tm)
