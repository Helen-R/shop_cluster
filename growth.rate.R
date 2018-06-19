if (!"mylib" %in% ls()) source("../auxiliary/mylib.R")
if (!"slackme" %in% ls()) source("../auxiliary/slackme.R")
mylib(c("data.table", "magrittr", "dplyr", "googleCloudStorageR", "plyr", "ggplot2", 
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


suffix <- ""
if(!dir.exists(sprintf("plots/%s/", suffix))) dir.create(sprintf("plots/%s/", suffix))
if(!dir.exists(sprintf("data/processed/%s/", suffix))) dir.create(sprintf("data/processed/%s/", suffix))

dly.sales1 <- as_tbl_time(dly.sales.raw, index = order.date) 
print(sprintf("ts decomposing %s shops in total...", length(shop.ls)))
shop.id <- 2202
# shop.ls <- c(1773, 1772, 1769, 1764)
shop.ls <- unique(dly.sales1$ShopId)
result.lss <- mclapply(shop.ls, function (shop.id) {
    if (which(shop.id==shop.ls)%%100==1) 
        slackme(sprintf("%s (%s/%s) ts decompose start", 
                        shop.id, which(shop.id==shop.ls), 
                        length(shop.ls)), 
                st.tm)
    return(tryCatch({
        dly.sales <- filter(dly.sales1, ShopId == shop.id) %>% 
            select(-ShopId) %>%
            collapse_by("monthly") %>%
            group_by(order.date) %>%
            summarise_all(sum)
    dly.sales %<>% as_tbl_time(., index = order.date) %>% 
        time_decompose(gross.sales, method="stl", frequency = "6 months", trend = "6 month", message = T) %>% 
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


# monthly data points
shop.id <- 14
dly.sales <- filter(dly.sales1, ShopId == shop.id) %>% 
    select(-ShopId) %>%
    collapse_by("monthly") %>%
    group_by(order.date) %>%
    summarise_all(sum)
dly.sales %<>% as_tbl_time(., index = order.date) %>% 
    time_decompose(gross.sales, method="stl", frequency = "6 months", trend = "6 month", message = T) %>% 
    anomalize(remainder, metho="iqr") %>% 
    time_recompose() 
plot_anomaly_decomposition(dly.sales)
View(dly.sales)




dly.sales <- dly.sales1 %>%
    collapse_by(., "monthly") %>%
    group_by(order.date, ShopId) %>%
    summarise_all(sum)
setorder(dly.sales, ShopId, order.date)
dly.sales <- data.table(dly.sales)
dly.sales[, `:=` (nth.mth=1:.N,
                  n.mth=.N,
                  last.gross.sales=shift(gross.sales, 1L)), by = ShopId]
dly.sales[, growth.rate:= round(gross.sales/last.gross.sales, 1)]
dly.sales[, monthly.million:=ifelse(gross.sales>=1000000, 1, 0)]
dly.sales[, been.million:=ifelse(sum(monthly.million, na.rm = T)>0, 1, 0), by = ShopId]
# been.million <- dly.sales[, .(been.million=ifelse(sum(monthly.million)>0, 1, 0)), by = ShopId]
tmo1 <- dly.sales[monthly.million==1]
View(tmo1[, .(order.date=order.date[1], growth.rate=growth.rate[1],
              gross.sales=gross.sales[1], last.gross.sales=last.gross.sales[1],
              max.gross.sales=max(gross.sales), 
              n.mth=n.mth[1], nth.mth=nth.mth[1]), by = ShopId])
View(dly.sales[ShopId==14])
