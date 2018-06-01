if (!"mylib" %in% ls()) source("../auxiliary/mylib.R")
if (!"slackme" %in% ls()) source("../auxiliary/slackme.R")
mylib(c("data.table", "BTYD", "magrittr", "lubridate", "parallel", "googleCloudStorageR"))

# fixed
# http://erikjohansson.blogspot.tw/2015/12/problems-with-btyd-walk-through-fixed.html

shop.id <- 14
fnm <- sprintf("data/raw_input/dt.dly_%s.RData", shop.id)
load(fnm)
date.ls <- seq.Date(as.Date("2013-07-01"), as.Date("2016-04-30"), "month")
if (!dir.exists("plots/BTYD_14_monthly_series_cumulate/")) dir.create("plots/BTYD_14_monthly_series_cumulate/")

result.lss <- mclapply(date.ls, function(start.date) {
param.ls <- NULL
result.ls <- NULL
# for (shop.id in  c(360, 2131, 11, 14)) {

cat(sprintf("%s\n", as.character(start.date)))

per <- "week"
censor <- 7 # This censor serves the same purpose described above x.star <- cal.cbs[,"x.star"]
# mid.wks <- 52
# tot.wks <- mid.wks * 2
# slackme()
# data prep----
# elog: daily transactions per customer
# elog.test: elog for after calibration and holdout
# calibration.elog[first/repeat]
# cal.cbs: x (frequency) / t.x (recency) / T.x (total length)
# holdout.elog (for x.star calculation)

# start.date <- as.Date("2014-02-01")
end.date <- as.Date("2018-04-30")
# end.date <- start.date %m+% weeks(tot.wks)
elog <- dt.dly[, .(MemberId, order.date, gross.sales)]
setnames(elog, colnames(elog), c("cust", "date", "sales"))
elog.test <- elog[date > end.date]
elog <- elog[date <= end.date & date >= start.date]
setorder(elog, cust, date)
elog[1:3, ]
# ex:
# cust       date sales
# 1    1 1997-01-01 29.33
# 2    1 1997-01-18 29.73
# 3    1 1997-08-02 14.96

# cut.date <- start.date + (end.date - start.date) / 2
# cut.date <- start.date %m+% weeks(mid.wks)
cut.date <- as.Date("2018-04-30") %m-% weeks(52)

# training period
calibration.elog <- elog[date <= cut.date, ]
setorder(calibration.elog, cust, date)
calibration.elog[, `:=` (n=.N, nth=1:.N, nth.rev=.N:1), by = cust]
calibration.elog.first <- calibration.elog[nth == 1]
repeat.transactions.elog <- calibration.elog[nth > 1]
cust.data <- calibration.elog[, type:="other"][nth==1, type:="first"][nth.rev==1, type:="last"][type!="other", .(cust, date, type, sales)]
cust.data <- dcast.data.table(cust.data, cust ~ type, value.var = c("date", "sales"))
cust.data[is.na(date_first), `:=` (date_first=date_last,
                                   sales_first=sales_last)]
cust.data <- cust.data[, .(cust, date_first, sales_first, date_last, sales_last)]
setnames(cust.data, colnames(cust.data), c("cust", "birth.per", "first.sales", "last.date", "last.sales"))
freq.repeat <- repeat.transactions.elog[, .(x=.N), by = cust]
aggr <- switch(per, day = 1, week = 7, month = 365/12, 
               quarter = 365/4, year = 365)
cust.data[, `:=` (t.x = unclass((last.date-birth.per) / aggr),
                  T.cal = unclass((cut.date-birth.per) / aggr))]
cal.cbs <- cust.data[, .(cust, t.x, T.cal)]
cal.cbs <- freq.repeat[cal.cbs, on = "cust"]
cal.cbs[is.na(x), x:=0]

# holdout period----
holdout.elog <- elog[date > cut.date, ]
setorder(holdout.elog, cust, date)
dates <- c(date.begin.holdout.period = (cut.date + 1), 
           date.end.holdout.period = end.date)
T.star <- as.vector(round(((dates["date.end.holdout.period"] - dates["date.begin.holdout.period"]) + 1) / aggr))
hld.cbs <- holdout.elog[, .(x.star=.N), by = cust]
cal.cbs[hld.cbs, x.star:=x.star, on = "cust"]
cal.cbs[is.na(x.star), x.star := 0]
cal.cbs

cbs <- as.data.frame(cal.cbs[, .(x, t.x, T.cal)])
x.star <- cal.cbs[, .(x.star)] %>% unlist()

elog[, `:=` (n=.N, nth=1:.N, nth.rev=.N:1), by = cust]
elog.valid <- elog[cust %in% cal.cbs$cust]
elog.first <- elog.valid[nth == 1]
elog.repeat <- elog.valid[nth > 1]

origin <- start.date
elog.repeat[, date.index := ((as.integer(date-min(date)) + 1) %/% aggr + 1)]
freq.dly <- elog.repeat[, .(f=.N), by = date.index]
setorder(freq.dly, date.index)
T.cal <- cbs[,"T.cal"] %>% as.vector()
T.tot <- ceiling((end.date - start.date) / aggr) %>% as.vector()
n.periods.final <- T.tot
w.track.data <- freq.dly$f
cum.tracking.data <- cumsum(freq.dly$f)

cal.cbs.spend <- repeat.transactions.elog[, .(m=sum(sales)), by = cust]
cal.cbs.spend <- cal.cbs.spend[cal.cbs, on = "cust"]
cal.cbs.spend[, m.x:=m/x]
ave.spend <- cal.cbs.spend[which(x > 0)]$m.x
tot.trans <- cal.cbs.spend[which(x > 0)]$x


slackme(sprintf("%s %s data prep done", shop.id, as.character(start.date)), st.tm)


# BG/NBD----
params <- bgnbd.EstimateParameters(cal.cbs)
# slackme(sprintf("bgnbd done", shop.id), st.tm)

t52 <- bgnbd.Expectation(params, t=52)

LL <- bgnbd.cbs.LL(params, cal.cbs)
p.matrix <- c(shop.id, "bgnbd", params, LL, t52)
# for (i in 1:2){
#     params <- bgnbd.EstimateParameters(cal.cbs, params)
#     LL <- bgnbd.cbs.LL(params, cal.cbs)
#     p.matrix.row <- c(params, LL)
#     p.matrix <- rbind(p.matrix, p.matrix.row)
# }
# colnames(p.matrix) <- c("r", "alpha", "s", "beta", "LL")
# rownames(p.matrix) <- 1:3
# names(p.matrix) <- c("r", "alpha", "a", "b", "LL")
# p.matrix
param.ls <- rbind(p.matrix, param.ls)
# idx <- 10778
# x <- cal.cbs[cust==idx, "x"] %>% unlist()
# t.x <- cal.cbs[cust==idx, "t.x"] %>% unlist()
# T.cal <- cal.cbs[idx, "T.cal"] %>% unlist()
# bgnbd.ConditionalExpectedTransactions(params, T.star = T.star, x, t.x, T.cal)
# [1] 1.676457
# bgnbd.PAlive(params, x, t.x, T.cal)
# [1] 0.8163679

# for (i in seq(10, 25, 5)){
#     cond.expectation <- bgnbd.ConditionalExpectedTransactions(
#         params, T.star = 52, x = i,
#         t.x = 20, T.cal = 39)
#     cat ("x:",i,"\t Expectation:",cond.expectation, fill = TRUE) }
png(sprintf("plots/BTYD_14_monthly_series_cumulate/plot7_%s_%s.png", shop.id, as.character(start.date)))
bgnbd.PlotFrequencyInCalibration(params, cal.cbs, censor)
dev.off()
png(sprintf("plots/BTYD_14_monthly_series_cumulate/plot8_%s_%s.png", shop.id, as.character(start.date)))
bgnbd.PlotTransactionRateHeterogeneity(params)
dev.off()
png(sprintf("plots/BTYD_14_monthly_series_cumulate/plot9_%s_%s.png", shop.id, as.character(start.date)))
bgnbd.PlotDropoutRateHeterogeneity(params)
dev.off()

png(sprintf("plots/BTYD_14_monthly_series_cumulate/plot60_%s_%s.png", shop.id, as.character(start.date)))
comp <- bgnbd.PlotFreqVsConditionalExpectedFrequency(params, T.star,
                                                     cbs, x.star, censor)
dev.off()

rownames(comp) <- c("act", "exp", "bin")
result.ls <- c(result.ls, list(comp))

png(sprintf("plots/BTYD_14_monthly_series_cumulate/plot5_%s_%s.png", shop.id, as.character(start.date)))
inc.tracking <- bgnbd.PlotTrackingInc(params, T.cal,
                                      T.tot, w.track.data,
                                      n.periods.final)
dev.off()
# actual <- w.track.data
# expected <- dc.CumulativeToIncremental(bgnbd.ExpectedCumulativeTransactions(params, 
#                                                                             T.cal, T.tot, length(actual)))
# xlab <- "Week"
# ylab <- "Transactions"
# xticklab <- NULL
# title <- "Tracking Weekly Transactions"
# ylim <- c(0, max(c(actual, expected)) * 1.05)
# plot(actual, type = "l", xaxt = "n", xlab = xlab, ylab = ylab, 
#      col = 1, ylim = ylim, main = title)
# lines(expected, lty = 2, col = 2)
# if (is.null(xticklab) == FALSE) {
#     if (length(actual) != length(xticklab)) {
#         stop("Plot error, xticklab does not have the correct size")
#     }
#     axis(1, at = 1:length(actual), labels = xticklab)
# } else {
#     axis(1, at = 1:length(actual), labels = 1:length(actual))
# }
# abline(v = max(T.cal), lty = 2)
# legend("topleft", legend = c("Actual", "Model"), col = 1:2, 
#        lty = 1:2, lwd = 1, bty = "n")

png(sprintf("plots/BTYD_14_monthly_series_cumulate/plot6_%s_%s.png", shop.id, as.character(start.date)))
cum.tracking <- bgnbd.PlotTrackingCum(params, T.cal,
                                      T.tot, cum.tracking.data,
                                      n.periods.final)
dev.off()

cum.tracking[,20:25]

# slackme("plot done", st.tm)






# # Pareto/NBD----
# params <- pnbd.EstimateParameters(cal.cbs)
# slackme("pnbd.EstimateParameters test done", st.tm)
# # [1]  0.5534 10.5802  0.6061 11.6562
# LL <- pnbd.cbs.LL(params, cal.cbs)
# t52 <- pnbd.Expectation(params, t=52)
# p.matrix <- c(shop.id, "pnbd", params, LL, t52)
# param.ls <- rbind(p.matrix, param.ls)
# # for (i in 1:2){
# #     params <- pnbd.EstimateParameters(cal.cbs, params)
# #     LL <- pnbd.cbs.LL(params, cal.cbs)
# #     p.matrix.row <- c(params, LL)
# #     p.matrix <- rbind(p.matrix, p.matrix.row)
# # }
# # colnames(p.matrix) <- c("r", "alpha", "s", "beta", "LL")
# # rownames(p.matrix) <- 1:3
# # names(p.matrix) <- c("r", "alpha", "s", "beta", "LL")
# # p.matrix
# # idx <- 10778
# # x <- cal.cbs[cust==idx, "x"] %>% unlist()
# # t.x <- cal.cbs[cust==idx, "t.x"] %>% unlist()
# # T.cal <- cal.cbs[idx, "T.cal"] %>% unlist()
# # pnbd.ConditionalExpectedTransactions(params, T.star = 52, x, t.x, T.cal)
# # # [1] 25.46
# # pnbd.PAlive(params, x, t.x, T.cal)
# # # [1] 0.9979
# 
# # for (i in seq(10, 25, 5)){
# #     cond.expectation <- pnbd.ConditionalExpectedTransactions(
# #         params, T.star = 52, x = i,
# #         t.x = 20, T.cal = 39)
# #     cat ("x:",i,"\t Expectation:",cond.expectation, fill = TRUE) }
# png(sprintf("plots/BTYD_14_monthly_series_cumulate/plot7-1_%s_%s.png", shop.id, as.character(start.date)))
# pnbd.PlotFrequencyInCalibration(params, cbs, censor)
# dev.off()
# png(sprintf("plots/BTYD_14_monthly_series_cumulate/plot8-1_%s_%s.png", shop.id, as.character(start.date)))
# pnbd.PlotTransactionRateHeterogeneity(params)
# dev.off()
# png(sprintf("plots/BTYD_14_monthly_series_cumulate/plot9-1_%s_%s.png", shop.id, as.character(start.date)))
# pnbd.PlotDropoutRateHeterogeneity(params)
# dev.off()
# 
# # elog <- dc.SplitUpElogForRepeatTrans(elog)$repeat.trans.elog
# # x.star <- rep(0, nrow(cal.cbs))
# # cal.cbs <- cbind(cal.cbs, x.star)
# # elog.custs <- elog$cust
# # for (i in 1:nrow(cal.cbs)){ 
# #     current.cust <- rownames(cal.cbs)[i]
# #     tot.cust.trans <- length(which(elog.custs == current.cust))
# #     cal.trans <- cal.cbs[i, "x"]
# #     cal.cbs[i, "x.star"] <-  tot.cust.trans - cal.trans
# # }
# 
# png(sprintf("plots/BTYD_14_monthly_series_cumulate/plot60-1_%s_%s.png", shop.id, as.character(start.date)))
# comp <- pnbd.PlotFreqVsConditionalExpectedFrequency(params, T.star,
#                                                     cbs, x.star, censor)
# dev.off()
# 
# rownames(comp) <- c("act", "exp", "bin")
# result.ls <- c(result.ls, list(comp))
# 
# png(sprintf("plots/BTYD_14_monthly_series_cumulate/plot5-1_%s_%s.png", shop.id, as.character(start.date)))
# inc.tracking <- pnbd.PlotTrackingInc(params, T.cal,
#                                      T.tot, w.track.data,
#                                      n.periods.final)
# dev.off()
# png(sprintf("plots/BTYD_14_monthly_series_cumulate/plot6-1_%s_%s.png", shop.id, as.character(start.date)))
# cum.tracking <- pnbd.PlotTrackingCum(params, T.cal,
#                                      T.tot, cum.tracking.data,
#                                      n.periods.final)
# dev.off()
# 
# # }
# 
# # load("data/intermediates/BTYD.result.RData")
# # params <- btyd[[1]][10,3:6] %>% as.numeric()
# 
# 
# 
# # gamma gamma spend model
# # input: a vector with each customer’s average observed transaction value in the calibration period.
# # (Q: include first purchase? No. (ref: https://stats.stackexchange.com/questions/303891/why-isnt-the-monetary-value-of-the-first-transaction-used-in-the-gamma-gamma-sp))
# # We will let the spend function use default starting parameters
# params <- spend.EstimateParameters(ave.spend, tot.trans)
# # q (shape), q (shape) and gamma (scale) of scale parameter
# # p, q, gamma
# # ex. 6.25, 3.74, 15.44
# p.matrix <- c(shop.id, "gg.spend", params, NA, NA)
# param.ls <- rbind(p.matrix, param.ls)
# 
# # Plot the actual and expected average transaction value across customers.
# png(sprintf("plots/BTYD_14_monthly_series_cumulate/plot61_%s_%s.png", shop.id, as.character(start.date)))
# f.m.x <- spend.plot.average.transaction.value(params, ave.spend, tot.trans)
# dev.off()

# btyd <- list(param.ls, result.ls)
# save(btyd, file = "data/intermediates/BTYD.test02.result.RData")
obj <- list(param.ls, result.ls, inc.tracking, cum.tracking)
return(obj)
}, mc.cores = 4)
# tmp <- cal.cbs.spend
# tmp[, x1:=x]
# tmp[x > 7, x1:=7]
# tmp1 <- tmp[, .(N=.N, x=sum(x), x.star=sum(x.star), avg.x=sum(x.star)/.N), by = x1]
# setorder(tmp1, x)
# plot(tmp1)

system("zip BTYD_14_monthly_series_cumulate plots/BTYD_14_monthly_series_cumulate/*")


default.bucket <- "helen-ml-4standard"
Sys.setenv("GCS_AUTH_FILE" = "/home/helen/gcs.oauth",
           "GCS_DEFAULT_BUCKET" = default.bucket)
library(googleCloudStorageR)
gcs_auth()
gcs_global_bucket(default.bucket)
# gcs_list_objects()
gcs_upload("BTYD_14_monthly_series_cumulate.zip")

file.copy("BTYD.monthly.series.R", "scripts/BTYD_14_monthly_series_cumulate.R")
slackme("done", st.tm)
