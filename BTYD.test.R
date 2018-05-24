if (!"mylib" %in% ls()) source("../auxiliary/mylib.R")
if (!"slackme" %in% ls()) source("../auxiliary/slackme.R")
mylib(c("data.table", "BTYD", "magrittr", "lubridate"))

# fixed
# http://erikjohansson.blogspot.tw/2015/12/problems-with-btyd-walk-through-fixed.html

shop.id <- 11
fnm <- sprintf("data/intermediates/dt.dly_%s.RData", shop.id)
load(fnm)

per <- "week"
censor <- 7 # This censor serves the same purpose described above x.star <- cal.cbs[,"x.star"]

# data prep----
start.date <- as.Date("2016-07-01")
end.date <- as.Date("2017-12-31")
elog <- dt.dly[, .(MemberId, order.date, gross.sales)]
setnames(elog, colnames(elog), c("cust", "date", "sales"))
elog.test <- elog[date > end.date]
elog <- elog[date <= end.date & date >= start.date]
setorder(elog, cust, date)
elog[1:3, ]
# cust       date sales
# 1    1 1997-01-01 29.33
# 2    1 1997-01-18 29.73
# 3    1 1997-08-02 14.96

cut.date <- start.date + (end.date - start.date) / 2

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
f <- repeat.transactions.elog[, .(x=.N), by = cust]
aggr <- switch(per, day = 1, week = 7, month = 365/12, 
               quarter = 365/4, year = 365)
cust.data[, `:=` (t.x = unclass((last.date-birth.per) / aggr),
                  T.cal = unclass((cut.date-birth.per) / aggr))]
cal.cbs <- cust.data[, .(cust, t.x, T.cal)]
cal.cbs <- f[cal.cbs, on = "cust"]
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
hcbs <- as.data.frame(cal.cbs[, .(x, t.x, T.cal, x.star)])

elog[, `:=` (n=.N, nth=1:.N, nth.rev=.N:1), by = cust]
elog.first <- elog[nth == 1]
elog.repeat <- elog[nth > 1]

slackme("done", st.tm)

origin <- start.date
elog.repeat[, date.index := ((as.integer(date-min(date)) + 1) %/% aggr + 1)]
freq.dly <- elog.repeat[, .(f=.N), by = date.index]
setorder(freq.dly, date.index)
T.cal <- cbs[,"T.cal"] %>% as.vector()
T.tot <- ceiling((end.date - start.date) / aggr) %>% as.vector()
n.periods.final <- T.tot
w.track.data <- freq.dly$f
cum.tracking.data <- cumsum(freq.dly$f)

slackme(sprintf("%s data prep done", shop.id), st.tm)





# Pareto/NBD----
slackme()
params <- pnbd.EstimateParameters(cal.cbs);
params
slackme("pnbd.EstimateParameters test done", st.tm)
# [1]  0.5534 10.5802  0.6061 11.6562
LL <- pnbd.cbs.LL(params, cal.cbs);
LL
p.matrix <- c(params, LL);
# for (i in 1:2){
#     params <- pnbd.EstimateParameters(cal.cbs, params);
#     LL <- pnbd.cbs.LL(params, cal.cbs);
#     p.matrix.row <- c(params, LL);
#     p.matrix <- rbind(p.matrix, p.matrix.row);
# }
# colnames(p.matrix) <- c("r", "alpha", "s", "beta", "LL")
# rownames(p.matrix) <- 1:3
names(p.matrix) <- c("r", "alpha", "s", "beta", "LL")
p.matrix
pnbd.Expectation(params, t=52);
idx <- 10778
x <- cal.cbs[cust==idx, "x"] %>% unlist()
t.x <- cal.cbs[cust==idx, "t.x"] %>% unlist()
T.cal <- cal.cbs[idx, "T.cal"] %>% unlist()
pnbd.ConditionalExpectedTransactions(params, T.star = 52, x, t.x, T.cal)
# [1] 25.46
pnbd.PAlive(params, x, t.x, T.cal)
# [1] 0.9979

# for (i in seq(10, 25, 5)){
#     cond.expectation <- pnbd.ConditionalExpectedTransactions(
#         params, T.star = 52, x = i,
#         t.x = 20, T.cal = 39)
#     cat ("x:",i,"\t Expectation:",cond.expectation, fill = TRUE) }

pnbd.PlotFrequencyInCalibration(params, cal.cbs, censor)
pnbd.PlotTransactionRateHeterogeneity(params)
pnbd.PlotDropoutRateHeterogeneity(params)

# elog <- dc.SplitUpElogForRepeatTrans(elog)$repeat.trans.elog;
# x.star <- rep(0, nrow(cal.cbs));
# cal.cbs <- cbind(cal.cbs, x.star);
# elog.custs <- elog$cust;
# for (i in 1:nrow(cal.cbs)){ 
#     current.cust <- rownames(cal.cbs)[i]
#     tot.cust.trans <- length(which(elog.custs == current.cust))
#     cal.trans <- cal.cbs[i, "x"]
#     cal.cbs[i, "x.star"] <-  tot.cust.trans - cal.trans
# }

comp <- pnbd.PlotFreqVsConditionalExpectedFrequency(params, T.star,
                                                    hcbs, x.star, censor)

rownames(comp) <- c("act", "exp", "bin")
comp

inc.tracking <- pnbd.PlotTrackingInc(params, T.cal,
                                     T.tot, w.track.data,
                                     n.periods.final)
cum.tracking <- pnbd.PlotTrackingCum(params, T.cal,
                                     T.tot, cum.tracking.data,
                                     n.periods.final)



# BG/NBD----
slackme(sprintf("%s bgnbd start", shop.id), st.tm)
params <- bgnbd.EstimateParameters(cal.cbs);
params
slackme(sprintf("bgnbd done", shop.id), st.tm)

LL <- bgnbd.cbs.LL(params, cal.cbs);
LL
p.matrix <- c(params, LL);
# for (i in 1:2){
#     params <- bgnbd.EstimateParameters(cal.cbs, params);
#     LL <- bgnbd.cbs.LL(params, cal.cbs);
#     p.matrix.row <- c(params, LL);
#     p.matrix <- rbind(p.matrix, p.matrix.row);
# }
# colnames(p.matrix) <- c("r", "alpha", "s", "beta", "LL")
# rownames(p.matrix) <- 1:3
names(p.matrix) <- c("r", "alpha", "a", "b", "LL")
p.matrix
bgnbd.Expectation(params, t=52)
idx <- 10778
x <- cal.cbs[cust==idx, "x"] %>% unlist()
t.x <- cal.cbs[cust==idx, "t.x"] %>% unlist()
T.cal <- cal.cbs[idx, "T.cal"] %>% unlist()
bgnbd.ConditionalExpectedTransactions(params, T.star = T.star, x, t.x, T.cal)
# [1] 1.676457
bgnbd.PAlive(params, x, t.x, T.cal)
# [1] 0.8163679

# for (i in seq(10, 25, 5)){
#     cond.expectation <- bgnbd.ConditionalExpectedTransactions(
#         params, T.star = 52, x = i,
#         t.x = 20, T.cal = 39)
#     cat ("x:",i,"\t Expectation:",cond.expectation, fill = TRUE) }

bgnbd.PlotFrequencyInCalibration(params, cal.cbs, censor)
bgnbd.PlotTransactionRateHeterogeneity(params)
bgnbd.PlotDropoutRateHeterogeneity(params)

comp <- bgnbd.PlotFreqVsConditionalExpectedFrequency(params, T.star,
                                                     cbs, x.star, censor)

rownames(comp) <- c("act", "exp", "bin")
comp

inc.tracking <- bgnbd.PlotTrackingInc(params, T.cal,
                                      T.tot, w.track.data,
                                      n.periods.final)
actual <- w.track.data
expected <- dc.CumulativeToIncremental(bgnbd.ExpectedCumulativeTransactions(params, 
                                                                            T.cal, T.tot, length(actual)))
xlab <- "Week"
ylab <- "Transactions"
xticklab <- NULL
title <- "Tracking Weekly Transactions"
ylim <- c(0, max(c(actual, expected)) * 1.05)
plot(actual, type = "l", xaxt = "n", xlab = xlab, ylab = ylab, 
     col = 1, ylim = ylim, main = title)
lines(expected, lty = 2, col = 2)
if (is.null(xticklab) == FALSE) {
    if (length(actual) != length(xticklab)) {
        stop("Plot error, xticklab does not have the correct size")
    }
    axis(1, at = 1:length(actual), labels = xticklab)
} else {
    axis(1, at = 1:length(actual), labels = 1:length(actual))
}
abline(v = max(T.cal), lty = 2)
legend("topleft", legend = c("Actual", "Model"), col = 1:2, 
       lty = 1:2, lwd = 1, bty = "n")
cum.tracking <- bgnbd.PlotTrackingCum(params, T.cal,
                                      T.tot, cum.tracking.data,
                                      n.periods.final)
cum.tracking[,20:25]


# data prep for rf.matrix
simElog <- system.file("data/discreteSimElog.csv",
                       package = "BTYD")
elog <- dc.ReadLines(simElog, cust.idx = 1, date.idx = 2)
elog$date <- as.Date(elog$date, "%Y-%m-%d")
elog[1:3,]
T.cal <- as.Date("1977-01-01")
simData <- dc.ElogToCbsCbt(elog, per="year", T.cal)
cal.cbs <- simData$cal$cbs

freq<- cal.cbs[,"x"]
rec <- cal.cbs[,"t.x"]
trans.opp <- 7 # transaction opportunities
rf.matrix <- dc.MakeRFmatrixCal(freq, rec, trans.opp) 
rf.matrix[1:5,]

data(donationsSummary)
rf.matrix2 <- donationsSummary$rf.matrix

# BG/BB
params <- bgbb.EstimateParameters(rf.matrix)
LL <- bgbb.rf.matrix.LL(params, rf.matrix)
p.matrix <- c(params, LL)
names(p.matrix) <- c("alpha", "beta", "gamma", "delta", "LL")
p.matrix
bgbb.PlotFrequencyInCalibration(params, rf.matrix, 7)
bgbb.PlotTransactionRateHeterogeneity(params)
bgbb.PlotDropoutRateHeterogeneity(params)

bgbb.Expectation(params, n=10)

# customer A
n.cal <- 6 
n.star <- 10 
x <- 0
t.x <- 0
bgbb.ConditionalExpectedTransactions(params, n.cal,n.star, x, t.x)
# [1] 0.1302
# customer B
x <- 4
t.x <- 5
bgbb.ConditionalExpectedTransactions(params, n.cal,n.star, x, t.x)
# [1] 3.628

holdout.cbs <- simData$holdout$cbs
x.star <- holdout.cbs[,"x.star"]
n.star <- 5 # length of the holdout period
x.star <- donationsSummary$x.star
comp <- bgbb.PlotFreqVsConditionalExpectedFrequency(params, n.star,
                                                    rf.matrix, x.star)
rownames(comp) <- c("act", "exp", "bin")
comp

comp <- bgbb.PlotRecVsConditionalExpectedFrequency(params, n.star,
                                                   rf.matrix, x.star)
rownames(comp) <- c("act", "exp", "bin")
comp

inc.track.data <- donationsSummary$annual.trans
n.cal <- 6
xtickmarks <- 1996:2006
inc.tracking <- bgbb.PlotTrackingInc(params, rf.matrix,
                                     inc.track.data,
                                     xticklab = xtickmarks)
rownames(inc.tracking) <- c("act", "exp")
inc.tracking

cum.track.data <- cumsum(inc.track.data)
cum.tracking <- bgbb.PlotTrackingCum(params, rf.matrix, cum.track.data,
                                     xticklab = xtickmarks)
rownames(cum.tracking) <- c("act", "exp")
cum.tracking




# gamma gamma spend model
data(cdnowSummary)

ave.spend <- cdnowSummary$m.x
tot.trans <- cdnowSummary$cbs[,"x"]
# There will be many warnings due to the zeroes that are
# included in the data above. To avoid them, use the following:
# (see example for spend.LL)
ave.spend <- ave.spend[which(tot.trans > 0)]
tot.trans <- tot.trans[which(tot.trans > 0)]
# We will let the spend function use default starting parameters
spend.EstimateParameters(ave.spend, tot.trans)
params <- c(6.25, 3.74, 15.44)
# Plot the actual and expected average transaction value across customers.
f.m.x <- spend.plot.average.transaction.value(params, ave.spend, tot.trans)
