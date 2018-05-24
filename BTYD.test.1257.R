if (!"mylib" %in% ls()) source("../auxiliary/mylib.R")
source("pnbd.R")
mylib(c("data.table", "BTYD", "magrittr"))
load("data/Timberland-2018-04/data_for_btyd.RData")
head(dt1)
dt <- dt1[, .(MemberID, DateId, TotalPayment)]
setnames(dt, colnames(dt), c("cust", "date", "sales"))
dt[, date:=as.Date(as.character(date), "%Y%m%d")]
setorder(dt, cust, date)
ed.dt <- as.Date("2017-03-21")
train.raw <- dt[date <= ed.dt]
setorder(train.raw, cust, date)
train.raw[, `:=` (n=.N, nth=1:.N, nth.rev=.N:1), by = .(cust)]
train.raw.one <- train.raw[n==1|nth==1]
train.raw.rep <- train.raw[nth > 1]
stopifnot(nrow(train.raw)==nrow(train.raw.one)+nrow(train.raw.rep))
# tr <- dc.SplitUpElogForRepeatTrans(train.raw)
clean.elog <- train.raw.rep[, .(cust, date, sales)]
freq.cbt <- dc.CreateFreqCBT(clean.elog)

elog <- train.raw[, .(cust, date, sales)]
tot.cbt <- dc.CreateFreqCBT(elog)
dim(tot.cbt)
cal.cbt <- dc.MergeCustomers(tot.cbt, freq.cbt)

# cust.data <- train.raw[(nth == 1 | nth.rev == 1)][, type:=ifelse(nth==1, "first", "last")]
# cust.data1 <- cust.data[n==1][, type:="last"]
# cust.data <- rbind(cust.data, cust.data1)
cust.data <- rbind(train.raw[(nth == 1 | nth.rev == 1)][, type:=ifelse(nth==1, "first", "last")], train.raw[n==1][, type:="last"])
cust.data <- cust.data[, .(cust, date, sales, type)] %>% 
    dcast(., cust~type, value.var = c("date", "sales"))
birth.periods <- cust.data$date_first
last.dates <- cust.data$date_last
cal.cbs.dates <- data.frame(birth.periods, last.dates, ed.dt)
cal.cbs <- dc.BuildCBSFromCBTAndDates(cal.cbt, cal.cbs.dates, per="week")

params <- pnbd.EstimateParameters(cal.cbs);
params
# [1]  0.5534 10.5802  0.6061 11.6562
LL <- pnbd.cbs.LL(params, cal.cbs);
LL
p.matrix <- c(params, LL);
for (i in 1:2){
    params <- pnbd.EstimateParameters(cal.cbs, params);
    LL <- pnbd.cbs.LL(params, cal.cbs);
    p.matrix.row <- c(params, LL);
    p.matrix <- rbind(p.matrix, p.matrix.row);
}
colnames(p.matrix) <- c("r", "alpha", "s", "beta", "LL"); rownames(p.matrix) <- 1:3;
p.matrix

pnbd.PlotTransactionRateHeterogeneity(params)
pnbd.PlotDropoutRateHeterogeneity(params)

pred.length <- 52 # in weeks
pnbd.Expectation(params, t=pred.length)
x <- cal.cbs[1516, 1]
t.x <- cal.cbs[1516, 2]
T.cal <- cal.cbs[1516, 3]
pnbd.ConditionalExpectedTransactions(params, T.star = pred.length, x, t.x, T.cal)
pnbd.PAlive(params, x, t.x, T.cal)
for (i in seq(10, 25, 5)){
    cond.expectation <- pnbd.ConditionalExpectedTransactions(
        params, T.star = 52, x = i,
        t.x = 20, T.cal = 39)
    cat ("x:",i,"\t Expectation:",cond.expectation, fill = TRUE) }
pnbd.PlotFrequencyInCalibration(params, cal.cbs, 7)

save(p.matrix, file="p.matrix.RData")

elog <- dc.SplitUpElogForRepeatTrans(elog)$repeat.trans.elog;
x.star <- rep(0, nrow(cal.cbs));
cal.cbs <- cbind(cal.cbs, x.star);
elog.custs <- elog$cust;
for (i in 1:nrow(cal.cbs)){
    current.cust <- rownames(cal.cbs)[i]
    tot.cust.trans <- length(which(elog.custs == current.cust)) cal.trans <- cal.cbs[i, "x"]
    cal.cbs[i, "x.star"] <- tot.cust.trans - cal.trans
}
cal.cbs[1:3,]


elog <- dt[, .(cust, date, sales)]
tot.cbt <- dc.CreateFreqCBT(elog)
dim(tot.cbt)
cal.cbt <- dc.MergeCustomers(tot.cbt, freq.cbt)

# cust.data <- train.raw[(nth == 1 | nth.rev == 1)][, type:=ifelse(nth==1, "first", "last")]
# cust.data1 <- cust.data[n==1][, type:="last"]
# cust.data <- rbind(cust.data, cust.data1)
cust.data <- rbind(train.raw[(nth == 1 | nth.rev == 1)][, type:=ifelse(nth==1, "first", "last")], train.raw[n==1][, type:="last"])
cust.data <- cust.data[, .(cust, date, sales, type)] %>% 
    dcast(., cust~type, value.var = c("date", "sales"))