if (!"mylib" %in% ls()) source("../auxiliary/mylib.R")
if (!"slackme" %in% ls()) source("../auxiliary/slackme.R")
source("fun.fixed.R")
mylib(c("data.table", "BTYD", "magrittr", "lubridate", "parallel", "googleCloudStorageR"))

# fixed
# http://erikjohansson.blogspot.tw/2015/12/problems-with-btyd-walk-through-fixed.html

shop.id <- "all"
fnm <- sprintf("data/intermediates/dt.dly_%s.RData", shop.id)
load(fnm)
setnames(dt.dly, "TotalPayment", "gross.sales")
dt.dly.raw <- dt.dly
# date.ls <- seq.Date(as.Date("2013-07-01"), as.Date("2016-04-30"), "month")
suffix <- "BTYD_all"
if (!dir.exists(file.path("plots", suffix))) dir.create(file.path("plots", suffix))
dt.dly.stats <- dt.dly[, .(st.dt = min(order.date), 
                           ed.dt = max(order.date),
                           duration = as.integer(max(order.date)-min(order.date))), by = ShopId]
shop.ls <- dt.dly.stats[duration > 365*2]$ShopId
# fls <- file.path("plots", sprintf(suffix)) %>% list.files()
# done.ls <- fls %>% strsplit(., "_") %>%
#     lapply(., "[", 2) %>% unlist()
# not.done.ls <- table(done.ls)[table(done.ls) != 6] %>% names() %>% as.integer()
# file.remove(file.path("plots", sprintf(suffix), fls[which(done.ls %in% not.done.ls)]))
# done.ls <- done.ls %>% unique() %>% as.integer()
# done.ls <- done.ls[!done.ls %in% not.done.ls]
# shop.ls <- shop.ls[!shop.ls %in% done.ls]
# shop.id <- 360
# shop.ls <- c(11, 49, 1327, 815, 16)
result.lss <- mclapply(shop.ls, function(shop.id) {
    param.ls <- NULL
    result.ls <- NULL
    dt.dly <- dt.dly.raw[ShopId == shop.id]
    duration <- dt.dly.stats[ShopId==shop.id]$duration
    start.date <- dt.dly.stats[ShopId==shop.id]$st.dt
    ed.dt <- dt.dly.stats[ShopId==shop.id]$ed.dt
    if (duration > 365 * 2) {
        tot.wks <- difftime(ed.dt, start.date, units = "weeks") %>% floor(.) %>% as.integer()
        end.date <- start.date %m+% weeks(tot.wks)
        mid.wks <- 52
        cut.date <- end.date %m-% weeks(mid.wks)
    } else {
        tot.wks <- difftime(ed.dt, start.date, units = "weeks") %>% floor(.) %>% as.integer()
        mid.wks <- tot.wks %/% 2
        end.date <- start.date %m+% weeks(tot.wks)
        cut.date <- start.date %m+% weeks(mid.wks)
    }
    if (which(shop.id==dt.dly.stats$ShopId) %% 1==0) {
        # cat(sprintf("%s\n", as.character(start.date)))
        cat(sprintf("shop:%s dur:%s st.dt:%s\n", shop.id, duration, as.character(start.date)))
    }
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
    
    # cut.date <- as.Date("2018-04-30") %m-% weeks(52)
    
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
    
    return(tryCatch({
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
        if (max(cal.cbs$x) < censor) censor <- max(cal.cbs$x)
        png(sprintf("plots/%s/plot7_%s_%s.png", suffix, shop.id, as.character(start.date)))
        bgnbd.PlotFrequencyInCalibration.fixed(params, cal.cbs, censor)
        dev.off()
        png(sprintf("plots/%s/plot8_%s_%s.png", suffix, shop.id, as.character(start.date)))
        bgnbd.PlotTransactionRateHeterogeneity(params)
        dev.off()
        png(sprintf("plots/%s/plot9_%s_%s.png", suffix, shop.id, as.character(start.date)))
        bgnbd.PlotDropoutRateHeterogeneity(params)
        dev.off()
        
        png(sprintf("plots/%s/plot60_%s_%s.png", suffix, shop.id, as.character(start.date)))
        comp <- bgnbd.PlotFreqVsConditionalExpectedFrequency(params, T.star,
                                                             cbs, x.star, censor)
        dev.off()
        
        rownames(comp) <- c("act", "exp", "bin")
        result.ls <- c(result.ls, list(comp))
        
        png(sprintf("plots/%s/plot5_%s_%s.png", suffix, shop.id, as.character(start.date)))
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
        
        png(sprintf("plots/%s/plot6_%s_%s.png", suffix, shop.id, as.character(start.date)))
        cum.tracking <- bgnbd.PlotTrackingCum(params, T.cal,
                                              T.tot, cum.tracking.data,
                                              n.periods.final)
        dev.off()
        
        # cum.tracking[,20:25]
        slackme(sprintf("%s %s bgnbd.plot done", shop.id, as.character(start.date)), st.tm)
        
        obj <- list(param.ls, result.ls, inc.tracking, cum.tracking)
        obj
    }, error=function(e) sprintf("%s error", shop.id)))
}, mc.cores = 4, mc.preschedule=FALSE)
# tmp <- cal.cbs.spend
# tmp[, x1:=x]
# tmp[x > 7, x1:=7]
# tmp1 <- tmp[, .(N=.N, x=sum(x), x.star=sum(x.star), avg.x=sum(x.star)/.N), by = x1]
# setorder(tmp1, x)
# plot(tmp1)

system(sprintf("zip %s plots/%s/*", suffix, suffix))
save(result.lss, file = file.path("data/models", sprintf("%s.result.lss-v01.RData", suffix)))

default.bucket <- "helen-ml-4standard"
Sys.setenv("GCS_AUTH_FILE" = "/home/helen/gcs.oauth",
           "GCS_DEFAULT_BUCKET" = default.bucket)
library(googleCloudStorageR)
gcs_auth()
gcs_global_bucket(default.bucket)
# gcs_list_objects()
gcs_upload(sprintf("%s.zip", suffix), name = sprintf("%s.zip", suffix))

file.copy("BTYD.monthly.series.R", sprintf("scripts/%s.R", suffix), overwrite = T)
slackme("done", st.tm)
