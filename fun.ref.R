function (elog, per = "week", T.cal = max(elog$date), T.tot = max(elog$date), 
          merge.same.date = TRUE, cohort.birth.per = T.cal, dissipate.factor = 1, 
          statistic = "freq") 
{
    dc.WriteLine("Started making CBS and CBT from the ELOG...")
    elog <- dc.FilterCustByBirth(elog, cohort.birth.per)
    if (nrow(elog) == 0) 
        stop("error caused by customer birth filtering")
    elog <- elog[elog$date <= T.tot, ]
    if (nrow(elog) == 0) 
        stop("error caused by holdout period end date")
    elog <- dc.DissipateElog(elog, dissipate.factor)
    if (nrow(elog) == 0) 
        stop("error caused by event long dissipation")
    if (merge.same.date) {
        elog <- dc.MergeTransactionsOnSameDate(elog)
        if (nrow(elog) == 0) 
            stop("error caused by event log merging")
    }
    calibration.elog <- elog[elog$date <= T.cal, ]
    holdout.elog <- elog[elog$date > T.cal, ]
    split.elog.list <- dc.SplitUpElogForRepeatTrans(calibration.elog)
    repeat.transactions.elog <- split.elog.list$repeat.trans.elog
    cust.data <- split.elog.list$cust.data
    dc.WriteLine("Started Building CBS and CBT for calibration period...")
    cbt.cal <- dc.BuildCBTFromElog(calibration.elog, statistic)
    cbt.cal.rep.trans <- dc.BuildCBTFromElog(repeat.transactions.elog, 
                                             statistic)
    cbt.cal <- dc.MergeCustomers(cbt.cal, cbt.cal.rep.trans)
    dates <- data.frame(cust.data$birth.per, cust.data$last.date, 
                        T.cal)
    cbs.cal <- dc.BuildCBSFromCBTAndDates(cbt.cal, dates, per, 
                                          cbt.is.during.cal.period = TRUE)
    dc.WriteLine("Finished building CBS and CBT for calibration period.")
    cbt.holdout <- NULL
    cbs.holdout <- NULL
    if (nrow(holdout.elog) > 0) {
        dc.WriteLine("Started building CBS and CBT for holdout period...")
        cbt.holdout <- dc.BuildCBTFromElog(holdout.elog, statistic)
        dates <- c((T.cal + 1), T.tot)
        cbs.holdout <- dc.BuildCBSFromCBTAndDates(cbt.holdout, 
                                                  dates, per, cbt.is.during.cal.period = FALSE)
        cbt.holdout <- dc.MergeCustomers(cbt.cal, cbt.holdout)
        cbs.holdout <- dc.MergeCustomers(cbs.cal, cbs.holdout)
        dc.WriteLine("Finished building CBS and CBT for holdout.")
        dc.WriteLine("...Finished Making All CBS and CBT")
        return(list(cal = list(cbs = cbs.cal, cbt = cbt.cal), 
                    holdout = list(cbt = cbt.holdout, cbs = cbs.holdout), 
                    cust.data = cust.data))
    }
    dc.WriteLine("...Finished Making All CBS and CBT")
    return(list(cal = list(cbs = cbs.cal, cbt = cbt.cal), holdout = list(cbt = cbt.holdout, 
                                                                         cbs = cbs.holdout), cust.data = cust.data))
}

dc.SplitUpElogForRepeatTrans
function (elog) 
{
    dc.WriteLine("Started Creating Repeat Purchases")
    unique.custs <- unique(elog$cust)
    first.trans.indices <- rep(0, length(unique.custs))
    last.trans.indices <- rep(0, length(unique.custs))
    count <- 0
    for (cust in unique.custs) {
        count <- count + 1
        cust.indices <- which(elog$cust == cust)
        first.trans.indices[count] <- min(cust.indices[which(elog$date[cust.indices] == 
                                                                 min(elog$date[cust.indices]))])
        last.trans.indices[count] <- min(cust.indices[which(elog$date[cust.indices] == 
                                                                max(elog$date[cust.indices]))])
    }
    repeat.trans.elog <- elog[-first.trans.indices, ]
    first.trans.data <- elog[first.trans.indices, ]
    last.trans.data <- elog[last.trans.indices, ]
    names(first.trans.data)[-1] <- paste("first.", names(first.trans.data)[-1], 
                                         sep = "")
    names(first.trans.data)[which(names(first.trans.data) == 
                                      "first.date")] <- "birth.per"
    names(last.trans.data) <- paste("last.", names(last.trans.data), 
                                    sep = "")
    cust.data <- data.frame(first.trans.data, last.trans.data[, 
                                                              -1])
    names(cust.data) <- c(names(first.trans.data), names(last.trans.data)[-1])
    dc.WriteLine("Finished Creating Repeat Purchases")
    return(list(repeat.trans.elog = repeat.trans.elog, cust.data = cust.data))
}


function (elog, statistic = "freq") 
{
    dc.WriteLine("Started Building CBT...")
    if (statistic == "freq") {
        return(dc.CreateFreqCBT(elog))
    }
    else if (statistic == "reach") {
        return(dc.CreateReachCBT(elog))
    }
    else if (statistic == "total.spend") {
        return(dc.CreateSpendCBT(elog))
    }
    else if (statistic == "average.spend") {
        return(dc.CreateSpendCBT(elog, is.avg.spend = TRUE))
    }
    else {
        stop("Invalid cbt build (var: statistic) specified.")
    }
}


dc.CreateFreqCBT
function (elog) 
{
    elog$cust <- factor(elog$cust, levels = unique(elog$cust))
    xt <- xtabs(~cust + date, data = elog)
    dc.WriteLine("...Completed Freq CBT")
    return(xt)
}



dc.BuildCBSFromCBTAndDates
function (cbt, dates, per, cbt.is.during.cal.period = TRUE) 
{
    if (cbt.is.during.cal.period == TRUE) {
        dc.WriteLine("Started making calibration period CBS...")
        custs.first.dates <- dates[, 1]
        custs.last.dates <- dates[, 2]
        T.cal <- dates[, 3]
        if (length(custs.first.dates) != length(custs.last.dates)) {
            stop("Invalid dates (different lengths) in BuildCBSFromFreqCBTAndDates")
        }
        f <- rowSums(cbt)
        r <- as.numeric(difftime(custs.last.dates, custs.first.dates, 
                                 units = "days"))
        T <- as.numeric(difftime(T.cal, custs.first.dates, units = "days"))
        x <- switch(per, day = 1, week = 7, month = 365/12, quarter = 365/4, 
                    year = 365)
        r = r/x
        T = T/x
        cbs = cbind(f, r, T)
        rownames(cbs) <- rownames(cbt)
        colnames(cbs) <- c("x", "t.x", "T.cal")
    }
    else {
        dc.WriteLine("Started making holdout period CBS...")
        date.begin.holdout.period <- dates[1]
        date.end.holdout.period <- dates[2]
        f <- rowSums(cbt)
        T <- as.numeric(difftime(date.end.holdout.period, date.begin.holdout.period, 
                                 units = "days")) + 1
        x <- switch(per, day = 1, week = 7, month = 365/12, quarter = 365/4, 
                    year = 365)
        T = T/x
        cbs = cbind(f, T)
        rownames(cbs) <- rownames(cbt)
        colnames(cbs) <- c("x.star", "T.star")
    }
    dc.WriteLine("Finished building CBS.")
    return(cbs)
}


dc.MergeCustomers
function (data.correct, data.to.correct) 
{
    data.to.correct.new <- matrix(0, nrow = nrow(data.correct), 
                                  ncol = ncol(data.to.correct))
    orig.order <- 1:nrow(data.correct)
    orig.order <- orig.order[order(rownames(data.correct))]
    data.correct.ordered <- data.correct[order(rownames(data.correct)), 
                                         ]
    if (is.null(nrow(data.correct.ordered))) {
        rownames(data.correct.ordered) <- rownames(data.correct)[order(rownames(data.correct))]
        colnames(data.correct.ordered) <- colnames(data.correct)
    }
    data.to.correct <- data.to.correct[order(rownames(data.to.correct)), 
                                       ]
    rownames(data.to.correct.new) <- rownames(data.correct.ordered)
    colnames(data.to.correct.new) <- colnames(data.to.correct)
    ii.correct <- 1
    ii.to.correct <- 1
    max.correct.iterations <- nrow(data.correct.ordered)
    max.to.correct.iterations <- nrow(data.to.correct)
    cust.list.correct <- rownames(data.correct.ordered)
    cust.list.to.correct <- rownames(data.to.correct)
    cust.correct.indices <- c()
    cust.to.correct.indices <- c()
    while (ii.correct <= max.correct.iterations & ii.to.correct <= 
           max.to.correct.iterations) {
        cur.cust.correct <- cust.list.correct[ii.correct]
        cur.cust.to.correct <- cust.list.to.correct[ii.to.correct]
        if (cur.cust.correct < cur.cust.to.correct) {
            ii.correct <- ii.correct + 1
        }
        else if (cur.cust.correct > cur.cust.to.correct) {
            ii.to.correct <- ii.to.correct + 1
        }
        else if (cur.cust.correct == cur.cust.to.correct) {
            cust.correct.indices <- c(cust.correct.indices, ii.correct)
            cust.to.correct.indices <- c(cust.to.correct.indices, 
                                         ii.to.correct)
            ii.correct <- ii.correct + 1
            ii.to.correct <- ii.to.correct + 1
        }
        else {
            stop("Array checking error in MergeCustomers")
        }
    }
    data.to.correct.new[cust.correct.indices, ] <- data.to.correct
    data.to.correct.new <- data.to.correct.new[order(orig.order), 
                                               ]
    return(data.to.correct.new)
}


bgnbd.PlotFreqVsConditionalExpectedFrequency
function (params, T.star, cal.cbs, x.star, censor, xlab = "Calibration period transactions", 
          ylab = "Holdout period transactions", xticklab = NULL, title = "Conditional Expectation") 
{
    tryCatch(x <- cal.cbs[, "x"], error = function(e) stop("Error in bgnbd.PlotFreqVsConditionalExpectedFrequency: cal.cbs must have a frequency column labelled \"x\""))
    tryCatch(t.x <- cal.cbs[, "t.x"], error = function(e) stop("Error in bgnbd.PlotFreqVsConditionalExpectedFrequency: cal.cbs must have a recency column labelled \"t.x\""))
    tryCatch(T.cal <- cal.cbs[, "T.cal"], error = function(e) stop("Error in bgnbd.PlotFreqVsConditionalExpectedFrequency: cal.cbs must have a column for length of time observed labelled \"T.cal\""))
    dc.check.model.params(c("r", "alpha", "a", "b"), params, 
                          "bgnbd.PlotFreqVsConditionalExpectedFrequency")
    if (censor > max(x)) 
        stop("censor too big (> max freq) in PlotFreqVsConditionalExpectedFrequency.")
    if (any(T.star < 0) || !is.numeric(T.star)) 
        stop("T.star must be numeric and may not contain negative numbers.")
    if (any(x.star < 0) || !is.numeric(x.star)) 
        stop("x.star must be numeric and may not contain negative numbers.")
    n.bins = censor + 1
    transaction.actual = rep(0, n.bins)
    transaction.expected = rep(0, n.bins)
    bin.size = rep(0, n.bins)
    for (cc in 0:censor) {
        if (cc != censor) {
            this.bin = which(cc == x)
        }
        else if (cc == censor) {
            this.bin = which(x >= cc)
        }
        n.this.bin = length(this.bin)
        bin.size[cc + 1] = n.this.bin
        transaction.actual[cc + 1] = sum(x.star[this.bin])/n.this.bin
        transaction.expected[cc + 1] = sum(bgnbd.ConditionalExpectedTransactions(params, 
                                                                                 T.star, x[this.bin], t.x[this.bin], T.cal[this.bin]))/n.this.bin
    }
    col.names = paste(rep("freq", length(censor + 1)), (0:censor), 
                      sep = ".")
    col.names[censor + 1] = paste(col.names[censor + 1], "+", 
                                  sep = "")
    comparison = rbind(transaction.actual, transaction.expected, 
                       bin.size)
    colnames(comparison) = col.names
    if (is.null(xticklab) == FALSE) {
        x.labels = xticklab
    }
    if (is.null(xticklab) != FALSE) {
        if (censor < ncol(comparison)) {
            x.labels = 0:(censor)
            x.labels[censor + 1] = paste(censor, "+", sep = "")
        }
        if (censor >= ncol(comparison)) {
            x.labels = 0:(ncol(comparison))
        }
    }
    actual = comparison[1, ]
    expected = comparison[2, ]
    ylim = c(0, ceiling(max(c(actual, expected)) * 1.1))
    plot(actual, type = "l", xaxt = "n", col = 1, ylim = ylim, 
         xlab = xlab, ylab = ylab, main = title)
    lines(expected, lty = 2, col = 2)
    axis(1, at = 1:ncol(comparison), labels = x.labels)
    legend("topleft", legend = c("Actual", "Model"), col = 1:2, 
           lty = 1:2, lwd = 1)
    return(comparison)
}



function (params, T.cal, T.tot, actual.inc.tracking.data, xlab = "Week", 
          ylab = "Transactions", xticklab = NULL, title = "Tracking Weekly Transactions") 
{
    dc.check.model.params(c("r", "alpha", "a", "b"), params, 
                          "bgnbd.Plot.PlotTrackingCum")
    if (any(T.cal < 0) || !is.numeric(T.cal)) 
        stop("T.cal must be numeric and may not contain negative numbers.")
    if (any(actual.inc.tracking.data < 0) || !is.numeric(actual.inc.tracking.data)) 
        stop("actual.inc.tracking.data must be numeric and may not contain negative numbers.")
    if (length(T.tot) > 1 || T.tot < 0 || !is.numeric(T.tot)) 
        stop("T.cal must be a single numeric value and may not be negative.")
    actual <- actual.inc.tracking.data
    expected <- dc.CumulativeToIncremental(bgnbd.ExpectedCumulativeTransactions(params, 
                                                                                T.cal, T.tot, length(actual)))
    ylim <- c(0, max(c(actual, expected)) * 1.05)
    plot(actual, type = "l", xaxt = "n", xlab = xlab, ylab = ylab, 
         col = 1, ylim = ylim, main = title)
    lines(expected, lty = 2, col = 2)
    if (is.null(xticklab) == FALSE) {
        if (length(actual) != length(xticklab)) {
            stop("Plot error, xticklab does not have the correct size")
        }
        axis(1, at = 1:length(actual), labels = xticklab)
    }
    else {
        axis(1, at = 1:length(actual), labels = 1:length(actual))
    }
    abline(v = max(T.cal), lty = 2)
    legend("topright", legend = c("Actual", "Model"), col = 1:2, 
           lty = 1:2, lwd = 1)
    return(rbind(actual, expected))
}



dc.MakeRFmatrixCal
function (frequencies, periods.of.final.purchases, num.of.purchase.periods, 
          holdout.frequencies = NULL) 
{
    if (!is.numeric(periods.of.final.purchases)) {
        stop("periods.of.final.purchases must be numeric")
    }
    if (length(periods.of.final.purchases) != length(frequencies)) {
        stop(paste("number of customers in frequencies is not equal", 
                   "to the last purchase period vector"))
    }
    rf.mx.skeleton <- dc.MakeRFmatrixSkeleton(num.of.purchase.periods)
    if (is.null(holdout.frequencies)) {
        RF.matrix <- cbind(rf.mx.skeleton, num.of.purchase.periods, 
                           0)
        colnames(RF.matrix) <- c("x", "t.x", "n.cal", "custs")
    }
    else {
        RF.matrix <- cbind(rf.mx.skeleton, num.of.purchase.periods, 
                           0, 0)
        colnames(RF.matrix) <- c("x", "t.x", "n.cal", "custs", 
                                 "x.star")
    }
    rf.n.custs <- cbind(frequencies, periods.of.final.purchases, 
                        holdout.frequencies)
    zeroes.rf.subset <- which(rf.n.custs[, 1] == 0)
    RF.matrix[1, 4] <- length(zeroes.rf.subset)
    if (!is.null(holdout.frequencies)) {
        RF.matrix[1, 5] <- sum(holdout.frequencies[zeroes.rf.subset])
    }
    rf.n.custs <- rf.n.custs[-zeroes.rf.subset, ]
    rf.n.custs <- rf.n.custs[order(rf.n.custs[, 1], rf.n.custs[, 
                                                               2]), ]
    current.pair <- c(rf.n.custs[1, 1], rf.n.custs[1, 2])
    same.item.in.a.row.counter <- 1
    if (!is.null(holdout.frequencies)) {
        x.star.total <- rf.n.custs[1, 3]
    }
    num.count.points <- nrow(rf.n.custs)
    for (ii in 2:num.count.points) {
        last.pair <- current.pair
        current.pair <- c(rf.n.custs[ii, 1], rf.n.custs[ii, 2])
        if (identical(last.pair, current.pair)) {
            same.item.in.a.row.counter <- same.item.in.a.row.counter + 
                1
            if (!is.null(holdout.frequencies)) {
                x.star.total <- x.star.total + rf.n.custs[ii, 
                                                          3]
            }
        }
        else {
            x <- last.pair[1]
            t.x <- last.pair[2]
            corresponding.rf.index <- (x - 1) + 1 + t.x * (t.x - 
                                                               1)/2 + 1
            RF.matrix[corresponding.rf.index, 4] <- same.item.in.a.row.counter
            same.item.in.a.row.counter <- 1
            if (!is.null(holdout.frequencies)) {
                RF.matrix[corresponding.rf.index, 5] <- x.star.total
                x.star.total <- rf.n.custs[ii, 3]
            }
        }
        if (ii == num.count.points) {
            x <- current.pair[1]
            t.x <- current.pair[2]
            corresponding.rf.index <- (x - 1) + 1 + t.x * (t.x - 
                                                               1)/2 + 1
            RF.matrix[corresponding.rf.index, 4] <- same.item.in.a.row.counter
            same.item.in.a.row.counter <- NULL
            if (!is.null(holdout.frequencies)) {
                RF.matrix[corresponding.rf.index, 5] <- x.star.total
                x.star.total = NULL
            }
        }
    }
    return(RF.matrix)
}

pnbd.PlotFreqVsConditionalExpectedFrequency
function (params, T.star, cal.cbs, x.star, censor, xlab = "Calibration period transactions", 
    ylab = "Holdout period transactions", xticklab = NULL, title = "Conditional Expectation") 
{
    tryCatch(x <- cal.cbs[, "x"], error = function(e) stop("Error in pnbd.PlotFreqVsConditionalExpectedFrequency: cal.cbs must have a frequency column labelled \"x\""))
    tryCatch(t.x <- cal.cbs[, "t.x"], error = function(e) stop("Error in pnbd.PlotFreqVsConditionalExpectedFrequency: cal.cbs must have a recency column labelled \"t.x\""))
    tryCatch(T.cal <- cal.cbs[, "T.cal"], error = function(e) stop("Error in pnbd.PlotFreqVsConditionalExpectedFrequency: cal.cbs must have a column for length of time observed labelled \"T.cal\""))
    dc.check.model.params(c("r", "alpha", "s", "beta"), params, 
        "pnbd.PlotFreqVsConditionalExpectedFrequency")
    if (censor > max(x)) 
        stop("censor too big (> max freq) in PlotFreqVsConditionalExpectedFrequency.")
    if (any(T.star < 0) || !is.numeric(T.star)) 
        stop("T.star must be numeric and may not contain negative numbers.")
    if (any(x.star < 0) || !is.numeric(x.star)) 
        stop("x.star must be numeric and may not contain negative numbers.")
    n.bins <- censor + 1
    transaction.actual <- rep(0, n.bins)
    transaction.expected <- rep(0, n.bins)
    bin.size <- rep(0, n.bins)
    for (cc in 0:censor) {
        if (cc != censor) {
            this.bin <- which(cc == x)
        }
        else if (cc == censor) {
            this.bin <- which(x >= cc)
        }
        n.this.bin <- length(this.bin)
        bin.size[cc + 1] <- n.this.bin
        transaction.actual[cc + 1] <- sum(x.star[this.bin])/n.this.bin
        transaction.expected[cc + 1] <- sum(pnbd.ConditionalExpectedTransactions(params, 
            T.star, x[this.bin], t.x[this.bin], T.cal[this.bin]))/n.this.bin
    }
    col.names <- paste(rep("freq", length(censor + 1)), (0:censor), 
        sep = ".")
    col.names[censor + 1] <- paste(col.names[censor + 1], "+", 
        sep = "")
    comparison <- rbind(transaction.actual, transaction.expected, 
        bin.size)
    colnames(comparison) <- col.names
    if (is.null(xticklab) == FALSE) {
        x.labels <- xticklab
    }
    else {
        if (censor < ncol(comparison)) {
            x.labels <- 0:(censor)
            x.labels[censor + 1] <- paste(censor, "+", sep = "")
        }
        else {
            x.labels <- 0:(ncol(comparison))
        }
    }
    actual <- comparison[1, ]
    expected <- comparison[2, ]
    ylim <- c(0, ceiling(max(c(actual, expected)) * 1.1))
    plot(actual, type = "l", xaxt = "n", col = 1, ylim = ylim, 
        xlab = xlab, ylab = ylab, main = title)
    lines(expected, lty = 2, col = 2)
    axis(1, at = 1:ncol(comparison), labels = x.labels)
    legend("topleft", legend = c("Actual", "Model"), col = 1:2, 
        lty = 1:2, lwd = 1)
    return(comparison)
}