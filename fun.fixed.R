bgnbd.pmf.General.fixed <- function (params, t.start, t.end, x) {
    max.length = max(length(t.start), length(t.end), length(x))
    if (max.length%%length(t.start)) 
        warning("Maximum vector length not a multiple of the length of t.start")
    if (max.length%%length(t.end)) 
        warning("Maximum vector length not a multiple of the length of t.end")
    if (max.length%%length(x)) 
        warning("Maximum vector length not a multiple of the length of x")
    dc.check.model.params(c("r", "alpha", "a", "b"), params, 
                          "bgnbd.pmf.General")
    if (any(t.start < 0) || !is.numeric(t.start)) 
        stop("t.start must be numeric and may not contain negative numbers.")
    if (any(t.end < 0) || !is.numeric(t.end)) 
        stop("t.end must be numeric and may not contain negative numbers.")
    if (any(x < 0) || !is.numeric(x)) 
        stop("x must be numeric and may not contain negative numbers.")
    t.start = rep(t.start, length.out = max.length)
    t.end = rep(t.end, length.out = max.length)
    x = rep(x, length.out = max.length)
    if (any(t.start > t.end)) {
        stop("Error in bgnbd.pmf.General: t.start > t.end.")
    }
    r <- params[1]
    alpha <- params[2]
    a <- params[3]
    b <- params[4]
    equation.part.0 <- rep(0, max.length)
    t = t.end - t.start
    term3 = rep(0, max.length)
    term1 = ifelse(x < 170, beta(a, b + x)/beta(a, b) * gamma(r + x)/gamma(r)/factorial(x) * 
        ((alpha/(alpha + t))^r) * ((t/(alpha + t))^x),
        beta(a, b + x)/beta(a, b) / beta(r, x) / (x + 1) * 
        ((alpha/(alpha + t))^r) * ((t/(alpha + t))^x))
    for (i in 1:max.length) {
        if (x[i] > 0) {
            ii = c(0:(x[i] - 1))
            summation.term = ifelse(x < 170, 
                                    sum(gamma(r + ii)/gamma(r)/factorial(ii) * 
                                            ((t[i]/(alpha + t[i]))^ii)), 
                                    sum(1 / beta(r, ii) / (ii + 1) * 
                                            ((t[i]/(alpha + t[i]))^ii)))
            term3[i] = 1 - (((alpha/(alpha + t[i]))^r) * summation.term)
        }
    }
    term2 = as.numeric(x > 0) * beta(a + 1, b + x - 1)/beta(a, 
                                                            b) * term3
    return(term1 + term2)
}

bgnbd.pmf.fixed <- function (params, t, x) {
    max.length <- max(length(t), length(x))
    if (max.length%%length(t)) 
        warning("Maximum vector length not a multiple of the length of t")
    if (max.length%%length(x)) 
        warning("Maximum vector length not a multiple of the length of x")
    dc.check.model.params(c("r", "alpha", "a", "b"), params, 
                          "bgnbd.pmf")
    if (any(t < 0) || !is.numeric(t)) 
        stop("t must be numeric and may not contain negative numbers.")
    if (any(x < 0) || !is.numeric(x)) 
        stop("x must be numeric and may not contain negative numbers.")
    t <- rep(t, length.out = max.length)
    x <- rep(x, length.out = max.length)
    return(bgnbd.pmf.General.fixed(params, 0, t, x))
}

bgnbd.pmf <- function (params, t, x) {
    max.length <- max(length(t), length(x))
    if (max.length%%length(t)) 
        warning("Maximum vector length not a multiple of the length of t")
    if (max.length%%length(x)) 
        warning("Maximum vector length not a multiple of the length of x")
    dc.check.model.params(c("r", "alpha", "a", "b"), params, 
                          "bgnbd.pmf")
    if (any(t < 0) || !is.numeric(t)) 
        stop("t must be numeric and may not contain negative numbers.")
    if (any(x < 0) || !is.numeric(x)) 
        stop("x must be numeric and may not contain negative numbers.")
    t <- rep(t, length.out = max.length)
    x <- rep(x, length.out = max.length)
    return(bgnbd.pmf.General(params, 0, t, x))
}

bgnbd.PlotFrequencyInCalibration.fixed <- function (params, cal.cbs, censor, 
                                              plotZero = TRUE, 
                                              xlab = "Calibration period transactions", 
                                              ylab = "Customers", 
                                              title = "Frequency of Repeat Transactions") {
    tryCatch(x <- cal.cbs[, "x"], error = function(e) stop("Error in bgnbd.PlotFrequencyInCalibration: cal.cbs must have a frequency column labelled \"x\""))
    tryCatch(T.cal <- cal.cbs[, "T.cal"], error = function(e) stop("Error in bgnbd.PlotFrequencyInCalibration: cal.cbs must have a column for length of time observed labelled \"T.cal\""))
    dc.check.model.params(c("r", "alpha", "a", "b"), params, 
                          "bgnbd.PlotFrequencyInCalibration")
    if (censor > max(x)) 
        stop("censor too big (> max freq) in PlotFrequencyInCalibration.")
    x = cal.cbs$x
    T.cal = cal.cbs$T.cal
    n.x <- rep(0, max(x) + 1)
    ncusts = nrow(cal.cbs)
    for (ii in unique(x)) {
        n.x[ii + 1] <- sum(ii == x)
    }
    n.x.censor <- sum(n.x[(censor + 1):length(n.x)])
    n.x.actual <- c(n.x[1:censor], n.x.censor)
    T.value.counts <- table(T.cal)
    T.values <- as.numeric(names(T.value.counts))
    n.T.values <- length(T.values)
    n.x.expected <- rep(0, length(n.x.actual))
    n.x.expected.all <- rep(0, max(x) + 1)
    for (ii in 0:max(x)) {
        this.x.expected = 0
        if ((params[4]+ii-1) <=0 ) next
        for (T.idx in 1:n.T.values) {
            Tx = T.values[T.idx]
            if (Tx == 0) 
                next
            n.T = T.value.counts[T.idx]
            # print(c(ii, Tx))
            # flush.console()
            if (ii > 170) {
                prob.of.this.x.for.this.T <- bgnbd.pmf.fixed(params, Tx, ii)
            } else {
                prob.of.this.x.for.this.T <- bgnbd.pmf(params, Tx, ii)
            }
            
            expected.given.x.and.T = n.T * prob.of.this.x.for.this.T
            this.x.expected = this.x.expected + expected.given.x.and.T
        }
        n.x.expected.all[ii + 1] = this.x.expected
    }
    n.x.expected[1:censor] = n.x.expected.all[1:censor]
    n.x.expected[censor + 1] = sum(n.x.expected.all[(censor + 
                                                         1):(max(x) + 1)])
    col.names <- paste(rep("freq", length(censor + 1)), (0:censor), 
                       sep = ".")
    col.names[censor + 1] <- paste(col.names[censor + 1], "+", 
                                   sep = "")
    censored.freq.comparison <- rbind(n.x.actual, n.x.expected)
    colnames(censored.freq.comparison) <- col.names
    cfc.plot <- censored.freq.comparison
    if (plotZero == FALSE) 
        cfc.plot <- cfc.plot[, -1]
    n.ticks <- ncol(cfc.plot)
    if (plotZero == TRUE) {
        x.labels <- 0:(n.ticks - 1)
        x.labels[n.ticks] <- paste(n.ticks - 1, "+", sep = "")
    }
    ylim <- c(0, ceiling(max(cfc.plot) * 1.1))
    barplot(cfc.plot, names.arg = x.labels, beside = TRUE, ylim = ylim, 
            main = title, xlab = xlab, ylab = ylab, col = 1:2)
    legend("topright", legend = c("Actual", "Model"), col = 1:2, 
           lwd = 2)
    return(censored.freq.comparison)
}
