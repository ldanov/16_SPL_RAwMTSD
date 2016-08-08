#### Define column indices of time variables (to be excluded from tests)
timeIndices = c()

#### Register Cluster
clst = makeCluster(as.numeric(Sys.getenv("NUMBER_OF_PROCESSORS")))
registerDoParallel(clst)

timeIndices = foreach(n = 1:length(timeCols), .combine = c) %dopar% {
    temp = grep(timeCols[n], colnames(input), ignore.case = TRUE)
}

notTimeCols = c(1:ncol(input))[-timeIndices]

lagLevels = c(seq(1, 21, 1))

#### Generate ADF dataset with lags from 1 to 21 #### k=22 gives an error for not enough data ###

ADF_multiLag_foreach = foreach(z = 1:2, .combine = rbind) %:% foreach(i = notTimeCols, .combine = rbind) %:% foreach(j = lagLevels, 
    .packages = "tseries", .combine = rbind) %dopar% {
    data.frame(t(unlist(adf.test(input[, i], alternative = c("s", "e")[z], k = j))), colnames(input)[i], stringsAsFactors = FALSE)
}


ADF_multiLag_foreach = ADF_multiLag_foreach[, -c(5:6)]
colnames(ADF_multiLag_foreach) = c("ADFstat", "lag", "alternative", "pvalue", "var")

ADF_multiLag_foreach = within(ADF_multiLag_foreach, {
    ADFstat = as.numeric(as.character(ADFstat))
    lag = as.numeric(as.character(lag))
    pvalue = as.numeric(as.character(pvalue))
})

#### Generate KPSS dataset with tseries function ####

KPSS_multiLag_foreach = foreach(k = 1:2, .combine = rbind) %:% foreach(i = notTimeCols, .combine = rbind) %dopar% 
    {
        temp = data.frame(t(unname(unlist(kpss.test(input[, i], null = c("Level", "Trend")[k], lshort = FALSE)))), 
            colnames(input)[i], stringsAsFactors = FALSE)
    }
KPSS_multiLag_foreach = KPSS_multiLag_foreach[, -5]
colnames(KPSS_multiLag_foreach) = c("KPSSstat", "lag", "pvalue", "null", "var")
KPSS_multiLag_foreach$null = gsub("KPSS Test for", "", KPSS_multiLag_foreach$null)

#### Generate KPSS dataset with customized tseries function #### Changed returned object from list to vector
#### Removed automatic choice of lag-parameter
kpss.test.lag = function(x, null = c("Level", "Trend"), k = 1) {
    if ((NCOL(x) > 1) || is.data.frame(x)) 
        stop("x is not a vector or univariate time series")
    DNAME = deparse(substitute(x))
    null = match.arg(null)
    x = as.vector(x, mode = "double")
    n = length(x)
    if (null == "Trend") {
        t = 1:n
        e = residuals(lm(x ~ t))
        table = c(0.216, 0.176, 0.146, 0.119)
    } else if (null == "Level") {
        e = residuals(lm(x ~ 1))
        table = c(0.739, 0.574, 0.463, 0.347)
    }
    tablep = c(0.01, 0.025, 0.05, 0.1)
    s = cumsum(e)
    eta = sum(s^2)/(n^2)
    s2 = sum(e^2)/n
    l = k
    
    s2 = .C("R_pp_sum", as.vector(e, mode = "double"), as.integer(n), as.integer(l), s2 = as.double(s2), PACKAGE = "tseries")$s2
    STAT = eta/s2
    PVAL = approx(table, tablep, STAT, rule = 2)$y
    if (is.na(approx(table, tablep, STAT, rule = 1)$y)) 
        if (PVAL == min(tablep)) 
            warning("p-value smaller than printed p-value") else warning("p-value greater than printed p-value")
    data.frame(KPSSstat = STAT, lag = l, pvalue = PVAL, null = paste(null, "Stationarity"), var = DNAME, stringsAsFactors = FALSE)
}

KPSS_v2_multiLag_foreach = foreach(k = 1:2, .combine = rbind) %:% foreach(i = notTimeCols, .combine = rbind) %:% 
    foreach(j = lagLevels, .combine = rbind) %dopar% {
    data.frame(kpss.test.lag(input[, i], null = c("Level", "Trend")[k], k = j), colnames(input)[i], stringsAsFactors = FALSE)
}

KPSS_v2_multiLag_foreach = KPSS_v2_multiLag_foreach[, -5]
colnames(KPSS_v2_multiLag_foreach) = c("KPSSstat", "lag", "pvalue", "null", "var")
stopCluster(clst)
