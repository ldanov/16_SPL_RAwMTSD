#### Define column indices of time variables (to be excluded from tests)
timeIndices = sapply(1:length(timeCols), function(x) grep(timeCols[x], colnames(input), ignore.case = TRUE))
notTimeCols = c(1:ncol(input))[-timeIndices]

lagLevels = c(seq(1, 21, 1))

#### Generate ADF dataset with lags from 1 to 21 #### k=22 gives an error for not enough data ###


ADF_temp_1 = lapply(notTimeCols, function(x) {
    data.frame(t(sapply(lagLevels, function(y) c(t(unlist(adf.test(input[, x], alternative = "s", k = y))), colnames(input)[x]))), 
        stringsAsFactors = FALSE)
})
ADF_temp_1 = do.call(rbind, ADF_temp_1)
ADF_temp_2 = lapply(notTimeCols, function(x) {
    data.frame(t(sapply(lagLevels, function(y) c(t(unlist(adf.test(input[, x], alternative = "e", k = y))), colnames(input)[x]))), 
        stringsAsFactors = FALSE)
})
ADF_temp_2 = do.call(rbind, ADF_temp_2)
ADF_multiLag_apply = rbind(ADF_temp_1, ADF_temp_2)


ADF_multiLag_apply = ADF_multiLag_apply[, -c(5:6)]
colnames(ADF_multiLag_apply) = c("ADFstat", "lag", "alternative", "pvalue", "var")

ADF_multiLag_apply = within(ADF_multiLag_apply, {
    ADFstat = as.numeric(as.character(ADFstat))
    lag = as.numeric(as.character(lag))
    pvalue = as.numeric(as.character(pvalue))
})
rm(ADF_temp_1, ADF_temp_2)
#### Generate KPSS dataset with tseries function ####

KPSS_multiLag_apply = lapply(1:2, function(y) {
    data.frame(t(sapply(notTimeCols, function(x) c(t(unlist(kpss.test(input[, x], null = c("Level", "Trend")[y], 
        lshort = FALSE))), colnames(input)[x]))), stringsAsFactors = FALSE)
})
KPSS_multiLag_apply = do.call(rbind, KPSS_multiLag_apply)
KPSS_multiLag_apply = KPSS_multiLag_apply[, -5]
colnames(KPSS_multiLag_apply) = c("KPSSstat", "lag", "pvalue", "null", "var")
KPSS_multiLag_apply$null = gsub("KPSS Test for", "", KPSS_multiLag_apply$null)

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


KPSS_temp_1 = lapply(notTimeCols, function(x) {
    data.frame(t(sapply(lagLevels, function(y) c(kpss.test.lag(input[, x], null = "Trend", k = y)))), var2 = colnames(input)[x], 
        stringsAsFactors = FALSE)
})
KPSS_temp_1 = do.call(rbind, KPSS_temp_1)

KPSS_temp_2 = lapply(notTimeCols, function(x) {
    data.frame(t(sapply(lagLevels, function(y) c(kpss.test.lag(input[, x], null = "Level", k = y)))), var2 = colnames(input)[x], 
        stringsAsFactors = FALSE)
})
KPSS_temp_2 = do.call(rbind, KPSS_temp_2)

KPSS_v2_multiLag_apply = data.frame(rbind(KPSS_temp_1, KPSS_temp_2), stringsAsFactors = FALSE)
KPSS_v2_multiLag_apply = KPSS_v2_multiLag_apply[, -5]
colnames(KPSS_v2_multiLag_apply)[5] = "var"
rm(KPSS_temp_1, KPSS_temp_2)
