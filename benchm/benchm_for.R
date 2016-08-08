#### Define column indices of time variables (to be excluded from tests)
timeIndices = c()
for (n in 1:length(timeCols)) {
    temp = grep(timeCols[n], colnames(input), ignore.case = TRUE)
    timeIndices = c(timeIndices, temp)
}

notTimeCols = c(1:ncol(input))[-timeIndices]

lagLevels = c(seq(1, 21, 1))

#### Generate ADF dataset with lags from 1 to 21 #### k=22 gives an error for not enough data ###
ADF_multiLag_for = data.frame(matrix(vector(), nrow = 0, ncol = 6))

for (i in notTimeCols) {
    for (j in lagLevels) {
        temp = adf.test(input[, i], alternative = "s", k = j)
        temp = t(unlist(temp))
        temp[, 6] = colnames(input)[i]
        ADF_multiLag_for = rbind(ADF_multiLag_for, temp)
        rm(temp)
        temp = adf.test(input[, i], alternative = "e", k = j)
        temp = t(unlist(temp))
        temp[, 6] = colnames(input)[i]
        ADF_multiLag_for = rbind(ADF_multiLag_for, temp)
    }
}

ADF_multiLag_for = ADF_multiLag_for[, -5]
colnames(ADF_multiLag_for) = c("ADFstat", "lag", "alternative", "pvalue", "var")

ADF_multiLag_for = within(ADF_multiLag_for, {
    ADFstat = as.numeric(as.character(ADFstat))
    lag = as.numeric(as.character(lag))
    pvalue = as.numeric(as.character(pvalue))
})

#### Generate KPSS dataset with tseries function ####

KPSS_multiLag_for = data.frame(matrix(vector(), nrow = 0, ncol = 5), stringsAsFactors = FALSE)

for (i in notTimeCols) {
    temp = data.frame(t(unname(unlist(kpss.test(input[, i], null = "Level", lshort = FALSE)))), stringsAsFactors = FALSE)
    temp[1, 5] = colnames(input)[i]
    KPSS_multiLag_for = rbind(KPSS_multiLag_for, temp)
    rm(temp)
    temp = data.frame(t(unname(unlist(kpss.test(input[, i], null = "Trend", lshort = FALSE)))), stringsAsFactors = FALSE)
    temp[1, 5] = colnames(input)[i]
    KPSS_multiLag_for = rbind(KPSS_multiLag_for, temp)
}

colnames(KPSS_multiLag_for) = c("KPSSstat", "lag", "pvalue", "null", "var")
KPSS_multiLag_for$null = gsub("KPSS Test for", "", KPSS_multiLag_for$null)

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

KPSS_v2_multiLag_for = data.frame(matrix(vector(), nrow = 0, ncol = 5), stringsAsFactors = FALSE)

for (i in notTimeCols) {
    for (j in lagLevels) {
        temp = kpss.test.lag(input[, i], null = "Trend", k = j)
        temp[5] = colnames(input)[i]
        KPSS_v2_multiLag_for = rbind(KPSS_v2_multiLag_for, temp)
        rm(temp)
        temp = kpss.test.lag(input[, i], null = "Level", k = j)
        temp[5] = colnames(input)[i]
        KPSS_v2_multiLag_for = rbind(KPSS_v2_multiLag_for, temp)
    }
}
rm(i, j, temp)
