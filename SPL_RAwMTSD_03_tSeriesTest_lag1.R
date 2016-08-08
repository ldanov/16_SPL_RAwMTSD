#### Dickey-Fuller Test ####
DF_base = basic.DF(input, colnames(input)[3:10], meth = "All")
# DF_base_old = basic.DF.old(input, colnames(input)[3:10])

#### Augmented Dickey-Fuller Test ####
ADF_stationary = data.frame(matrix(vector(), nrow = 0, ncol = 6))
ADF_explosive = ADF_stationary

### Generate Augmented Dickey-Fuller Test statistic with H1: stationarity and lag 1
for (i in 3:length(input)) {
    if (grepl(".prev", colnames(input)[i])) {
        (next)()
    }
    temp = adf.test(input[, i], alternative = "s", k = 1L)
    temp = t(unlist(temp))
    temp[, 6] = colnames(input)[i]
    ADF_stationary = rbind(ADF_stationary, temp)
    
}

ADF_stationary = ADF_stationary[, -5]
colnames(ADF_stationary) = c("ADFstat", "lag", "alternative", "pvalue", "var")

ADF_stationary = within(ADF_stationary, {
    ADFstat = as.numeric(as.character(ADFstat))
    lag = as.numeric(as.character(lag))
    pvalue = as.numeric(as.character(pvalue))
})



### Generate Augmented Dickey-Fuller Test statistic with H1: explosive and lag 1
for (i in 3:ncol(input)) {
    if (grepl(".prev", colnames(input)[i])) {
        (next)()
    }
    temp = adf.test(input[, i], alternative = "e", k = 1L)
    temp = t(unlist(temp))
    temp[, 6] = colnames(input)[i]
    ADF_explosive = rbind(ADF_explosive, temp)
    
}

ADF_explosive = ADF_explosive[, -5]
colnames(ADF_explosive) = c("ADFstat", "lag", "alternative", "pvalue", "var")

ADF_explosive = within(ADF_explosive, {
    ADFstat = as.numeric(as.character(ADFstat))
    lag = as.numeric(as.character(lag))
    pvalue = as.numeric(as.character(pvalue))
})

#### Kwiatkowski–Phillips–Schmidt–Shin (KPSS) Test ####
print(paste0("KPSS lag paratmeter is set to trunc(3*sqrt(n)/13) = ", trunc(3 * sqrt(nrow(input))/13)))
KPSS_level = data.frame(matrix(vector(), nrow = 0, ncol = 5))
KPSS_trend = KPSS_level


### Generate KPSS Test statistic with H0: level stationarity and lag 1
for (i in 3:ncol(input)) {
    if (grepl(".prev", colnames(input)[i])) {
        (next)()
    }
    KPSS_level[i, ] = unlist(kpss.test(input[, i], null = "Level"))
    KPSS_level[i, 5] = colnames(input)[i]
}

colnames(KPSS_level) = c("KPSSstat", "lag", "pvalue", "null", "var")
KPSS_level$null = gsub("KPSS Test for", "", KPSS_level$null)

KPSS_level = within(KPSS_level, {
    KPSSstat = as.numeric(KPSSstat)
    lag = as.numeric(lag)
    p.value = as.numeric(pvalue)
})

### Generate KPSS Test statistic with H0: trend stationarity and lag 1
for (i in 3:ncol(input)) {
    if (grepl(".prev", colnames(input)[i])) {
        (next)()
    }
    KPSS_trend[i, ] = unlist(kpss.test(input[, i], null = "Trend"))
    KPSS_trend[i, 5] = colnames(input)[i]
}

colnames(KPSS_trend) = c("KPSSstat", "lag", "pvalue", "null", "var")
KPSS_trend$null = gsub("KPSS Test for", "", KPSS_trend$null)

KPSS_trend = within(KPSS_trend, {
    KPSSstat = as.numeric(KPSSstat)
    lag = as.numeric(lag)
    p.value = as.numeric(pvalue)
})
