DF_base <- BasicDF(inputTest, colnames(inputTest)[!grepl(".prev", colnames(inputTest))])

KPSS_level <- data.frame(matrix(vector(), nrow = 0, ncol = 5))
colnames(KPSS_level) <- c("statistic.KPSS.Level", "parameter.Truncation.lag.parameter", "p.value", "method", "var")

for (i in 3:ncol(inputTest)) {
  if (grepl(".prev", colnames(inputTest)[i])) {
    next() 
  }
  KPSS_level[i, ] <- unlist(kpss.test(inputTest[, i], null = "Level"))
  KPSS_level[i, 5] <- colnames(inputTest)[i]
}

KPSS_level <- KPSS_level[!is.na(KPSS_level$var),]

KPSS_level <- within(KPSS_level, {
  statistic.KPSS.Level               <- as.numeric(statistic.KPSS.Level)
  parameter.Truncation.lag.parameter <- as.numeric(parameter.Truncation.lag.parameter)
  p.value                            <- as.numeric(p.value)
}
)

KPSS_trend           <- data.frame(matrix(vector(), nrow = 0, ncol = 5))
colnames(KPSS_trend) <- c("statistic.KPSS.Trend", "parameter.Truncation.lag.parameter", "p.value", "method", "var")

for (i in 3:ncol(inputTest)){
  if (grepl(".prev", colnames(inputTest)[i])) {
    next() 
  }
  KPSS_trend[i, ]  <- unlist(kpss.test(inputTest[, i], null = "Trend"))
  KPSS_trend[i, 5] <- colnames(inputTest)[i]
}

KPSS_trend <- KPSS_trend[!is.na(KPSS_trend$var),]
KPSS_trend <- within(KPSS_trend, {
  statistic.KPSS.Trend               <- as.numeric(statistic.KPSS.Trend)
  parameter.Truncation.lag.parameter <- as.numeric(parameter.Truncation.lag.parameter)
  p.value                            <- as.numeric(p.value)
}
)

# save.image(file = "curr_env.rda")
# load("curr_env.rda")

### adf.test from tseries generates a list with the results.
### Create a meta list that gathers all results for different lagvalues
### ?Check for different alternatives (stationary/explosive)
### a <- adf.test(input$USGDP...D, alternative = "s", k = 20)

ADF_stationary <- data.frame(matrix(vector(), nrow = 0, ncol = 6))

for (i in 3:length(inputTest)) {
  if (grepl(".prev", colnames(inputTest)[i])) {
    next() 
  }
  temp <- adf.test(inputTest[, i], alternative = "s", k = 1L)
  temp <- t(unlist(temp))
  temp[, 6] <- colnames(inputTest)[i]
  ADF_stationary <- rbind (ADF_stationary, temp)
  
}

colnames(ADF_stationary) <- c("DFstat", "lag", "alternative", "pvalue", "method", "data")

ADF_stationary <- within(ADF_stationary, {
  DFstat <- as.numeric(as.character(DFstat))
  pvalue <- as.numeric(as.character(pvalue))
}
)

plot(ADF_stationary$lag, ADF_stationary$DFstat)
plot(ADF_stationary$lag, ADF_stationary$pvalue)

ADF_explosive <- data.frame(matrix(vector(), nrow = 0, ncol = 6))

for (i in 3:ncol(inputTest)){
  if (grepl(".prev", colnames(inputTest)[i])) {
    next() 
  }
  temp <- adf.test(inputTest[, i], alternative = "e", k = 1L)
  temp <- t(unlist(temp))
  temp[, 6] <- colnames(inputTest)[i]
  ADF_explosive <- rbind (ADF_explosive, temp)
  
}


colnames(ADF_explosive) <- c("DFstat", "lag", "alternative", "pvalue", "method", "data")

ADF_explosive <- within(ADF_explosive, {
  DFstat <- as.numeric(as.character(DFstat))
  pvalue <- as.numeric(as.character(pvalue))
})

plot(ADF_explosive$lag, ADF_explosive$DFstat)
plot(ADF_explosive$lag, ADF_explosive$pvalue)

rm(temp, i)