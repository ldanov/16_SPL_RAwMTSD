if (!"tseries" %in% installed.packages()) install.packages("tseries", dependencies = TRUE)
library(tseries)

### adf.test from tseries generates a list with the results.
### Create a meta list that gathers all results for different lagvalues 
### ?Check for different alternatives (stationary/explosive)
### a <- adf.test(input$USGDP...D, alternative = "s", k = 20)
resultsStationary <- data.frame(matrix(vector(), nrow = 0, ncol = 6))
for (i in 2:5){
  for (j in 1:ceiling(.9*nrow(input))) {
    temp <- adf.test(input[,i], alternative = "s", k=j)
    temp <- t(unlist(temp))
    resultsStationary <- rbind (resultsStationary, temp) 
  }
}
rm(temp, i ,j)
colnames(resultsStationary)<- c("DFstat", "lag", "alternative", "pvalue", "method", "data")
resultsStationary <- within(resultsStationary, {
  DFstat <- as.numeric(as.character(DFstat))
  pvalue <- as.numeric(as.character(pvalue))
})
plot(resultsStationary$lag, resultsStationary$DFstat)
plot(resultsStationary$lag, resultsStationary$pvalue)

resultsExplosive <- data.frame(matrix(vector(), nrow = 0, ncol = 6))
for (i in 2:5){
  for (j in 1:ceiling(.9*nrow(input))) {
    temp <- adf.test(input[,i], alternative = "e", k=j)
    temp <- t(unlist(temp))
    resultsExplosive <- rbind (resultsExplosive, temp) 
  }
}
rm(temp, i ,j)
colnames(resultsExplosive)<- c("DFstat", "lag", "alternative", "pvalue", "method", "data")
resultsExplosive <- within(resultsExplosive, {
  DFstat <- as.numeric(as.character(DFstat))
  pvalue <- as.numeric(as.character(pvalue))
})

plot(resultsExplosive$lag, resultsExplosive$DFstat)
plot(resultsExplosive$lag, resultsExplosive$pvalue)
