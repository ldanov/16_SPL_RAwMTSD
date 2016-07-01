### Are two or more columns of a dataframe equal?
### Checks only first column in vector against each of the following
ColsSame <- function(data, cols=c(1:ncol(data))){
  n <- colnames(data)[cols[1]]
  eqT <- NULL
  eqF <- NULL
  for (i in 2:length(cols)) {
    current_check <- colnames(data)[cols[i]]
    if (isTRUE(all.equal(data[,n], data[,cols[i]])) )
    {
      print(paste0("Column ",cols[i] ," - '", colnames(data)[cols[i]],
                   "' is equal to Column 1 - '", colnames(data)[cols[1]],"'"))
      #data <- subset(data, select = -c(Code2, Code3, Code4))
      eqT <- c(eqT, cols[i])
    } else {
      print(paste0("Column ",cols[i] ," - '", colnames(data)[cols[i]],
                   "' differs from Column 1 - '", colnames(data)[cols[1]],"'"))
      eqF <- c(eqF, cols[i])
    }
  }
  eq <- list(eqT, eqF)
  names(eq) <- c("colsSame", "colsDiff")
  return(eq)
}