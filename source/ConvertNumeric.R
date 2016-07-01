ConvertNumeric <- function(data, colCheckInd=c(1:ncol(data)), thSep = "", decSep = ",", colSuffix = "_num"){
  
  st     <- 0
  output <- data.frame(matrix(vector(), nrow(data), 1), stringsAsFactors = F)
  
  for (i in colCheckInd) {
    st <- st + 1
    
    if (!class(data[, i]) %in% c("numeric", "integer")) {
      
      output[, st]         <- gsub(thSep, "", data[, i])
      output[, st]         <- gsub(decSep, ".", output[, st])
      output[, st]         <- as.numeric(output[, st])
      colnames(output)[st] <- paste0(colnames(data)[i], colSuffix)
      
      if (sum(is.na(output[, st])) == nrow(data)) {
        output <- output[, -st]
        print(paste0(
          "Converting Column '", colnames(data)[i], "' failed."))
        st <- st - 1
        
      } else {
        print(paste0(
          "Column '",colnames(data)[i], "' successfully converted to numeric."))
        
      }
      
    } else {
      print(paste0("Column '", colnames(data)[i], "' is already numeric."))
      st <- st - 1
    }
    
    
  } 
  return(output)
}
