### Generate first differences
time      <- input[, timeCols]
notTime   <- input[, !colnames(input) %in% timeCols]
inputTest <- cbind(time, notTime)
fDcols    <- colnames(notTime)
rm(time, notTime)

for (i in 1:length(fDcols)) {
  colnames(inputTest)[colnames(inputTest)==fDcols[i]] <- paste0(fDcols[i], ".now")
  fDcols[i] <- paste0(fDcols[i], ".now")
}

foreach (j = 1:length(fDcols)) %do% {
  
  k <- ncol(inputTest) + 1
  inputTest[, k] <- 0
  
  for (i in 2:nrow(inputTest)) {
    inputTest[i, k] <- inputTest[i, fDcols[j]] - inputTest[i-1, fDcols[j]]
  }
  
  colnames(inputTest)[k] <- gsub(".now", ".fd", fDcols[j])

}

foreach (j = 1:length(fDcols)) %do% {
  
  k <- ncol(inputTest)+1
  inputTest[, k] <- 0
  
  for (i in 2:nrow(inputTest)) {
    inputTest[i, k] <- inputTest[i-1, fDcols[j]]
  }
  
  colnames(inputTest)[k] <- gsub(".now", ".prev", fDcols[j])
  
}

### Rearrange columns 
colArrangement <- c(colnames(inputTest)[1:2])

for (i in 1:length(fDcols)) {
  colArrangement <- c(colArrangement, 
                      fDcols[i], 
                      gsub(".now", ".fd", fDcols[i]), 
                      gsub(".now", ".prev", fDcols[i]))  
}

inputTest <- inputTest[,colArrangement]
rm(colArrangement)
