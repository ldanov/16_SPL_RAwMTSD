BasicDF <- function(data, fDcols=colnames(data)){
  
  library(foreach)
  ### Generate 3 variants of DF test 
  DF_values <- data.frame(matrix(vector(), nrow = 1, ncol = 5), stringsAsFactors = FALSE)
  colnames(DF_values) <- c("varName", "test.level", "test.stat", "tval.test.stat", "pval.regr")
  
  foreach (i = 1:length(fDcols) ) %do% {
    y.now  <- data[, fDcols[i]]
    y.diff <- rep(0, length(y.now))
    
    for (j in 2:length(y.now)) {
      y.diff[j] <- y.now[j] - y.now[j-1]
    }
    

  
  y.prev    <- y.now[-length(y.now)]
  y.diff    <- y.diff[-1]
  timeTrend <- c(1:(length(y.prev)))
  ur        <- lm(y.diff ~ y.prev -1)
  ur.dr     <- lm(y.diff ~ y.prev)
  ur.dr.tt  <- lm(y.diff ~ y.prev + timeTrend)
  
  out.ur    <- c(fDcols[i], 
                 "UR.base", 
                 summary(ur)$coefficients[1],
                 summary(ur)$coefficients[3],
                 pf(summary(ur)$fstatistic[1], 
                    summary(ur)$fstatistic[2], 
                    summary(ur)$fstatistic[3], 
                    lower.tail = F))
  out.ur2   <- c(fDcols[i], 
                 "UR.drift",
                 summary(ur.dr)$coefficients[2],
                 summary(ur.dr)$coefficients[6],
                 pf(summary(ur.dr)$fstatistic[1], 
                    summary(ur.dr)$fstatistic[2], 
                    summary(ur.dr)$fstatistic[3], 
                    lower.tail = F))
  out.ur3   <- c(fDcols[i], 
                 "UR.drift.tTrend", 
                 summary(ur.dr.tt)$coefficients[2],
                 summary(ur.dr.tt)$coefficients[8],
                 pf(summary(ur.dr.tt)$fstatistic[1], 
                    summary(ur.dr.tt)$fstatistic[2], 
                    summary(ur.dr.tt)$fstatistic[3], 
                    lower.tail = F))
  
  
  # temp <- list(ur, ur.dd, ur.dd.tt)
  # names(temp) <- c(paste0(fDcols[i], "_DF_ur"), paste0(fDcols[i], "_DF_urdr"), paste0(fDcols[i], "_DF_urdrtt"))
  # assign(paste0("DF_", fDcols[i]), temp)
  
  
  DF_values <- rbind(DF_values, out.ur, out.ur2, out.ur3)
  
}


DF_values <- DF_values[-1, ]
return(DF_values)
}