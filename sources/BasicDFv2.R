basic.DF = function(data, fDcols = colnames(data), meth = c("All", "Base", "Drift", "Complete")) {
    
    DF_values = data.frame(matrix(vector(), nrow = 0, ncol = 6, dimnames = list(NULL, c("varName", "test.level", 
        "test.stat", "tval.test.stat", "pval.coeff", "pval.regr"))), stringsAsFactors = FALSE)
    
    chosenSettings = data.frame(matrix(c(0), nrow = 1, ncol = 3, dimnames = list(NULL, c("Base", "Drift", "Complete"))), 
        stringsAsFactors = FALSE)
    
    if (TRUE %in% grepl("All", meth, ignore.case = TRUE)) {
        chosenSettings[1, ] = 1
    } else {
        for (k in 1:ncol(chosenSettings)) {
            if (TRUE %in% grepl(colnames(chosenSettings)[k], meth, ignore.case = TRUE)) {
                chosenSettings[1, colnames(chosenSettings)[k]] = 1
            }
        }
    }
    
    
    for (i in 1:length(fDcols)) {
        
        if (length(fDcols) == 1) {
            y.now = as.numeric(data)
        } else {
            y.now = data[, fDcols[i]]
        }
        
        y.diff = diff(y.now)
        y.prev = y.now[-length(y.now)]
        timeTrend = c(1:(length(y.prev)))
        
        if (chosenSettings[1, 1] == 1) {
            ur = lm(y.diff ~ y.prev - 1)
            out.ur1 = data.frame(fDcols[i], "UR.base", summary(ur)$coefficients[1], summary(ur)$coefficients[3], 
                summary(ur)$coefficients[4], pf(summary(ur)$fstatistic[1], summary(ur)$fstatistic[2], summary(ur)$fstatistic[3], 
                  lower.tail = F), stringsAsFactors = FALSE)
            colnames(out.ur1) = c("varName", "test.level", "test.stat", "tval.test.stat", "pval.coeff", "pval.regr")
        }
        
        if (chosenSettings[1, 2] == 1) {
            ur.dr = lm(y.diff ~ y.prev)
            out.ur2 = data.frame(fDcols[i], "UR.drift", summary(ur.dr)$coefficients[2], summary(ur.dr)$coefficients[6], 
                summary(ur.dr)$coefficients[8], pf(summary(ur.dr)$fstatistic[1], summary(ur.dr)$fstatistic[2], summary(ur.dr)$fstatistic[3], 
                  lower.tail = F), stringsAsFactors = FALSE)
            colnames(out.ur2) = c("varName", "test.level", "test.stat", "tval.test.stat", "pval.coeff", "pval.regr")
        }
        
        if (chosenSettings[1, 3] == 1) {
            ur.dr.tt = lm(y.diff ~ y.prev + timeTrend)
            out.ur3 = data.frame(fDcols[i], "UR.drift.tTrend", summary(ur.dr.tt)$coefficients[2], summary(ur.dr.tt)$coefficients[8], 
                summary(ur.dr.tt)$coefficients[11], pf(summary(ur.dr.tt)$fstatistic[1], summary(ur.dr.tt)$fstatistic[2], 
                  summary(ur.dr.tt)$fstatistic[3], lower.tail = F), stringsAsFactors = FALSE)
            colnames(out.ur3) = c("varName", "test.level", "test.stat", "tval.test.stat", "pval.coeff", "pval.regr")
        }
        
        DF_values = rbind(DF_values, 
                          if (exists("out.ur1")) {
                            out.ur1
                            }, 
                          if (exists("out.ur2")) {
                            out.ur2
                            }, 
                          if (exists("out.ur3")) {
                            out.ur3
                            }
                          )
        
    }
    
    return(DF_values)
}
