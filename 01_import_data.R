#### Load libraries ####

### Function PrepPackages
### Load needed libraries from a vector
### If not in installed.packages, install them
PrepPackages(c("tseries", "foreach"))

#### Import ####

### Import dataset
if (!exists("input")) {
  input <- read.csv("SPL_data_v2.csv", header = TRUE, stringsAsFactors = FALSE) 
  print("input")
  str(input)
  input$timeIndex <- c(1:nrow(input))
  
} else {
  print("input")
  str(input)
}

#### Dataset check ####

### Function ColsSame
### Checks if the first column in a vector of column indexes
### is equal to any of the others
### Was developed to keep track of multiple time units columns (quarters, years)
### Returns a list with vectors of column indexes - same and differing

### Example use
# exa <- cbind(c(1:25), c(1:25), c(1:25), c(25:1), c(11:35))
# colnames(exa) <- c("one", "two", "three", "four", "five")
# ColsSame(exa)
# ColsSame(exa, c(3:5, 1, 2))
# rm(exa)

if (!is.null(unlist(ColsSame(input)["colsSame"]))) {
  toRem <- unlist(ColsSame(input)["colsSame"])
  input <- input[,-toRem]
}

### Function ConvertNumeric
### Replaces thousand and decimal separator with R defaults
### Converts vector to numeric
### Suffix to transformed colnames adjustable
### Developed to be able to transform R objects after import

### Example use
# test <- read.delim(paste0(getwd(), "/demo/test_for_num_function.csv"), sep = ";", stringsAsFactors = F)
# str(test)
# test1 <- ConvertNumeric(test, thSep = "_", decSep = ",", colSuffix = "")
# str(test1)
# rm(test, test1)
