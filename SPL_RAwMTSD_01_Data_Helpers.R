#### Clear Global Environment ####
rm(list = ls())

#### Load functions ####
cd = getwd()
cd.new = paste0(cd, "/sources")
setwd(cd.new)

function.sources = list.files(getwd(), pattern = "\\.r$", ignore.case = TRUE)
for (i in 1:length(function.sources)) {
    source(function.sources[i])
}

setwd(cd)
rm(cd, cd.new, i, function.sources)

#### Load libraries ####

### Function PrepPackages Load needed libraries from a vector If not in installed.packages, install them xtable
### for df to latex table, stargazer for lm results
PrepPackages(c("tseries", "foreach", "doParallel", "microbenchmark", "ggplot2", "stargazer"))

#### Import ####

### Import dataset
if (!exists("input")) {
    input = read.csv("SPL_RAwMTSD_data_v2.csv", header = TRUE, stringsAsFactors = FALSE)
    print("input")
    str(input)
    input$timeIndex = c(1:nrow(input))
    
} else {
    print("input")
    str(input)
}

#### Dataset check ####

### Function ColsSame Checks if the first column in a vector of column indexes is equal to any of the others Was
### developed to keep track of multiple time units columns (quarters, years) Returns a list with vectors of column
### indexes - same and differing

### Example use exa = cbind(c(1:25), c(1:25), c(1:25), c(25:1), c(11:35)) colnames(exa) = c('one', 'two',
### 'three', 'four', 'five') ColsSame(exa) ColsSame(exa, c(3:5, 1, 2)) rm(exa)

if (!is.null(unlist(ColsSame(input)["colsSame"]))) {
    toRem = unlist(ColsSame(input)["colsSame"])
    input = input[, -toRem]
}

### Function ConvertNumeric Replaces thousand and decimal separator with R defaults Converts vector to numeric
### Suffix to transformed colnames adjustable Developed to be able to transform R objects after import

### Example use test = read.delim(paste0(getwd(), '/demo/test_for_num_function.csv'), sep = ';', stringsAsFactors
### = F) str(test) test1 = ConvertNumeric(test, thSep = '_', decSep = ',', colSuffix = '') str(test1) rm(test,
### test1)

#### Define time variable and indices column names
timeCols = c("Year", "timeIndex")

#### Generate first differences ####
fDcols = colnames(input)[!colnames(input) %in% timeCols]

for (i in 1:length(fDcols)) {
    colnames(input)[colnames(input) == fDcols[i]] = paste0(fDcols[i], ".now")
    fDcols[i] = paste0(fDcols[i], ".now")
}

foreach(j = 1:length(fDcols)) %do% {
    
    k = ncol(input) + 1
    input[, k] = 0
    
    for (i in 2:nrow(input)) {
        input[i, k] = input[i, fDcols[j]] - input[i - 1, fDcols[j]]
    }
    
    colnames(input)[k] = gsub(".now", ".fd", fDcols[j])
    
}

#### Rearrange columns ####
colArrangement = timeCols

for (i in 1:length(fDcols)) {
    colArrangement = c(colArrangement, fDcols[i], gsub(".now", ".fd", fDcols[i]))
}

input = input[, colArrangement]
rm(colArrangement, i, j, k)
