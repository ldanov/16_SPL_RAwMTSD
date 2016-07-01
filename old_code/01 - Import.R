#### Import ####

### Import dataset
input <- read.csv("SPL_data_to_use.csv", header = TRUE, stringsAsFactors = FALSE)
str(input)

#### Dataset check ####

### Check if Quarters are same on each row. If it is remove unnecessary columns
if (isTRUE(all.equal(input$Code1, input$Code2)) &
    isTRUE(all.equal(input$Code1, input$Code3)) &
    isTRUE(all.equal(input$Code1, input$Code4))
    )
{
  print(paste0("All quarter identifiers are equal"))
  input <- subset(input, select = -c(Code2, Code3, Code4))
} else {
  print(paste0("Not all quarter identifiers are equal. Edit needed"))
}

### Check if numeric columns are interpreted as numeric, if not convert them
### Try to convert each column, whose name does not begin with Code (case insensitive),
### into a numeric one. If successful keep both, but return a vector of all successes;
### if unsuccessful remove attempt (as it will most likely yield an NA vector)
colCheckInd <- 1:ncol(input)
colCheckInd <- colCheckInd[!colCheckInd %in% grep("Code", colnames(input), ignore.case = TRUE)]
origCols <- 0

for (i in colCheckInd) {
  if (class(input[,i]) != "numeric")
  {
    print(paste0("Class of column ", colnames(input)[i], " is not nummeric. Will try to convert"))
    input[,ncol(input)+1] <- as.numeric(input[,i])
    colnames(input)[ncol(input)]<- paste0("num_",colnames(input)[i])
    ifelse(is.na(input[ncol(input)]), 
           print(paste0("Converting column ",colnames(input)[i]," failed")),
           print(paste0("Column ",colnames(input)[i]," converted to numeric in column ", colnames(input)[ncol(input)])))
    temp <- i; origCols <- c(origCols, temp)
  } 
}; rm (i, colCheckInd, temp); origCols <- origCols[-1]

if(length(origCols)==0) {rm(origCols)
  } else print(paste0("Columns that were attempted to convert: ", toString(origCols)))

