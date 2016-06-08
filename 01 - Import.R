#### Import ####

### Import dataset
input <- read.csv("SPL_data_to_use.csv", header = TRUE, stringsAsFactors = FALSE)
str(input)

#### Identifier check ####

### Check if Quarters are same on each row. If it is remove unnecessary columns
if (isTRUE(all.equal(input$Code1, input$Code2)) & 
    isTRUE(all.equal(input$Code1, input$Code3)) &
    isTRUE(all.equal(input$Code1, input$Code4))
    )
{
  print(paste0("All quarter identifiers are equal"))
  input <- subset(input, select = -c(Code2, Code3, Code4))
} else {
  print(paste0("Not all quarter identifiers are equal"))
}

if ()