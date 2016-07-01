cd <- getwd()
cd.new <- paste0(cd,"/source")
setwd(cd.new)

function.sources <- list.files(getwd(), pattern = "\\.r$", ignore.case = TRUE)
for(i in 1:length(function.sources)) {
  source(function.sources[i])
}

setwd(cd)
rm(cd, cd.new, i, function.sources)
