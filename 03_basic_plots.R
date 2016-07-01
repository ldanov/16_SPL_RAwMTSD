### First plots

### Create plots in specified directory. 
### If directory does not exist, create it.

if (!dir.exists(paste0(getwd(), "/plots"))) {
  print("Creating folder '/plots'")
  dir.create(paste0(getwd(), "/plots"))
} else {
  print("Folder '/plots' exists. Careful to not overwrite existing plots")
}



foreach (i = 3:ncol(inputTest)) %do% {
  if (!grepl(".prev", colnames(inputTest)[i])){
    plotPath <- file.path(paste0(getwd(), "/plots/", colnames(inputTest)[i], ".png"))
    png(file = plotPath, width = 640, height = 480)
    plot(x = inputTest$Year, 
         y = inputTest[, i], 
         main=paste0(colnames(inputTest[i]), " change ", min(inputTest$Year), "-", max(inputTest$Year)), 
         xlab = "Year", 
         ylab = colnames(inputTest[i]))
    dev.off()
  }
}