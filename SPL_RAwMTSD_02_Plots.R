### First plots

### Create plots in specified directory.  If directory does not exist, create it.

if (!dir.exists(paste0(getwd(), "/plots"))) {
    print("Creating folder '/plots'")
    dir.create(paste0(getwd(), "/plots"))
} else {
    print("Folder '/plots' exists. Careful to not overwrite existing plots")
}

clst = makeCluster(as.numeric(Sys.getenv("NUMBER_OF_PROCESSORS")))
registerDoParallel(clst)

foreach(i = 3:ncol(input), .packages = "ggplot2") %dopar% {
    if (!grepl(".prev", colnames(input)[i])) {
        plotPath = file.path(paste0(getwd(), "/plots/"))
        
        temp = ggplot(input, aes(Year)) + geom_line(aes(y = input[, i]), colour = "red", size = 0.5) + scale_x_continuous(breaks = seq(min(input$Year), 
            max(input$Year), by = 5)) + labs(title = paste0(colnames(input[i]), " change ", min(input$Year), "-", 
            max(input$Year)), x = "Year", y = colnames(input[i])) + theme(panel.background = element_rect(fill = "white"), 
            panel.grid.major = element_line(colour = "black", size = 0.15))
        
        ggsave(filename = paste0(colnames(input)[i], ".png"), path = plotPath, plot = temp, width = 5, height = 4, 
            dpi = 400)
        rm(temp)
    }
}

stopCluster(clst)
