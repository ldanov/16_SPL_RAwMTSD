### Load packages from vector, if not in installed packages, install.
PrepPackages = function(requiredLibs) {
    notInstalled = requiredLibs[!(requiredLibs %in% installed.packages()[, "Package"])]
    if (length(notInstalled)) 
        install.packages(notInstalled, dependencies = TRUE)
    suppressWarnings(suppressMessages(invisible(lapply(requiredLibs, require, character.only = T))))
    print("All packages loaded successfully")
}
