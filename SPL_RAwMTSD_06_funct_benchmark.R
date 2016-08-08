#### Find codes to be benchmarked ####
cd = getwd()
cd.new = paste0(cd, "/benchm")
setwd(cd.new)

#### Setting number of code runs ####
t = 20

#### Test base::for
time_test_funct = summary(microbenchmark(source("benchm_for.R"), times = t, unit = "s"))

#### Test foreach::foreach with maximum number of processor cores ####
clst = makeCluster(as.numeric(Sys.getenv("NUMBER_OF_PROCESSORS")))
registerDoParallel(clst)

time_test_funct = rbind(time_test_funct, summary(microbenchmark(source("benchm_foreach.R"), times = t, unit = "s")))
time_test_funct$expr = gsub("foreach.R", paste0("foreach_", as.numeric(Sys.getenv("NUMBER_OF_PROCESSORS")), "CPUs.R"), 
    time_test_funct$expr)
stopCluster(clst)

#### Test base::sapply and base::lapply ####
time_test_funct = rbind(time_test_funct, summary(microbenchmark(source("benchm_apply.R"), times = t, unit = "s")))

#### Test foreach::foreach with 1 processor core ####
clst = makeCluster(1)
registerDoParallel(clst)

time_test_funct = rbind(time_test_funct, summary(microbenchmark(source("benchm_foreach.R"), times = t, unit = "s")))
time_test_funct$expr = gsub("foreach.R", "foreach_1CPU.R", time_test_funct$expr)
stopCluster(clst)

#### Format output data.frame ####
time_test_funct$expr = gsub("source(\"benchm_", "", time_test_funct$expr, fixed = TRUE)
time_test_funct$expr = gsub(".R\")", "", time_test_funct$expr, fixed = TRUE)
time_test_funct = time_test_funct[order(time_test_funct$mean), ]
rownames(time_test_funct) = c(1:nrow(time_test_funct))

#### Return to project working directory ####
setwd(cd)
rm(cd.new, cd)
