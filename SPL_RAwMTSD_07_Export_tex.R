#### Set export directory ####
cd = getwd()
cd.new = paste0(cd, "/tex")
if (!dir.exists(paste0(getwd(), "/tex"))) {
    print("Creating folder '/tex'")
    dir.create(cd.new)
} else {
    print("Folder '/tex' exists. Careful to not overwrite existing text and tex files")
}

setwd(cd.new)

partDF = cbind(DF_base[DF_base$test.level == "UR.drift.tTrend", c(1, 5)], ADF_stationary[, c(4, 5)])
partDF$var = as.character(partDF$var)

if (identical(partDF[, 1], partDF[, 4])) {
    print("Variables match")
    partDF = partDF[, -4]
} else {
    print("Variable names do not match")
}

colnames(partDF) = c("varName", "DF.p-value", "ADF.p-value")

partDF2 = cbind(DF_base[DF_base$test.level == "UR.drift.tTrend", c(1, 3)], ADF_stationary[, c(1, 5)])
partDF2$var = as.character(partDF2$var)

if (identical(partDF2[, 1], partDF2[, 4])) {
    print("Variables match")
    partDF2 = partDF2[, -4]
} else {
    print("Variable names do not match")
}

colnames(partDF2) = c("varName", "DF.t-value", "ADF.t-value")

stargazer(partDF, summary = FALSE, align = TRUE, rownames = FALSE, out = "df_pval_comp.txt")
stargazer(partDF, summary = FALSE, align = TRUE, rownames = FALSE, out = "df_pval_comp.tex")

stargazer(partDF2, summary = FALSE, align = TRUE, rownames = FALSE, out = "df_tval_comp.txt")
stargazer(partDF2, summary = FALSE, align = TRUE, rownames = FALSE, out = "df_tval_comp.tex")

stargazer(reg.now, reg.fd, align = TRUE, out = "reg_table.txt")
stargazer(reg.now, reg.fd, align = TRUE, out = "reg_table.tex")

stargazer(time_test_funct, align = TRUE, summary = FALSE, rownames = FALSE, out = "benchm_results.txt")
stargazer(time_test_funct, align = TRUE, summary = FALSE, rownames = FALSE, out = "benchm_results.tex")

stargazer(DF_base, summary = FALSE, align = TRUE, rownames = FALSE, out = "df_pval_comp.txt")
stargazer(DF_base, summary = FALSE, align = TRUE, rownames = FALSE, out = "df_pval_comp.tex")

multiLagsADF = ADF_multiLag[ADF_multiLag$var %in% c("GDP.now", "Gini.Coefficient.now") & ADF_multiLag$alternative == 
    "stationary", ]
multiLagsKPSS = KPSS_multiLag_v2[KPSS_multiLag_v2$var %in% c("GDP.now", "Gini.Coefficient.now"), ]

export.envir = c("ADF_explosive", "ADF_stationary", "KPSS_level", "KPSS_trend", "multiLagsADF", "multiLagsKPSS")

for (k in 1:length(export.envir)) {
    arrangeCols = c("var", "lag", "pvalue", ifelse(grepl("ADF", export.envir[k]), "ADFstat", "KPSSstat"), ifelse(grepl("ADF", 
        export.envir[k]), "alternative", "null"))
    temp = eval(parse(text = export.envir[k]), envir = .GlobalEnv)
    temp = temp[, arrangeCols]
    stargazer(temp, align = TRUE, summary = FALSE, rownames = FALSE, out = paste0(export.envir[k], ".txt"))
    stargazer(temp, align = TRUE, summary = FALSE, rownames = FALSE, out = paste0(export.envir[k], ".tex"))
    
}
#### Return to project working directory ####
setwd(cd)
rm(cd.new, cd)
