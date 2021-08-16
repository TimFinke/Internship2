# libraries & functions
source("./Script dependencies.R")

# data
load("./data/data_report.Rdata")
load("./data/cormatrix.Rdata")

# make supplementary data
if(!dir.exists(paste0("./data/SuppData/Descriptives/"))){
  dir.create(paste0("./data/SuppData/Descriptives/"))
}
  
list_descriptives <- list("descriptives NTR" = descriptives_NTR,
                          "descriptives NESDA" = descriptives_NESDA,
                          "sex education NTR" = desc_extra$sex_education_NTR,
                          "sex education NESDA" = desc_extra$sex_education_NESDA,
                          "depstatus education NTR" = desc_extra$sex_status_NTR,
                          "depstatus education NESDA" = desc_extra$sex_status_NESDA,
                          "smoke status education NTR" = desc_extra$sex_currentsmoke_NTR,
                          "smoke status education NESDA" = desc_extra$sex_currentsmoke_NESDA,
                          "corrmatrix blood cell counts" = cormatrix)
write.xlsx(list_descriptives, file = paste0("./data/SuppData/Descriptives/Descriptives.xlsx"),
             col.names = TRUE, row.names = FALSE,
             overwrite = TRUE)
  