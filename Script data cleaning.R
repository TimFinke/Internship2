### dependencies
source("./Script dependencies NESDA server.R")

### data
load("./data/workspace_2_v2_tim_select.Rdata"); rm(alcohol, fasting)
load("./data/U219_probeset_info_new_v2")
## NTR data
data <- read_sav("./data/2658_TimFinke_20210607.sav") # data incl inflammatory + income
data_IDs <- read_xls("./data/trap reg ext NTR gene expresion.xls")
data_IDs <- cbind.data.frame(data_IDs, FID[group == "NTR"],
                             edu[group == "NTR"],
                             sex[group == "NTR"], age[group == "NTR"], status[group == "NTR"],
                             bmi[group == "NTR"], currentsmoke[group == "NTR"], 
                             D[group == "NTR"], nday_ext_amp[group == "NTR"], ndays[group == "NTR"],
                             hour[group == "NTR"], month[group == "NTR"], year[group == "NTR"], 
                             ht[group == "NTR"], 
                             baso[group == "NTR"], ceo[group == "NTR"], lymp[group == "NTR"],
                             mono[group == "NTR"], neut[group == "NTR"], 
                             highxy[group == "NTR"], lab[group == "NTR"], 
                             sexmm[group == "NTR"], plate[group == "NTR"]
                             )
colnames(data_IDs) <- c("trapreg", "trapext", "FID", "education", "sex",
                        "age", "status", "bmi", "currentsmoke",
                        "D", "nday_ext_amp", "ndays", "hour",
                        "month", "year", "ht", "baso",
                        "ceo", "lymp", "mono", "neut", 
                        "highxy", "lab", "sexmm", "plate")

# create variable date of biobanking NTR
data_IDs$my_dates <- paste0(month[group == "NTR"], "/", rep("1", 3479),"/",  
                            year[group == "NTR"]) %>% 
  as.character() %>%
  anytime()

# NTR data selection
colnames(data)[c(1,2)] <- colnames(data_IDs)[c(1,2)]
data_complete <- left_join(data_IDs, data, by = c("trapreg", "trapext")) # 2 with missing data
data_complete$rownumber <- rownames(data_complete); rm(data)

# create variable selecting date AS_7/AS_8 closest to date biobanking
data_complete$distance_AS_7 <- abs(difftime(data_complete$invd7, data_IDs$my_dates, units = "days"))
data_complete$distance_AS_8 <- abs(difftime(data_complete$invd8, data_IDs$my_dates, units = "days"))
data_complete$distance[data_complete$distance_AS_7 < data_complete$distance_AS_8] <- "AS_7"
data_complete$distance[data_complete$distance_AS_7 > data_complete$distance_AS_8] <- "AS_8"
data_complete$distance[!is.na(data_complete$distance_AS_7) & is.na(data_complete$distance_AS_8)] <- "AS_7"
data_complete$distance[is.na(data_complete$distance_AS_7) & !is.na(data_complete$distance_AS_8)] <- "AS_8"
data_complete$distance[is.na(data_complete$distance_AS_7) & is.na(data_complete$distance_AS_8)] <- "NA"

# create aggregate variables SES
data_complete <- aggregate(data_complete, "hsvl04_7", "hsvl04_8", "agg_hsvl04")
data_complete <- aggregate(data_complete, "income10_7", "income10_8","agg_income_region") # income region
data_complete <- aggregate(data_complete, "isco_mean_7", "isco_mean_8", "agg_income_work_mean") # income work mean
data_complete <- aggregate(data_complete, "isco_median_7", "isco_median_8", "agg_income_work_median") # income work median
data_complete <- aggregate(data_complete, "imp_log_hourly_7", "imp_log_hourly_8", "agg_income_work_log_hourly") # income work log hourly
data_complete <- aggregate(data_complete, "age7", "age8", "age_agg")

# check concordance of variables over the two datasets
plot(data_complete$sex.x, data_complete$sex.y)
plot(data_complete$bmi, data_complete$bmi_bb1_agg)
plot(data_complete$age, data_complete$age_agg)

# remove irrelevant variables
remove <- data_complete[is.na(data_complete$sex.y),]$rownumber %>% as.numeric() 
remove1 <- remove[c(2,3)] # first ppn will be removed automatically later due to incomplete data
remove2 <- remove[1]
data_complete$sex.y <- NULL; colnames(data_complete)[5] <- "sex"

# data Gonneke to encrypt
data_edu <- data_complete[, c("trapreg", "trapext", "education", "educat_c_agg", "educat_a_agg")]
save(data_edu, file = "./data/data_Gonneke.Rdata")

# plot differences educational variables
plot_categorize_education3_4 <- plot(data_complete$education, data_complete$educat_a_agg,
                                     xlab = "Education as 3 category variable", 
                                     ylab = "Education as 4 category variable")
plot_categorize_education3_4_num <- plot(as.numeric(data_complete$education), as.numeric(data_complete$educat_a_agg),
                                         xlab = "Education as 3 category variable", 
                                         ylab = "Education as 4 category variable")
plot_categorize_education3_7 <- plot(data_complete$education, 
                                     data_complete$educat_c_agg,
                                     xlab = "Education as 3 category variable", 
                                     ylab = "Education as 7 category variable")
plot_categorize_education3_7_num <- plot(as.numeric(data_complete$education), 
                                         as.numeric(data_complete$educat_c_agg),
                                         xlab = "Education as 3 category variable", 
                                         ylab = "Education as 7 category variable")
plot_categorize_education4_7 <- plot(data_complete$educat_a_agg,
                                     data_complete$educat_c_agg,
                                     xlab = "Education as 4 category variable",
                                     ylab = "Education as 7 category variable")
# adding missing education data
data_complete <- add_missing_edu(data_complete, "education", "educat_c_agg", "educat_a_agg")

# criteria for removal
remove1 <- c(remove1, data_complete[which(data_complete$D > 5 | data_complete$highxy == "YES" | data_complete$sexmm == "YES"), "rownumber"]) %>% as.numeric()
nrow(data_complete[-c(remove1),])
# Final dataset NTR
NTR <- data_complete[-c(remove1), c(1:24, 57:59, 71, 75, 76, 78, 25)]
rm(data_complete, data_IDs)
N1_A403$Alab_stor
lab
## NESDA data
N1_A053 <- read_sav("./data/N1_A053R.sav") # NESDA ID nr
N1_A100 <- read_sav("./data/N1_A100R.sav") # descriptives, sex, age
N1_A102 <- read_sav("./data/N1_A102R.sav") # work
N1_A106 <- read_sav("./data/N1_A106R.sav") # income
N1_A200D1 <- read_sav("./data/N1_A200D1 (Fagerstrom).sav") # smoking questionnaire
N1_A200D2 <- read_sav("./data/N1_A200D2 (various smoking history variables).sav") # smoking status
N1_A357 <- read_sav("./data/N1_A357D.sav") # bmi
N1_A400 <- read_sav("./data/N1_A400R.sav") # blood withdrawal date etc
N1_A401 <- read_sav("./data/N1_A401R.sav") # CRP (~300), leukocytes (873)
N1_A403 <- read_sav("./data/N1_A403R.sav") # lab at which data stored
N1_A404 <- read_sav("./data/N1_A404R.sav") # CRP, IL6, TNFa
N1_A408 <- read_sav("./data/N1_A408R.sav") # TNFa, same as A404
GECCOGeodata120218 <- read_sav("./data/GECCOGeodata120218.sav") # GECCO SES
GODOT_masterfile_pident <- read_sav("./data/GODOT_masterfile 2981 pident 2011.sav")

# NESDA data selection
colnames(N1_A102)[1] = colnames(N1_A106)[1] = colnames(N1_A200D1)[1] = 
  colnames(N1_A200D2)[1] = colnames(N1_A357)[1] = colnames(N1_A400)[1] = 
  colnames(N1_A401)[1] = colnames(N1_A403)[1] = colnames(N1_A404)[1] = 
  colnames(N1_A408)[1] = colnames(GECCOGeodata120218)[1] = 
  colnames(GODOT_masterfile_pident)[1] = colnames(N1_A100)[1]

NESDA1 <- left_join(N1_A100[, c(1:3, 10)], N1_A106[, c(1, 2)], by = "pident") %>%
  left_join(., GECCOGeodata120218[, c(1, 4)], by = "pident") %>%
  left_join(., N1_A200D2[, c(1, 3)], by = "pident")  %>%
  left_join(., N1_A357, by = "pident") %>%
  left_join(., N1_A404[, c(1:4)], by = "pident") %>%
  left_join(., GODOT_masterfile_pident[, c(1, 2, 19)], by = "pident")

NESDA2 <- cbind.data.frame(FID[group == "NESDA"],
                           edu[group == "NESDA"],
                           sex[group == "NESDA"], age[group == "NESDA"], status[group == "NESDA"],
                           bmi[group == "NESDA"], currentsmoke[group == "NESDA"], 
                           D[group == "NESDA"], nday_ext_amp[group == "NESDA"], ndays[group == "NESDA"],
                           hour[group == "NESDA"], month[group == "NESDA"], year[group == "NESDA"], 
                           ht[group == "NESDA"], 
                           baso[group == "NESDA"], ceo[group == "NESDA"], lymp[group == "NESDA"],
                           mono[group == "NESDA"], neut[group == "NESDA"], 
                           highxy[group == "NESDA"], lab[group == "NESDA"], 
                           sexmm[group == "NESDA"], plate[group == "NESDA"])
colnames(NESDA2) <- c("DNAid", "education", "sex", "age", "status",
                      "bmi", "currentsmoke", "D", "nday_ext_amp", "ndays",
                      "hour", "month", "year", "ht", "baso",
                      "ceo", "lymp", "mono", "neut", "highxy",
                      "lab", "sexmm", "plate")

NESDA <- right_join(NESDA1[,-c(2, 3, 4, 7, 8)], NESDA2, by = "DNAid")
NESDA$sex <- factor(NESDA$sex)
rm(NESDA1, NESDA2, N1_A102, N1_A106, N1_A200D1, N1_A200D2, N1_A357,
   N1_A400, N1_A401, N1_A403, N1_A404, N1_A408, GECCOGeodata120218,
   GODOT_masterfile_pident, N1_A100, N1_A053)
# criteria for removal
NESDA$rownumber <- rownames(NESDA)
remove3 <- unlist(NESDA[which(NESDA$D > 5 | NESDA$highxy == "YES" | NESDA$sexmm == "YES"), "rownumber"]) %>% as.numeric()
# Final dataset NTR
NESDA <- NESDA[-remove3,]
# aincom01 variable from NESDA data from 24 categories to 24 midpoint values
aincom01 <- NESDA$aincom01 %>% 
  zap_labels() %>% 
  as.character() 
aincom01 <- aincom01 %>% recode("-1" = "NA",
                                "1" = "500",
                                "2" = "700",
                                "3" = "900",
                                "4" = "1100",
                                "5" = "1300",
                                "6" = "1500",
                                "7" = "1700",
                                "8" = "1900",
                                "9" = "2100",
                                "10" = "2300",
                                "11" = "2500",
                                "12" = "2700",
                                "13" = "2900",
                                "14" = "3100",
                                "15" = "3300",
                                "16" = "3500",
                                "17" = "3700",
                                "18" = "3900",
                                "19" = "4100",
                                "20" = "4300",
                                "21" = "4500",
                                "22" = "4700",
                                "23" = "4900",
                                "24" = "5100") %>% 
  as.numeric()
NESDA$aincom01 <- aincom01
rm(aincom01)
## plots education
plot_NTR_edu_age <- box_violin_edu(NTR, education, "education", age, "age")
plot_NESDA_edu_age <- box_violin_edu(NESDA, education, "education", age, "age")
plot_NTR_edu_bmi <- box_violin_edu(NTR, education, "education", bmi, "bmi", FALSE, "BMI")
plot_NESDA_edu_bmi <- box_violin_edu(NESDA, education, "education", bmi, "bmi", FALSE, "BMI")

# pro inflammatory 
plot_NTR_edu_il6 <- box_violin_edu(NTR, education, "education", log(bioil6), "bioil6", FALSE, "IL-6 (log)")
plot_NESDA_edu_il6 <- box_violin_edu(NESDA, education, "education", log(aIL6), "aIL6", FALSE, "IL-6 (log)")
plot_NTR_edu_crp <- box_violin_edu(NTR, education, "education", log(biocrp), "biocrp", FALSE, "CRP (log)")
plot_NESDA_edu_crp <- box_violin_edu(NESDA, education, "education", log(ahsCRP), "ahsCRP", FALSE, "CRP (log)")
plot_NTR_edu_tnfa <- box_violin_edu(NTR, education, "education", log(biotnfa), "biotnfa", FALSE, "TNFa (log)")
plot_NESDA_edu_tnfa <- box_violin_edu(NESDA, education, "education", log(aTNFa), "aTNFa", FALSE, "TNFa (log)")

# blood cell counts
plot_NTR_edu_ht <- box_violin_edu(NTR, education, "education", ht, "ht", FALSE, "Hematoglobin")
plot_NTR_edu_ceo <- box_violin_edu(NTR, education, "education", ceo, "ceo", FALSE, "Eosinphiles")
plot_NTR_edu_lymp <- box_violin_edu(NTR, education, "education", lymp, "lymp", FALSE, "Lymphocytes")
plot_NTR_edu_mono <- box_violin_edu(NTR, education, "education", mono, "mono", FALSE, "Monocytes")
plot_NTR_edu_neut <- box_violin_edu(NTR, education, "education", neut, "neut", FALSE, "Neutrophiles")

plot_NESDA_edu_ht <- box_violin_edu(NESDA, education, "education", ht, "ht", FALSE, "Hematoglobin") # ht not included in analyses for NESDA

# education relative to other ses variables
plot_NTR_edu_hsvl <- box_violin_edu(NTR, education, "education", 
                                    agg_hsvl04, "agg_hsvl04", 
                                    FALSE, "House value")
plot_NESDA_edu_hsvl <- box_violin_edu(NESDA, education, "education",
                                      Woz_2006, "Woz_2006", 
                                      FALSE, "House value")

plot_NTR_edu_increg <- box_violin_edu(NTR, education, "education", 
                                      agg_income_region, "agg_income_region", 
                                      FALSE, "Income Region")
plot_NTR_edu_incocc <- box_violin_edu(NTR, education, "education", 
                                      agg_income_work_median, "agg_income_work_median", 
                                      FALSE, "Income")
plot_NESDA_edu_incocc <- box_violin_edu(NESDA, education, "education", 
                                        aincom01, "aincom01", 
                                        FALSE, "Income")

# plot house value-age
plot(NTR$agg_hsvl04, NTR$age_agg)
plot(NTR$agg_hsvl04, NTR$bmi)

# blood cell counts for correlating + exclusion of highly correlating bcc as covariates
correlate_bcc <- cbind.data.frame(baso, ceo, erys, 
                                  hb, ht, leuc, 
                                  lymp, mono, neut)
# remove excluded participants from expression data
expr1 <- (expr[, which(group == "NTR")][, -remove1]) # 3369
expr2 <- (expr[, which(group == "NESDA")][, -remove3]) # 1992
remove1 %>% length() # 113
remove3 %>% length() # 72

rm(expr, age, baso, bmi, ceo, currentsmoke, 
   D, edu, erys, FID, group, hb, 
   highxy, hour, ht, lab, leuc, lymp,
   mono, month, nday_ext_amp, ndays, neut, plate, 
   remove, remove1, remove2, remove3, sex, sexmm,
   status, well, year)
# set education values from levels to numeric
NTR <- NTR %>% mutate(education = recode(education, 
                                         "BASIC" = 1, 
                                         "INTERMEDIATE" = 2, 
                                         "HIGH" = 3))
NESDA <- NESDA %>% mutate(education = recode(education, 
                                             "BASIC" = 1, 
                                             "INTERMEDIATE" = 2, 
                                             "HIGH" = 3)) 
# log transform the inflammatory variables
NTR$IL6_log <- log(NTR$bioil6)
NTR$CRP_log <- log(NTR$biocrp)
NTR$TNFa_log <- log(NTR$biotnfa)
NESDA$IL6_log <- log(NESDA$aIL6)
NESDA$CRP_log <- log(NESDA$ahsCRP)
NESDA$TNFa_log <- log(NESDA$aTNFa)

#for pro-inflammatory outliers, set >3SD to 3SD
NTR <- fix_outliers_infl(NTR, FALSE, 3)
NESDA <- fix_outliers_infl(NESDA, TRUE, 3)

# descriptives
descriptives_NTR <- describe(NTR)[-c(1:3, 5, 7, 10:15, 22:24, 28, 32), -c(1, 6, 7)][c(2, 3, 1, 14:16, 11:13, 17:19, 5:10),]
descriptives_NTR
descriptives_NESDA <- describe(NESDA)[c(11, 13, 9, 2:6, 21, 32:34), -c(1, 6, 7)][c(1:3, 5, 4, 7, 6, 8, 10:12, 9),]
descriptives_NESDA

# set log transformed inflammatory variables as standard
NTR$bioil6 <- NTR$IL6_log; NTR$IL6_log <- NULL
NTR$biocrp <- NTR$CRP_log; NTR$CRP_log <- NULL
NTR$biotnfa <- NTR$TNFa_log; NTR$TNFa_log <- NULL
NESDA$aIL6 <- NESDA$IL6_log; NESDA$IL6_log <- NULL
NESDA$ahsCRP <- NESDA$CRP_log; NESDA$CRP_log <- NULL
NESDA$aTNFa <- NESDA$TNFa_log; NESDA$TNFa_log <- NULL

# combined plots
plot_edu1 <- plot_grid(plot_NTR_edu_age, plot_NESDA_edu_age,
                       plot_NTR_edu_bmi, plot_NESDA_edu_bmi, 
                       nrow = 2, ncol = 2,
                       byrow = TRUE, 
                       labels = LETTERS[1:4])
plot_edu2 <- plot_grid(plot_NTR_edu_il6, plot_NESDA_edu_il6,
                       plot_NTR_edu_crp, plot_NESDA_edu_crp,
                       plot_NTR_edu_tnfa, plot_NESDA_edu_tnfa, 
                       nrow = 3, ncol = 2,
                       byrow = TRUE, 
                       labels = LETTERS[1:6])
plot_edu3 <- plot_grid(plot_NESDA_edu_ht, plot_NTR_edu_ht, 
                       plot_NTR_edu_ceo, plot_NTR_edu_lymp,
                       plot_NTR_edu_mono, plot_NTR_edu_neut,
                       nrow = 3, ncol = 2, 
                       labels = LETTERS[1:6])
plot_edu4 <- plot_grid(plot_NTR_edu_hsvl, plot_NESDA_edu_hsvl, 
                       plot_NTR_edu_incocc, plot_NESDA_edu_incocc, 
                       plot_NTR_edu_increg, 
                       nrow = 3, ncol = 2,
                       byrow = TRUE, 
                       labels = LETTERS[1:5])
# extra descriptives
desc_extra <- list()
desc_extra$sex_education_NTR <- table(NTR$sex, NTR$education)
desc_extra$sex_education_NESDA <- table(NESDA$sex, NESDA$education)
desc_extra$sex_status_NTR <- table(NTR$sex, NTR$status)
desc_extra$sex_status_NESDA <- table(NESDA$sex, NESDA$status)
desc_extra$sex_currentsmoke_NTR <- table(NTR$sex, NTR$currentsmoke)
desc_extra$sex_currentsmoke_NESDA <- table(NESDA$sex, NESDA$currentsmoke)


# save data for GE analysis
save(descriptives_NTR, descriptives_NESDA,
     correlate_bcc, desc_extra,
     plot_edu1, plot_edu2, plot_edu3, plot_edu4,
     plot_categorize_education3_4, plot_categorize_education3_7,
     plot_categorize_education4_7,
     plot_categorize_education3_4_num, plot_categorize_education3_7_num,
     file = "./data/data_report.Rdata")
save(NTR, NESDA, 
     descriptives_NTR, descriptives_NESDA,
     expr1, expr2,
     correlate_bcc,
     plot_edu1, plot_edu2, plot_edu3, plot_edu4,
     plot_categorize_education3_4, plot_categorize_education3_7,
     plot_categorize_education4_7,
     plot_categorize_education3_4_num, plot_categorize_education3_7_num,
     file = "./data/data_after_QC.Rdata")
save(NTR, expr1,
     file = "./data/data_after_QC_minimal_logfix.Rdata")
