# libraries
source("./Script dependencies NESDA server.R")
#source("./Script data cleaning.R")
load("./data/U219_probeset_info_new_v2")
load("./data/data_after_QC.Rdata")
load("./data/data_after_QC_minimal_logfix.Rdata")

# memory
memory.limit(size = 1e13)
memory.limit()

# correlate red & white bloodcells
cormatrix <- cor(correlate_bcc, use = "pairwise.complete.obs") %>% round(2)
cormatrix; rm(correlate_bcc) # exclude baso, erys & hb from the models due to high correlation

### descriptives
descriptives_NTR
descriptives_NESDA

# plots covariates conditional on education
plot_edu1
plot_edu2
plot_edu3
plot_edu4

### data analysis
## models education
# gee models NTR
model0_edu_NTR <- gee_model0(expr1, "education", NTR)
model0_edu_NTR_E <- model0_edu_NTR$E; model0_edu_NTR_SE <- model0_edu_NTR$SE
model0_edu_NTR_Z <- model0_edu_NTR$Z; model0_edu_NTR_P <- model0_edu_NTR$P
model0_edu_NTR_N <- model0_edu_NTR$N
save(model0_edu_NTR_E, model0_edu_NTR_SE, 
     model0_edu_NTR_Z, model0_edu_NTR_P, 
     model0_edu_NTR_N,
     file = "./data/models/model0_edu_NTR.Rdata")
model1_edu_NTR <- gee_model1(expr1, "education", NTR)
model1_edu_NTR_E <- model1_edu_NTR$E; model1_edu_NTR_SE <- model1_edu_NTR$SE
model1_edu_NTR_Z <- model1_edu_NTR$Z; model1_edu_NTR_P <- model1_edu_NTR$P
model1_edu_NTR_N <- model1_edu_NTR$N
save(model1_edu_NTR_E, model1_edu_NTR_SE, 
     model1_edu_NTR_Z, model1_edu_NTR_P, 
     model1_edu_NTR_N,
     file = "./data/models/model1_edu_NTR.Rdata")
model2_edu_NTR <- gee_model2(expr1, "education", NTR)
model2_edu_NTR_E <- model2_edu_NTR$E; model2_edu_NTR_SE <- model2_edu_NTR$SE
model2_edu_NTR_Z <- model2_edu_NTR$Z; model2_edu_NTR_P <- model2_edu_NTR$P
model2_edu_NTR_N <- model2_edu_NTR$N
save(model2_edu_NTR_E, model2_edu_NTR_SE, 
     model2_edu_NTR_Z, model2_edu_NTR_P, 
     model2_edu_NTR_N,
     file = "./data/models/model2_edu_NTR.Rdata")
model3_edu_NTR <- gee_model3(expr1, "education", NTR)
model3_edu_NTR_E <- model3_edu_NTR$E; model3_edu_NTR_SE <- model3_edu_NTR$SE
model3_edu_NTR_Z <- model3_edu_NTR$Z; model3_edu_NTR_P <- model3_edu_NTR$P
model3_edu_NTR_N <- model3_edu_NTR$N
save(model3_edu_NTR_E, model3_edu_NTR_SE, 
     model3_edu_NTR_Z, model3_edu_NTR_P, 
     model3_edu_NTR_N,
     file = "./data/models/model3_edu_NTR.Rdata")
# lm_F models NESDA
model0_edu_NESDA <- lm_F_model0(expr2, ses_var = "education", NESDA)
model0_edu_NESDA_E <- model0_edu_NESDA$B; model0_edu_NESDA_SE <- model0_edu_NESDA$SD
model0_edu_NESDA_P <- model0_edu_NESDA$P ; model0_edu_NESDA_Z <- model0_edu_NESDA$Z
model0_edu_NESDA_N <- model0_edu_NESDA$N
save(model0_edu_NESDA_E, model0_edu_NESDA_SE, 
     model0_edu_NESDA_P, model0_edu_NESDA_N, 
     model0_edu_NESDA_Z,
     file = "./data/models/model0_edu_NESDA.Rdata")
model1_edu_NESDA <- lm_F_model1(expr2, ses_var = "education", NESDA)
model1_edu_NESDA_E <- model1_edu_NESDA$B; model1_edu_NESDA_SE <- model1_edu_NESDA$SD
model1_edu_NESDA_P <- model1_edu_NESDA$P ; model1_edu_NESDA_Z <- model1_edu_NESDA$Z
model1_edu_NESDA_N <- model1_edu_NESDA$N
save(model1_edu_NESDA_E, model1_edu_NESDA_SE, 
     model1_edu_NESDA_P, model1_edu_NESDA_N, 
     model1_edu_NESDA_Z,
     file = "./data/models/model1_edu_NESDA.Rdata")
model2_edu_NESDA <- lm_F_model2(expr2, ses_var = "education", NESDA)
model2_edu_NESDA_E <- model2_edu_NESDA$B; model2_edu_NESDA_SE <- model2_edu_NESDA$SD
model2_edu_NESDA_P <- model2_edu_NESDA$P ; model2_edu_NESDA_Z <- model2_edu_NESDA$Z
model2_edu_NESDA_N <- model2_edu_NESDA$N
save(model2_edu_NESDA_E, model2_edu_NESDA_SE, 
     model2_edu_NESDA_P, model2_edu_NESDA_N, 
     model2_edu_NESDA_Z,
     file = "./data/models/model2_edu_NESDA.Rdata")

## models house value
# gee models NTR
model0_hsvl_NTR <- gee_model0(expr1, "agg_hsvl04", NTR)
model0_hsvl_NTR_E <- model0_hsvl_NTR$E; model0_hsvl_NTR_SE <- model0_hsvl_NTR$SE
model0_hsvl_NTR_Z <- model0_hsvl_NTR$Z; model0_hsvl_NTR_P <- model0_hsvl_NTR$P
model0_hsvl_NTR_N <- model0_hsvl_NTR$N
save(model0_hsvl_NTR_E, model0_hsvl_NTR_SE, 
     model0_hsvl_NTR_Z, model0_hsvl_NTR_P, 
     model0_hsvl_NTR_N,
     file = "./data/models/model0_hsvl_NTR.Rdata")
model1_hsvl_NTR <- gee_model1(expr1, "agg_hsvl04", NTR)
model1_hsvl_NTR_E <- model1_hsvl_NTR$E; model1_hsvl_NTR_SE <- model1_hsvl_NTR$SE
model1_hsvl_NTR_Z <- model1_hsvl_NTR$Z; model1_hsvl_NTR_P <- model1_hsvl_NTR$P
model1_hsvl_NTR_N <- model1_hsvl_NTR$N
save(model1_hsvl_NTR_E, model1_hsvl_NTR_SE, 
     model1_hsvl_NTR_Z, model1_hsvl_NTR_P, 
     model1_hsvl_NTR_N,
     file = "./data/models/model1_hsvl_NTR.Rdata")
model2_hsvl_NTR <- gee_model2(expr1, "agg_hsvl04", NTR)
model2_hsvl_NTR_E <- model2_hsvl_NTR$E; model2_hsvl_NTR_SE <- model2_hsvl_NTR$SE
model2_hsvl_NTR_Z <- model2_hsvl_NTR$Z; model2_hsvl_NTR_P <- model2_hsvl_NTR$P
model2_hsvl_NTR_N <- model2_hsvl_NTR$N
save(model2_hsvl_NTR_E, model2_hsvl_NTR_SE, 
     model2_hsvl_NTR_Z, model2_hsvl_NTR_P, 
     model2_hsvl_NTR_N,
     file = "./data/models/model2_hsvl_NTR.Rdata")
model3_hsvl_NTR <- gee_model3(expr1, "agg_hsvl04", NTR)
model3_hsvl_NTR_E <- model3_hsvl_NTR$E; model3_hsvl_NTR_SE <- model3_hsvl_NTR$SE
model3_hsvl_NTR_Z <- model3_hsvl_NTR$Z; model3_hsvl_NTR_P <- model3_hsvl_NTR$P
model3_hsvl_NTR_N <- model3_hsvl_NTR$N
save(model3_hsvl_NTR_E, model3_hsvl_NTR_SE, 
     model3_hsvl_NTR_Z, model3_hsvl_NTR_P, 
     model3_hsvl_NTR_N,
     file = "./data/models/model3_hsvl_NTR.Rdata")
# lm_F models NESDA
model0_hsvl_NESDA <- lm_F_model0(expr2, ses_var = "Woz_2006", NESDA)
model0_hsvl_NESDA_E <- model0_hsvl_NESDA$B; model0_hsvl_NESDA_SE <- model0_hsvl_NESDA$SD
model0_hsvl_NESDA_P <- model0_hsvl_NESDA$P ; model0_hsvl_NESDA_Z <- model0_hsvl_NESDA$Z
model0_hsvl_NESDA_N <- model0_hsvl_NESDA$N
save(model0_hsvl_NESDA_E, model0_hsvl_NESDA_SE, 
     model0_hsvl_NESDA_P, model0_hsvl_NESDA_N, 
     model0_hsvl_NESDA_Z,
     file = "./data/models/model0_hsvl_NESDA.Rdata")
model1_hsvl_NESDA <- lm_F_model1(expr2, ses_var = "Woz_2006", NESDA)
model1_hsvl_NESDA_E <- model1_hsvl_NESDA$B; model1_hsvl_NESDA_SE <- model1_hsvl_NESDA$SD
model1_hsvl_NESDA_P <- model1_hsvl_NESDA$P ; model1_hsvl_NESDA_Z <- model1_hsvl_NESDA$Z
model1_hsvl_NESDA_N <- model1_hsvl_NESDA$N
save(model1_hsvl_NESDA_E, model1_hsvl_NESDA_SE, 
     model1_hsvl_NESDA_P, model1_hsvl_NESDA_N, 
     model1_hsvl_NESDA_Z,
     file = "./data/models/model1_hsvl_NESDA.Rdata")
model2_hsvl_NESDA <- lm_F_model2(expr2, ses_var = "Woz_2006", NESDA)
model2_hsvl_NESDA_E <- model2_hsvl_NESDA$B; model2_hsvl_NESDA_SE <- model2_hsvl_NESDA$SD
model2_hsvl_NESDA_P <- model2_hsvl_NESDA$P ; model2_hsvl_NESDA_Z <- model2_hsvl_NESDA$Z
model2_hsvl_NESDA_N <- model2_hsvl_NESDA$N
save(model2_hsvl_NESDA_E, model2_hsvl_NESDA_SE, 
     model2_hsvl_NESDA_P, model2_hsvl_NESDA_N, 
     model2_hsvl_NESDA_Z,
     file = "./data/models/model2_hsvl_NESDA.Rdata")

## models income occupation
# gee models NTR
model0_incocc_NTR <- gee_model0(expr1, "agg_income_work_median", NTR)
model0_incocc_NTR_E <- model0_incocc_NTR$E; model0_incocc_NTR_SE <- model0_incocc_NTR$SE
model0_incocc_NTR_Z <- model0_incocc_NTR$Z; model0_incocc_NTR_P <- model0_incocc_NTR$P
model0_incocc_NTR_N <- model0_incocc_NTR$N
save(model0_incocc_NTR_E, model0_incocc_NTR_SE, 
     model0_incocc_NTR_Z, model0_incocc_NTR_P, 
     model0_incocc_NTR_N,
     file = "./data/models/model0_incocc_NTR.Rdata")
model1_incocc_NTR <- gee_model1(expr1, "agg_income_work_median", NTR)
model1_incocc_NTR_E <- model1_incocc_NTR$E; model1_incocc_NTR_SE <- model1_incocc_NTR$SE
model1_incocc_NTR_Z <- model1_incocc_NTR$Z; model1_incocc_NTR_P <- model1_incocc_NTR$P
model1_incocc_NTR_N <- model1_incocc_NTR$N
save(model1_incocc_NTR_E, model1_incocc_NTR_SE, 
     model1_incocc_NTR_Z, model1_incocc_NTR_P, 
     model1_incocc_NTR_N,
     file = "./data/models/model1_incocc_NTR.Rdata")
model2_incocc_NTR <- gee_model2(expr1, "agg_income_work_median", NTR)
model2_incocc_NTR_E <- model2_incocc_NTR$E; model2_incocc_NTR_SE <- model2_incocc_NTR$SE
model2_incocc_NTR_Z <- model2_incocc_NTR$Z; model2_incocc_NTR_P <- model2_incocc_NTR$P
model2_incocc_NTR_N <- model2_incocc_NTR$N
save(model2_incocc_NTR_E, model2_incocc_NTR_SE, 
     model2_incocc_NTR_Z, model2_incocc_NTR_P, 
     model2_incocc_NTR_N,
     file = "./data/models/model2_incocc_NTR.Rdata")
model3_incocc_NTR <- gee_model3(expr1, "agg_income_work_median", NTR)
model3_incocc_NTR_E <- model3_incocc_NTR$E; model3_incocc_NTR_SE <- model3_incocc_NTR$SE
model3_incocc_NTR_Z <- model3_incocc_NTR$Z; model3_incocc_NTR_P <- model3_incocc_NTR$P
model3_incocc_NTR_N <- model3_incocc_NTR$N
save(model3_incocc_NTR_E, model3_incocc_NTR_SE, 
     model3_incocc_NTR_Z, model3_incocc_NTR_P, 
     model3_incocc_NTR_N,
     file = "./data/models/model3_incocc_NTR.Rdata")
# lm_F models NESDA
model0_incocc_NESDA <- lm_F_model0(expr2, ses_var = "aincom01", NESDA)
model0_incocc_NESDA_E <- model0_incocc_NESDA$B; model0_incocc_NESDA_SE <- model0_incocc_NESDA$SD
model0_incocc_NESDA_P <- model0_incocc_NESDA$P ; model0_incocc_NESDA_Z <- model0_incocc_NESDA$Z
model0_incocc_NESDA_N <- model0_incocc_NESDA$N
save(model0_incocc_NESDA_E, model0_incocc_NESDA_SE, 
     model0_incocc_NESDA_P, model0_incocc_NESDA_N, 
     model0_incocc_NESDA_Z,
     file = "./data/models/model0_incocc_NESDA.Rdata")
model1_incocc_NESDA <- lm_F_model1(expr2, ses_var = "aincom01", NESDA)
model1_incocc_NESDA_E <- model1_incocc_NESDA$B; model1_incocc_NESDA_SE <- model1_incocc_NESDA$SD
model1_incocc_NESDA_P <- model1_incocc_NESDA$P ; model1_incocc_NESDA_Z <- model1_incocc_NESDA$Z
model1_incocc_NESDA_N <- model1_incocc_NESDA$N
save(model1_incocc_NESDA_E, model1_incocc_NESDA_SE, 
     model1_incocc_NESDA_P, model1_incocc_NESDA_N, 
     model1_incocc_NESDA_Z,
     file = "./data/models/model1_incocc_NESDA.Rdata")
model2_incocc_NESDA <- lm_F_model2(expr2, ses_var = "aincom01", NESDA)
model2_incocc_NESDA_E <- model2_incocc_NESDA$B; model2_incocc_NESDA_SE <- model2_incocc_NESDA$SD
model2_incocc_NESDA_P <- model2_incocc_NESDA$P ; model2_incocc_NESDA_Z <- model2_incocc_NESDA$Z
model2_incocc_NESDA_N <- model2_incocc_NESDA$N
save(model2_incocc_NESDA_E, model2_incocc_NESDA_SE, 
     model2_incocc_NESDA_P, model2_incocc_NESDA_N, 
     model2_incocc_NESDA_Z,
     file = "./data/models/model2_incocc_NESDA.Rdata")

## models income region
# gee models NTR
model0_increg_NTR <- gee_model0(expr1, "agg_income_region", NTR)
model0_increg_NTR_E <- model0_increg_NTR$E; model0_increg_NTR_SE <- model0_increg_NTR$SE
model0_increg_NTR_Z <- model0_increg_NTR$Z; model0_increg_NTR_P <- model0_increg_NTR$P
model0_increg_NTR_N <- model0_increg_NTR$N
save(model0_increg_NTR_E, model0_increg_NTR_SE, 
     model0_increg_NTR_Z, model0_increg_NTR_P, 
     model0_increg_NTR_N,
     file = "./data/models/model0_increg_NTR.Rdata")
model1_increg_NTR <- gee_model1(expr1, "agg_income_region", NTR)
model1_increg_NTR_E <- model1_increg_NTR$E; model1_increg_NTR_SE <- model1_increg_NTR$SE
model1_increg_NTR_Z <- model1_increg_NTR$Z; model1_increg_NTR_P <- model1_increg_NTR$P
model1_increg_NTR_N <- model1_increg_NTR$N
save(model1_increg_NTR_E, model1_increg_NTR_SE, 
     model1_increg_NTR_Z, model1_increg_NTR_P, 
     model1_increg_NTR_N,
     file = "./data/models/model1_increg_NTR.Rdata")
model2_increg_NTR <- gee_model2(expr1, "agg_income_region", NTR)
model2_increg_NTR_E <- model2_increg_NTR$E; model2_increg_NTR_SE <- model2_increg_NTR$SE
model2_increg_NTR_Z <- model2_increg_NTR$Z; model2_increg_NTR_P <- model2_increg_NTR$P
model2_increg_NTR_N <- model2_increg_NTR$N
save(model2_increg_NTR_E, model2_increg_NTR_SE, 
     model2_increg_NTR_Z, model2_increg_NTR_P, 
     model2_increg_NTR_N,
     file = "./data/models/model2_increg_NTR.Rdata")
model3_increg_NTR <- gee_model3(expr1, "agg_income_region", NTR)
model3_increg_NTR_E <- model3_increg_NTR$E; model3_increg_NTR_SE <- model3_increg_NTR$SE
model3_increg_NTR_Z <- model3_increg_NTR$Z; model3_increg_NTR_P <- model3_increg_NTR$P
model3_increg_NTR_N <- model3_increg_NTR$N
save(model3_increg_NTR_E, model3_increg_NTR_SE, 
     model3_increg_NTR_Z, model3_increg_NTR_P, 
     model3_increg_NTR_N,
     file = "./data/models/model3_increg_NTR.Rdata")

### load complete models
# all models for education
load("./data/models/model0_edu_NTR.Rdata")
load("./data/models/model0_edu_NESDA.Rdata")
load("./data/models/model1_edu_NTR.Rdata")
load("./data/models/model1_edu_NESDA.Rdata")
load("./data/models/model2_edu_NTR.Rdata")
load("./data/models/model2_edu_NESDA.Rdata")
load("./data/models/model3_edu_NTR.Rdata")
# all models for house value
load("./data/models/model0_hsvl_NTR.Rdata")
load("./data/models/model0_hsvl_NESDA.Rdata")
load("./data/models/model1_hsvl_NTR.Rdata")
load("./data/models/model1_hsvl_NESDA.Rdata")
load("./data/models/model2_hsvl_NTR.Rdata")
load("./data/models/model2_hsvl_NESDA.Rdata")
load("./data/models/model3_hsvl_NTR.Rdata")
# all models for income occupation
load("./data/models/model0_incocc_NTR.Rdata")
load("./data/models/model0_incocc_NESDA.Rdata")
load("./data/models/model1_incocc_NTR.Rdata")
load("./data/models/model1_incocc_NESDA.Rdata")
load("./data/models/model2_incocc_NTR.Rdata")
load("./data/models/model2_incocc_NESDA.Rdata")
load("./data/models/model3_incocc_NTR.Rdata")
# all models for income region
load("./data/models/model0_increg_NTR.Rdata")
load("./data/models/model1_increg_NTR.Rdata")
load("./data/models/model2_increg_NTR.Rdata")
load("./data/models/model3_increg_NTR.Rdata")

### ses-gene expression associations
## education
model0_edu <- output_meta_fdr(model0_edu_NTR_P, model0_edu_NESDA_P,
                              model0_edu_NTR_E, model0_edu_NESDA_E,
                              model0_edu_NTR_SE, model0_edu_NESDA_SE,
                              model0_edu_NTR_N, model0_edu_NESDA_N,
                              "education", "education_model0",
                              P_to_META = "p_Zscore_weighted")
save(model0_edu, file = "./data/models/meta/model0_edu_META.Rdata")
model1_edu <- output_meta_fdr(model1_edu_NTR_P, model1_edu_NESDA_P,
                              model1_edu_NTR_E, model1_edu_NESDA_E,
                              model1_edu_NTR_SE, model1_edu_NESDA_SE,
                              model1_edu_NTR_N, model1_edu_NESDA_N,
                              "education", "education_model1",
                              P_to_META = "p_Zscore_weighted")
save(model1_edu, file = "./data/models/meta/model1_edu_META.Rdata")
model2_edu <- output_meta_fdr(model2_edu_NTR_P, model2_edu_NESDA_P,
                              model2_edu_NTR_E, model2_edu_NESDA_E,
                              model2_edu_NTR_SE, model2_edu_NESDA_SE,
                              model2_edu_NTR_N, model2_edu_NESDA_N,
                              "education", "education_model2",
                              P_to_META = "p_Zscore_weighted")
save(model2_edu, file = "./data/models/meta/model2_edu_META.Rdata")
# not a meta-analysis
model3_edu <- output_ntr_fdr(model3_edu_NTR_P, 
                              model3_edu_NTR_E, 
                              model3_edu_NTR_SE, 
                              "education", "education_model3")
save(model3_edu, file = "./data/models/meta/model3_edu_FDR.Rdata")
## house value
model0_hsvl <- output_meta_fdr(model0_hsvl_NTR_P, model0_hsvl_NESDA_P,
                              model0_hsvl_NTR_E, model0_hsvl_NESDA_E,
                              model0_hsvl_NTR_SE, model0_hsvl_NESDA_SE,
                              model0_hsvl_NTR_N, model0_hsvl_NESDA_N,
                              "house value", "hsvl_model0",
                              P_to_META = "p_Zscore_weighted")
save(model0_hsvl, file = "./data/models/meta/model0_hsvl_META.Rdata")
model1_hsvl <- output_meta_fdr(model1_hsvl_NTR_P, model1_hsvl_NESDA_P,
                              model1_hsvl_NTR_E, model1_hsvl_NESDA_E,
                              model1_hsvl_NTR_SE, model1_hsvl_NESDA_SE,
                              model1_hsvl_NTR_N, model1_hsvl_NESDA_N,
                              "house value", "hsvl_model1",
                              P_to_META = "p_Zscore_weighted")
save(model1_hsvl, file = "./data/models/meta/model1_hsvl_META.Rdata")
model2_hsvl <- output_meta_fdr(model2_hsvl_NTR_P, model2_hsvl_NESDA_P,
                              model2_hsvl_NTR_E, model2_hsvl_NESDA_E,
                              model2_hsvl_NTR_SE, model2_hsvl_NESDA_SE,
                              model2_hsvl_NTR_N, model2_hsvl_NESDA_N,
                              "house value", "hsvl_model2",
                              P_to_META = "p_Zscore_weighted")
save(model2_hsvl, file = "./data/models/meta/model2_hsvl_META.Rdata")
# not a meta-analysis
model3_hsvl <- output_ntr_fdr(model3_hsvl_NTR_P, 
                              model3_hsvl_NTR_E, 
                              model3_hsvl_NTR_SE, 
                              "house value", "hsvl_model3")
save(model3_hsvl, file = "./data/models/meta/model3_hsvl_FDR.Rdata")
## income occupation
model0_incocc <- output_meta_fdr(model0_incocc_NTR_P, model0_incocc_NESDA_P,
                               model0_incocc_NTR_E, model0_incocc_NESDA_E,
                               model0_incocc_NTR_SE, model0_incocc_NESDA_SE,
                               model0_incocc_NTR_N, model0_incocc_NESDA_N,
                               "income occupation", "incocc_model0",
                               P_to_META = "p_Zscore_weighted")
save(model0_incocc, file = "./data/models/meta/model0_incocc_META.Rdata")
model1_incocc <- output_meta_fdr(model1_incocc_NTR_P, model1_incocc_NESDA_P,
                               model1_incocc_NTR_E, model1_incocc_NESDA_E,
                               model1_incocc_NTR_SE, model1_incocc_NESDA_SE,
                               model1_incocc_NTR_N, model1_incocc_NESDA_N,
                               "income occupation", "incocc_model1",
                               P_to_META = "p_Zscore_weighted")
save(model1_incocc, file = "./data/models/meta/model1_incocc_META.Rdata")
model2_incocc <- output_meta_fdr(model2_incocc_NTR_P, model2_incocc_NESDA_P,
                               model2_incocc_NTR_E, model2_incocc_NESDA_E,
                               model2_incocc_NTR_SE, model2_incocc_NESDA_SE,
                               model2_incocc_NTR_N, model2_incocc_NESDA_N,
                               "income occupation", "incocc_model2",
                               P_to_META = "p_Zscore_weighted")
save(model2_incocc, file = "./data/models/meta/model2_incocc_META.Rdata")
# not a meta-analysis
model3_incocc <- output_ntr_fdr(model3_incocc_NTR_P, 
                               model3_incocc_NTR_E, 
                               model3_incocc_NTR_SE, 
                               "income occupation", "incocc_model3")
save(model3_incocc, file = "./data/models/meta/model3_incocc_FDR.Rdata")
## income region
model0_increg <- output_ntr_fdr(model0_increg_NTR_P, model0_increg_NTR_E, 
                                model0_increg_NTR_SE,
                                "income region", "increg_model0")
save(model0_increg, file = "./data/models/meta/model0_increg_FDR.Rdata")

model1_increg <- output_ntr_fdr(model1_increg_NTR_P, model1_increg_NTR_E, 
                                model1_increg_NTR_SE,
                                "income region", "increg_model1")
save(model1_increg, file = "./data/models/meta/model1_increg_FDR.Rdata")
model2_increg <- output_ntr_fdr(model2_increg_NTR_P, model2_increg_NTR_E, 
                                model2_increg_NTR_SE,
                                "income region", "increg_model2")
save(model2_increg, file = "./data/models/meta/model2_increg_FDR.Rdata")
model3_increg <- output_ntr_fdr(model3_increg_NTR_P, model3_increg_NTR_E, 
                                model3_increg_NTR_SE,
                                "income region", "increg_model3")
save(model3_increg, file = "./data/models/meta/model3_increg_FDR.Rdata")


# list full gene set for FUMA 
pastename <- paste0("./data/models/top100_lists/", modelname, ".txt")
list_genes_full <- ens_gene_names %>% 
   as.data.frame() 
colnames(list_genes_full)[1] <- "genes_ENSG"
list_genes_full <- list_genes_full %>% filter(., genes_ENSG != "")
list_genes_full[c(1:10),]
write.table(list_genes_full, file = "./data/models/top100_lists/list_genes_full.txt", 
            quote = FALSE,
            row.names = FALSE, col.names = FALSE)

### loading all meta models
## load earlier models as well!
# income occupation
load("./data/models/meta/model0_edu_META.Rdata")
load("./data/models/meta/model1_edu_META.Rdata")
load("./data/models/meta/model2_edu_META.Rdata")
load("./data/models/meta/model3_edu_FDR.Rdata")
make_supp_data("edu", meta = TRUE, 
               model0_edu, model1_edu, 
               model2_edu, model3_edu) 

model0_edu$fdr_1_META
model1_edu$fdr_1_META # 2 genes sig, FUMA gene list top 100
model2_edu$fdr_1_META
model3_edu$fdr_1_NTR
# housing value
load("./data/models/meta/model0_hsvl_META.Rdata")
load("./data/models/meta/model1_hsvl_META.Rdata")
load("./data/models/meta/model2_hsvl_META.Rdata")
load("./data/models/meta/model3_hsvl_FDR.Rdata")
make_supp_data("hsvl", meta = TRUE, 
               model0_hsvl, model1_hsvl, 
               model2_hsvl, model3_hsvl) 
model0_hsvl$fdr_1_META
model1_hsvl$fdr_1_META # 5 genes sig, of which 2 identical, FUMA gene list top 100
model2_hsvl$fdr_1_META
model3_hsvl$fdr_1_NTR

# income occupation
load("./data/models/meta/model0_incocc_META.Rdata")
load("./data/models/meta/model1_incocc_META.Rdata")
load("./data/models/meta/model2_incocc_META.Rdata")
load("./data/models/meta/model3_incocc_FDR.Rdata")
make_supp_data("incocc", meta = TRUE, 
               model0_incocc, model1_incocc, 
               model2_incocc, model3_incocc) 

model0_incocc$fdr_1_META
model1_incocc$fdr_1_META # more than 10, make FUMA gene list!
model2_incocc$fdr_1_META
model3_incocc$fdr_1_NTR
# income region
load("./data/models/meta/model0_increg_FDR.Rdata")
load("./data/models/meta/model1_increg_FDR.Rdata")
load("./data/models/meta/model2_increg_FDR.Rdata")
load("./data/models/meta/model3_increg_FDR.Rdata")
make_supp_data("increg", meta = FALSE,
               model0_increg, model1_increg, 
               model2_increg, model3_increg)
model0_increg$fdr_1_NTR
model1_increg$fdr_1_NTR # 0 genes sig, FUMA gene list top 100
model2_increg$fdr_1_NTR
model3_increg$fdr_1_NTR

# FUMA gene list for model1_incocc
FUMA_list_model1_incocc <- model1_incocc$fdr_1_META %>% 
   filter(., concordant_E == "YES") %>%
   arrange(., P_value) %>%
   select(., ens_name)
FUMA_list_model1_incocc_unique <- FUMA_list_model1_incocc[!duplicated(FUMA_list_model1_incocc),] %>% as.data.frame()

write.table(FUMA_list_model1_incocc_unique, 
            file = "./data/models/top100_lists/socdem_model1_incocc.txt", 
            quote = FALSE,
            row.names = FALSE, col.names = FALSE)
