### libraries
library("tidyverse")
library("gee")
library("haven")
library("readxl")
library("psych")
library("lubridate")
library("anytime")
library("metap")
library("cowplot")
library("reshape2")
library("openxlsx")

source("./lm_F.R") # lm_F function Rick

### create directories
if(!dir.exists("./data/SuppData")){
  dir.create("./data/SuppData")
}
if(!dir.exists("./data/models")){
  dir.create("./data/models")
}
if(!dir.exists("./data/models/meta")){
  dir.create("./data/models/meta")
}
if(!dir.exists("./data/models/top100_lists")){
  dir.create("./data/models/top100_lists")
}
### functions
"%!in%" <- function(x, y){
  !("%in%"(x, y))
  }
# create aggregate variables
"aggregate" <- function(data, var_as_7, var_as_8, var_agg){
  for (i in 1:nrow(data)) {
    if(data[i, "distance"] == "AS_7" & !is.na(data[i, var_as_7])){
      data[i, var_agg] <- data[i, var_as_7] %>% as.numeric()
    } else
      if(data[i, "distance"] == "AS_8" & !is.na(data[i, var_as_8])){
        data[i, var_agg] <- data[i, var_as_8] %>% as.numeric()
      } else
        if(data[i, "distance"] == "AS_7" & is.na(data[i, var_as_7])){
          data[i, var_agg] <- data[i, var_as_8] %>% as.numeric()
        } else
          if(data[i, "distance"] == "AS_8" & is.na(data[i, var_as_8])){
            data[i, var_agg] <- data[i, var_as_7] %>% as.numeric()
          }
  }
  return(data)
}

# fill NAs in education variable with available info from other two education variables
"add_missing_edu" <- function(data, eduvar1, eduvar2, eduvar3){
  for (i in 1:nrow(data)) {
    if((is.na(data[i, eduvar1])) & (!is.na(data[i, eduvar2]))){
      data[i, eduvar1] <- ifelse(data[i, eduvar2] < 3, 
                                 "BASIC",
                                 ifelse(data[i, eduvar2] <= 5, 
                                        "INTERMEDIATE",
                                        "HIGH")
      )
    } else
      if((is.na(data[i, eduvar1])) & (is.na(data[i, eduvar2])) & (!is.na(data[i, eduvar3]))){
        data[i, eduvar1] <- ifelse(data[i, eduvar3] < 2, 
                                   "BASIC",
                                   ifelse(data[i, eduvar3] <= 3, 
                                          "INTERMEDIATE",
                                          "HIGH")
        )
      }
  }
  return(data) 
}
# fixing outliers logtransformed pro-inflammatory variables
"fix_outliers_infl" <- function(data, 
                                is_tibble = FALSE, 
                                N_SD = 4){
  if(is_tibble == FALSE){
    
    mean_IL6_log <- mean(data[, "IL6_log"], na.rm = TRUE)
    sd_IL6_log <- sd(data[, "IL6_log"], na.rm = TRUE)
    mean_CRP_log <- mean(data[, "CRP_log"], na.rm = TRUE)
    sd_CRP_log <- sd(data[, "CRP_log"], na.rm = TRUE)
    mean_TNFa_log <- mean(data[, "TNFa_log"], na.rm = TRUE)
    sd_TNFa_log <- sd(data[, "TNFa_log"], na.rm = TRUE)
    
  } else if(is_tibble == TRUE){
    
    mean_IL6_log <- mean(data[["IL6_log"]], na.rm = TRUE)
    sd_IL6_log <- sd(data[["IL6_log"]], na.rm = TRUE)
    mean_CRP_log <- mean(data[["CRP_log"]], na.rm = TRUE)
    sd_CRP_log <- sd(data[["CRP_log"]], na.rm = TRUE)
    mean_TNFa_log <- mean(data[["TNFa_log"]], na.rm = TRUE)
    sd_TNFa_log <- sd(data[["TNFa_log"]], na.rm = TRUE)
    
  }
  
  threshold_il6_upper <- mean_IL6_log + (N_SD * sd_IL6_log)
  threshold_il6_lower <- mean_IL6_log - (N_SD * sd_IL6_log)
  threshold_crp_upper <- mean_CRP_log + (N_SD * sd_CRP_log)
  threshold_crp_lower <- mean_CRP_log - (N_SD * sd_CRP_log)
  threshold_tnfa_upper <- mean_TNFa_log + (N_SD * sd_TNFa_log)
  threshold_tnfa_lower <- mean_TNFa_log - (N_SD * sd_TNFa_log)
  
  for (i in 1:nrow(data)) {
    if(!is.na(data[i, "IL6_log"]) & data[i, "IL6_log"] > threshold_il6_upper){
      data[i, "IL6_log"] <- threshold_il6_upper
    } else if(!is.na(data[i, "IL6_log"]) & data[i, "IL6_log"] < threshold_il6_lower){
      data[i, "IL6_log"] <- threshold_il6_lower
    }
    
    if(!is.na(data[i, "CRP_log"]) & data[i, "CRP_log"] > threshold_crp_upper){
      data[i, "CRP_log"] <- threshold_crp_upper
    } else if(!is.na(data[i, "CRP_log"]) & data[i, "CRP_log"] < threshold_crp_lower){
      data[i, "CRP_log"] <- threshold_crp_lower
    }
    
    if(!is.na(data[i, "TNFa_log"]) & data[i, "TNFa_log"] > threshold_tnfa_upper){
      data[i, "TNFa_log"] <- threshold_tnfa_upper
    } else if(!is.na(data[i, "TNFa_log"]) & data[i, "TNFa_log"] < threshold_tnfa_lower){
      data[i, "TNFa_log"] <- threshold_tnfa_lower
    }
  }
  return(data)
}
# box + violin plot
"box_violin_edu" <- function(data, sesvar, sesvar_quotes, 
                             depvar, depvar_quotes, 
                             title_as_var = TRUE, alt_var_name = NA){
  arg <- match.call()
  level_order <- c("BASIC", "INTERMEDIATE", "HIGH")
  sex_levels <- data[,which(colnames(data) == "sex")] %>% unique() %>% as.data.frame()
  sample_size <- data %>% 
    group_by(factor(eval(arg$sesvar), 
                    levels = level_order)) %>% 
    filter(., !is.na(eval(arg$depvar))) %>% 
    summarize(num = n()) %>% 
    na.omit()

  data %>% filter(!is.na(eval(arg$sesvar))) %>% 
    ggplot(., aes(eval(arg$depvar), factor(eval(arg$sesvar), levels = level_order))) +
    geom_violin(aes(fill = sex), trim = FALSE,
                alpha = .7) + 
    geom_boxplot(aes(eval(arg$depvar), eval(arg$sesvar), group = interaction(sex, eval(arg$sesvar))), 
                 width = .4,
                 alpha = .5, inherit.aes = FALSE,
                 position = position_dodge(.9)) + 
    ggtitle(
      ifelse(title_as_var == FALSE, 
             paste(
               alt_var_name, 
               "conditional on",
               colnames(data)[which(colnames(data) == sesvar_quotes)]),
             str_to_sentence(
               paste(
                 colnames(data)[which(colnames(data) == depvar_quotes)], 
                 "conditional on", 
                 colnames(data)[which(colnames(data) == sesvar_quotes)]))
      )) +
    ylab(str_to_sentence(
      paste(colnames(data)[which(colnames(data) == sesvar_quotes)])
    )) +
    xlab(
      ifelse(title_as_var == FALSE,
             paste(alt_var_name),
             str_to_sentence(
               paste(
                 colnames(data)[which(colnames(data) == depvar_quotes)]))
      )) +
    labs(caption = paste("Data:", deparse(substitute(data)))) +
    theme(
      plot.title = element_text(hjust = .5),
      panel.background = element_rect(fill = "grey85")) +
    scale_fill_manual(name = "Sex",
                      values = c("red", "green"),
                      breaks = sex_levels,#levels(data[,which(colnames(data) == "sex")]),
                      labels = sex_levels)+#c("Female", "Male"))+ #str_to_sentence(paste(levels(data[,which(colnames(data) == "sex")])))) +
    scale_y_discrete(breaks = c(level_order),
                     labels = c(str_to_sentence(paste(level_order, "\n", "N = ", sample_size$num)))) 
}

# GEE models
"gee_model0" <- function(expression_data, ses_var, data){
  E <- c(); SE <- c(); Z <- c(); P <- c()
  ind <- which(!is.na(data[, ses_var]))
  data <- data[ind, ]
  N <- nrow(data)
  
  expression_data <- expression_data[, ind]
  ses <- data[, ses_var]
  sex <- data[, "sex"]
  age <- data[, "age"]

  D <- data[, "D"]
  nday_ext_amp <- data[, "nday_ext_amp"]
  ndays <- data[, "ndays"]
  hour <- data[, "hour"]
  month <- data[, "month"]
  year <- data[, "year"]
  pt <- data[, "plate"] %>% as.character()
  FID <- data[, "FID"]
  
  start <- Sys.time()
  for(i in 1:nrow(expression_data)){ 
    time_loop_start <- Sys.time()
    cat("Loop", i, "starts at \n"); print(time_loop_start, quote = FALSE)
    
    gm <- gee(expression_data[i, ] ~ ses +
                sex + age +
                D + nday_ext_amp + ndays + 
                hour + month + year + 
                pt, 
              family = gaussian, corstr = "exchangeable",
              maxiter = 100, na.action = na.omit,
              id = as.numeric(FID), silent = TRUE)
    coeff <- summary(gm)$coefficients
    E <- cbind(E, coeff[, 1])
    SE <- cbind(SE, coeff[, 4])
    Z <- cbind(Z, coeff[, 5])
    P <- cbind(P, 2*pnorm(-abs(coeff[, 5])))
    
    end <- Sys.time() - time_loop_start
    cat("Loop", i, "has ended \n"); print(end, quote = FALSE)
  }
  output <- list()
  output$E <- E
  output$SE <- SE
  output$Z <- Z
  output$P <- P
  output$N <- N

  end_full_model <- Sys.time() - start
  cat("Full model finished. \n"); print(end_full_model, quote = FALSE)
  return(output)
}

"gee_model1" <- function(expression_data, ses_var, data){
  E <- c(); SE <- c(); Z <- c(); P <- c()
  ind <- which(!is.na(data[, ses_var]))
  data <- data[ind, ]
  N <- nrow(data)
  
  expression_data <- expression_data[, ind]
  ses <- data[, ses_var]
  status <- data[, "status"]
  bmi <- data[, "bmi"]
  sex <- data[, "sex"]
  age <- data[, "age"]
  currentsmoke <- data[, "currentsmoke"]
  D <- data[, "D"]
  nday_ext_amp <- data[, "nday_ext_amp"]
  ndays <- data[, "ndays"]
  hour <- data[, "hour"]
  month <- data[, "month"]
  year <- data[, "year"]
  pt <- data[, "plate"] %>% as.character()
  FID <- data[, "FID"]
  
  start <- Sys.time()
  for(i in 1:nrow(expression_data)){ 
    time_loop_start <- Sys.time()
    cat("Loop", i, "starts at \n"); print(time_loop_start, quote = FALSE)
    
    gm <- gee(expression_data[i, ] ~ ses + status +
                bmi + sex + age +
                currentsmoke +   
                D + nday_ext_amp + ndays + 
                hour + month + year + 
                pt, 
              family = gaussian, corstr = "exchangeable",
              maxiter = 100, na.action = na.omit,
              id = as.numeric(FID), silent = TRUE)
    coeff <- summary(gm)$coefficients
    E <- cbind(E, coeff[, 1])
    SE <- cbind(SE, coeff[, 4])
    Z <- cbind(Z, coeff[, 5])
    P <- cbind(P, 2*pnorm(-abs(coeff[, 5])))
    
    end <- Sys.time() - time_loop_start
    cat("Loop", i, "has ended \n"); print(end, quote = FALSE)
  }
  output <- list()
  output$E <- E
  output$SE <- SE
  output$Z <- Z
  output$P <- P
  output$N <- N

  end_full_model <- Sys.time() - start
  cat("Full model finished. \n"); print(end_full_model, quote = FALSE)
  return(output)
}

"gee_model2" <- function(expression_data, ses_var, data){
  E <- c(); SE <- c(); Z <- c(); P <- c()
  ind <- which(!is.na(data[, ses_var]))
  data <- data[ind, ]
  N <- nrow(data)
  
  expression_data <- expression_data[, ind]
  ses <- data[, ses_var]
  status <- data[, "status"]
  bmi <- data[, "bmi"]
  sex <- data[, "sex"]
  age <- data[, "age"]
  currentsmoke <- data[, "currentsmoke"]
  D <- data[, "D"]
  nday_ext_amp <- data[, "nday_ext_amp"]
  ndays <- data[, "ndays"]
  hour <- data[, "hour"]
  month <- data[, "month"]
  year <- data[, "year"]
  pt <- data[, "plate"] %>% as.character()
  IL6 <- data[, "bioil6"]
  CRP <- data[, "biocrp"]
  TNFa <- data[, "biotnfa"]
  FID <- data[, "FID"]

  start <- Sys.time()
  for(i in 1:nrow(expression_data)){ 
    time_loop_start <- Sys.time()
    cat("Loop", i, "starts at \n"); print(time_loop_start, quote = FALSE)
    
    gm <- gee(expression_data[i, ] ~ ses + status +
                bmi + sex + age +
                currentsmoke +   
                D + nday_ext_amp + ndays + 
                hour + month + year + 
                IL6 + CRP + TNFa +
                pt, 
              family = gaussian, corstr = "exchangeable",
              maxiter = 100, na.action = na.omit,
              id = as.numeric(FID), silent = TRUE)
    coeff <- summary(gm)$coefficients
    E <- cbind(E, coeff[, 1])
    SE <- cbind(SE, coeff[, 4])
    Z <- cbind(Z, coeff[, 5])
    P <- cbind(P, 2*pnorm(-abs(coeff[, 5])))
    
    end <- Sys.time() - time_loop_start
    cat("Loop", i, "has ended \n"); print(end, quote = FALSE)
  }
  output <- list()
  output$E <- E
  output$SE <- SE
  output$Z <- Z
  output$P <- P
  output$N <- N
  
  end_full_model <- Sys.time() - start
  cat("Full model finished. \n"); print(end_full_model, quote = FALSE)
  return(output)
}

"gee_model3" <- function(expression_data, ses_var, data){
  E <- c(); SE <- c(); Z <- c(); P <- c()
  ind <- which(!is.na(data[, ses_var]))
  data <- data[ind, ]
  N <- nrow(data)
  
  expression_data <- expression_data[, ind]
  ses <- data[, ses_var]
  status <- data[, "status"]
  bmi <- data[, "bmi"]
  sex <- data[, "sex"]
  age <- data[, "age"]
  currentsmoke <- data[, "currentsmoke"]
  D <- data[, "D"]
  nday_ext_amp <- data[, "nday_ext_amp"]
  ndays <- data[, "ndays"]
  hour <- data[, "hour"]
  month <- data[, "month"]
  year <- data[, "year"]
  ht <- data[, "ht"]
  pt <- data[, "plate"] %>% as.character()
  IL6 <- data[, "bioil6"]
  CRP <- data[, "biocrp"]
  TNFa <- data[, "biotnfa"]
  ht <- data[, "ht"]
  baso <- data[, "baso"]
  ceo <- data[, "ceo"]
  lymp <- data[, "lymp"]
  mono <- data[, "mono"]
  neut <- data[, "neut"]
  FID <- data[, "FID"]
  
  start <- Sys.time()
  for(i in 1:nrow(expression_data)){ 
    time_loop_start <- Sys.time()
    cat("Loop", i, "starts at \n"); print(time_loop_start, quote = FALSE)
    
    gm <- gee(expression_data[i, ] ~ ses + status +
                bmi + sex + age +
                currentsmoke +   
                D + nday_ext_amp + ndays + 
                hour + month + year + 
                IL6 + CRP + TNFa +
                ht +
                baso + ceo + lymp +
                mono + neut +
                pt, 
              family = gaussian, corstr = "exchangeable",
              maxiter = 100, na.action = na.omit,
              id = as.numeric(FID), silent = TRUE)
    coeff <- summary(gm)$coefficients
    E <- cbind(E, coeff[, 1])
    SE <- cbind(SE, coeff[, 4])
    Z <- cbind(Z, coeff[, 5])
    P <- cbind(P, 2*pnorm(-abs(coeff[, 5])))
    
    end <- Sys.time() - time_loop_start
    cat("Loop", i, "has ended \n"); print(end, quote = FALSE)
  }
  output <- list()
  output$E <- E
  output$SE <- SE
  output$Z <- Z
  output$P <- P
  output$N <- N
  
  end_full_model <- Sys.time() - start
  cat("Full model finished. \n"); print(end_full_model, quote = FALSE)
  return(output)
}

# lm_F models
"lm_F_model0" <- function(expression_data, ses_var, data){
  ind <- which(!is.na(data[, ses_var]))
  data <- data[ind, ]
  expression_data <- expression_data[, ind]
  
  ses <- data[, ses_var] %>% unlist() %>% as.numeric()
  sex <- data[, "sex"] %>% unlist() %>% as.factor()
  age <- data[, "age"] %>% unlist() %>% as.numeric()

  D <- data[, "D"] %>% unlist() %>% as.numeric()
  nday_ext_amp <- data[, "nday_ext_amp"] %>% unlist() %>% as.numeric()
  ndays <- data[, "ndays"] %>% unlist() %>% as.numeric()
  hour <- data[, "hour"] %>% unlist() %>% as.numeric()
  month <- data[, "month"] %>% unlist() %>% as.factor()
  year <- data[, "year"] %>% unlist() %>% as.factor()
  lt <- data[, "lab"]  %>% unlist() %>% as.factor()
  pt <- data[, "plate"]  %>% unlist() %>% as.character()
  
  fm <- paste(" ~ ses + sex + age + D + nday_ext_amp + ndays + hour + month + year + lt + pt")
  
  options(na.action = "na.pass")
  X <- model.matrix(as.formula(fm))
  m <- rowMeans(X)
  nain <- which(!is.na(m))
  cat("Data contains", nrow(X), 
      "participants, with", length(which(is.na(m))), 
      "participants removed due to incomplete data. \n",
      "This results in N =", length(nain), "\n")
  
  options(na.action = "na.omit")
  X <- model.matrix(as.formula(fm))
  s <- apply(X, 2, sd)
  X <- X[, c(1, which(s != 0))] # remove empty factors
  cat("Data contains", length(which(s == 0)), "empty factors. These were removed.\n")
  
  start <- Sys.time()
  P1 <- lm_F(t(as.matrix(expression_data[, nain])), X, residuals = FALSE); # expr or gexpr data
  P1$N <- length(nain)
  end <- Sys.time() - start # just over 4 mins
  cat("Full model finished. \n"); print(end, quote = FALSE)
  
  return(P1)
}

"lm_F_model1" <- function(expression_data, ses_var, data){
  ind <- which(!is.na(data[, ses_var]))
  data <- data[ind, ]
  expression_data <- expression_data[, ind]

  ses <- data[, ses_var] %>% unlist() %>% as.numeric()
  status <- data[, "status"] %>% unlist() %>% as.factor()
  bmi <- data[, "bmi"] %>% unlist() %>% as.numeric()
  sex <- data[, "sex"] %>% unlist() %>% as.factor()
  age <- data[, "age"] %>% unlist() %>% as.numeric()
  currentsmoke <- data[, "currentsmoke"] %>% unlist() %>% as.factor()
  D <- data[, "D"] %>% unlist() %>% as.numeric()
  nday_ext_amp <- data[, "nday_ext_amp"] %>% unlist() %>% as.numeric()
  ndays <- data[, "ndays"] %>% unlist() %>% as.numeric()
  hour <- data[, "hour"] %>% unlist() %>% as.numeric()
  month <- data[, "month"] %>% unlist() %>% as.factor()
  year <- data[, "year"] %>% unlist() %>% as.factor()
  lt <- data[, "lab"]  %>% unlist() %>% as.factor()
  pt <- data[, "plate"]  %>% unlist() %>% as.character()

  fm <- paste(" ~ ses + status + bmi + sex + age + currentsmoke + D + nday_ext_amp + ndays + hour + month + year + lt + pt")
  
  options(na.action = "na.pass")
  X <- model.matrix(as.formula(fm))
  m <- rowMeans(X)
  nain <- which(!is.na(m))
  cat("Data contains", nrow(X), 
      "participants, with", length(which(is.na(m))), 
      "participants removed due to incomplete data. \n",
      "This results in N =", length(nain), "\n")
  
  options(na.action = "na.omit")
  X <- model.matrix(as.formula(fm))
  s <- apply(X, 2, sd)
  X <- X[, c(1, which(s != 0))] # remove empty factors
  cat("Data contains", length(which(s == 0)), "empty factors. These were removed.\n")
  
  start <- Sys.time()
  P1 <- lm_F(t(as.matrix(expression_data[, nain])), X, residuals = FALSE); # expr or gexpr data
  P1$N <- length(nain)
  end <- Sys.time() - start # just over 4 mins
  cat("Full model finished. \n"); print(end, quote = FALSE)
  
  return(P1)
}

"lm_F_model2" <- function(expression_data, ses_var, data){
  ind <- which(!is.na(data[, ses_var]))
  data <- data[ind, ]
  expression_data <- expression_data[, ind]

  ses <- data[, ses_var] %>% unlist() %>% as.numeric()
  
  status <- data[, "status"] %>% unlist() %>% as.factor()
  bmi <- data[, "bmi"] %>% unlist() %>% as.numeric()
  sex <- data[, "sex"] %>% unlist() %>% as.factor()
  age <- data[, "age"] %>% unlist() %>% as.numeric()
  currentsmoke <- data[, "currentsmoke"] %>% unlist() %>% as.factor()
  D <- data[, "D"] %>% unlist() %>% as.numeric()
  nday_ext_amp <- data[, "nday_ext_amp"] %>% unlist() %>% as.numeric()
  ndays <- data[, "ndays"] %>% unlist() %>% as.numeric()
  hour <- data[, "hour"] %>% unlist() %>% as.numeric()
  month <- data[, "month"] %>% unlist() %>% as.factor()
  year <- data[, "year"] %>% unlist() %>% as.factor()
  lt <- data[, "lab"]  %>% unlist() %>% as.factor()
  IL6 <- data[, "aIL6"] %>% unlist() %>% as.numeric()
  CRP <- data[, "ahsCRP"] %>% unlist() %>% as.numeric()
  TNFa <- data[, "aTNFa"] %>% unlist() %>% as.numeric()
  pt <- data[, "plate"]  %>% unlist() %>% as.character()

  fm <- paste(" ~ ses + status + bmi + sex + age + currentsmoke + D + nday_ext_amp + ndays + hour + month + year + lt + IL6 + CRP + TNFa + pt")
  
  options(na.action = "na.pass")
  X <- model.matrix(as.formula(fm))
  m <- rowMeans(X)
  nain <- which(!is.na(m))
  cat("Data contains", nrow(X), 
      "participants, with", length(which(is.na(m))), 
      "participants removed due to incomplete data. \n",
      "This results in N =", length(nain), "\n")
  
  options(na.action = "na.omit")
  X <- model.matrix(as.formula(fm))
  s <- apply(X, 2, sd)
  X <- X[, c(1, which(s != 0))] # remove empty factors
  cat("Data contains", length(which(s == 0)), "empty factors. These were removed.\n")
  
  start <- Sys.time()
  P1 <- lm_F(t(as.matrix(expression_data[, nain])), X, residuals = FALSE); # expr or gexpr data
  P1$N <- length(nain)
  end <- Sys.time() - start # just over 4 mins
  cat("Full model finished. \n"); print(end, quote = FALSE)
  
  return(P1)
}
# Weighted Z scores for meta-analysis P-values
"weighted_Z_scores" <- function(NTRmodelP, NESDAmodelP,
                                NTRmodelN, NESDAmodelN){
  P <- cbind(NTRmodelP[2, ], NESDAmodelP[2, ]) # p1 = p-values NTR, p2 = p-values NESDA
  W <- c(NTRmodelN, NESDAmodelN)
  
  Zi <- qnorm(1 - P)
  Zi[, 1] <- Zi[, 1] * W[1]
  Zi[, 2] <- Zi[, 2] * W[2]
  Z  <- rowSums(Zi) / sqrt(sum(W^2))
  p_meta <- 1 - pnorm(Z)
  
  return(p_meta)
}
# Weighted Z scores for meta-analysis original Z-scores
"weighted_Z_scores_orig" <- function(NTRmodelZ, NESDAmodelZ,
                                NTRmodelN, NESDAmodelN){
  #P <- cbind(NTRmodelP[2, ], NESDAmodelP[2, ]) # p1 = p-values NTR, p2 = p-values NESDA
  W <- c(NTRmodelN, NESDAmodelN)
  
  #Zi <- qnorm(1 - P)
  Zi <- cbind(NTRmodelZ[2, ], NESDAmodelZ[2, ])
  Zi[, 1] <- Zi[, 1] * W[1]
  Zi[, 2] <- Zi[, 2] * W[2]
  Z  <- rowSums(Zi) / sqrt(sum(W^2))
  p_meta <- 1 - pnorm(Z)
  
  return(p_meta)
}
# meta analysis
"p_meta_analysis" <- function(NTRmodelP, NESDAmodelP,
                              NTRmodelE, NESDAmodelE,
                              NTRmodelSE, NESDAmodelSE,
                              NTRmodelN, NESDAmodelN,
                              P_to_META = c("p_Zscore_weighted", 
                                            "p_fischer", 
                                            "p_wilkinson")){
  
  model_meta_p <- tibble(
    gene_name = gene_names,
    chisq = rep(NA, ncol(NTRmodelP)),
    p_fisher = rep(NA, ncol(NTRmodelP)),
    p_wilkinson = rep(NA, ncol(NTRmodelP)),
    p_Zscore_weighted = rep(NA, ncol(NTRmodelP)),
    Q_value = rep(NA, ncol(NTRmodelP)),
    E_concordant = rep(NA, ncol(NTRmodelP)),
    NTR_E = NTRmodelE[2,],
    NTR_SE = NTRmodelSE[2,],
    NESDA_E = NESDAmodelE[2,],
    NESDA_SE = NESDAmodelSE[2,]
  ) %>% as.data.frame()
  
  model_meta_p[, "p_Zscore_weighted"] <- weighted_Z_scores(NTRmodelP, NESDAmodelP,
                                                           NTRmodelN, NESDAmodelN)
                                
  for (i in 1:ncol(NTRmodelP)){
    model_meta_p[i, "chisq"] <- sumlog(c(NTRmodelP[2, i], 
                                         NESDAmodelP[2, i]))[1] %>% as.numeric()
    model_meta_p[i, "p_fisher"] <- sumlog(c(NTRmodelP[2, i], 
                                            NESDAmodelP[2, i]))[3] %>% as.numeric()
    model_meta_p[i, "p_wilkinson"] <- minimump(c(NTRmodelP[2, i],  # Tippett
                                                 NESDAmodelP[2, i]), alpha = .05)$p
    
    if(NTRmodelE[2, i] > 0 & NESDAmodelE[2, i] > 0){
      model_meta_p[i, "E_concordant"] <- "YES"
    } else if(NTRmodelE[2, i] < 0 & NESDAmodelE[2, i] < 0){
      model_meta_p[i, "E_concordant"] <- "YES"
    } else{
      model_meta_p[i, "E_concordant"] <- "NO"
    }
  }
  
  # construct Q_values of on P-value variable of choice
  model_meta_p[, "Q_value"] <- p.adjust(model_meta_p[, P_to_META], "fdr")
  
  return(model_meta_p)
}


# return significant genes
"find_sig" <- function(model, fdr, model_origin,
                       NTR_E = NA, NTR_SE = NA,
                       NESDA_E = NA, NESDA_SE = NA,
                       P_to_META = c("p_Zscore_weighted", 
                                     "p_fischer", 
                                     "p_wilkinson")){
  
  if(model_origin == "NTR" | model_origin == "NESDA"){
     
    position_in_data <- which(p.adjust(model[2, ], "fdr") < fdr)
    gene_name <- gene_names[position_in_data]
    ens_name <- ens_gene_names[position_in_data]
    probeset_id <- probeset_id[position_in_data]
    MB <- MB[position_in_data]
    P_value <- model[2, ][position_in_data]
    Q_value <- p.adjust(model[2, ], "fdr")[position_in_data]
    
    if(model_origin == "NTR"){
      Estimate <- NTR_E[2, position_in_data]
      StandardError <- NTR_SE[2, position_in_data]
      } else if(model_origin == "NESDA"){
        Estimate <- NESDA_E[2, position_in_data]
        StandardError <- NESDA_SE[2, position_in_data]
        }
    
    
    output <- cbind.data.frame(position_in_data, gene_name, ens_name, 
                               probeset_id, MB, P_value, Q_value,
                               Estimate, StandardError)
    
  } else if(model_origin == "META"){
    
    position_in_data <- which(p.adjust(model[, P_to_META], "fdr") < fdr)
    gene_name <- gene_names[position_in_data]
    ens_name <- ens_gene_names[position_in_data]
    probeset_id <- probeset_id[position_in_data]
    MB <- MB[position_in_data]
    concordant_E <- model[position_in_data, "E_concordant"]
    P_value <- model[, P_to_META][position_in_data]
    Q_value <- p.adjust(model[, P_to_META], "fdr")[position_in_data]
    
    NTR_E <- NTR_E[2, position_in_data]
    NTR_SE <- NTR_SE[2, position_in_data]
    NESDA_E <- NESDA_E[2, position_in_data]
    NESDA_SE <- NESDA_SE[2, position_in_data]
    
    output <- cbind.data.frame(position_in_data, gene_name, ens_name, 
                               probeset_id, MB, concordant_E, P_value, Q_value,
                               NTR_E, NTR_SE, NESDA_E, NESDA_SE)
  }
  return(output)
}

"output_meta_fdr" <- function(NTR_P, NESDA_P,
                              NTR_E, NESDA_E,
                              NTR_SE, NESDA_SE,
                              NTR_N, NESDA_N,
                              varname, modelname,
                              P_to_META = c("p_Zscore_weighted", 
                                             "p_fischer", 
                                             "p_wilkinson")){
                                
  model_fdr_05_NTR <- find_sig(NTR_P, .05, "NTR",
                               NTR_E = NTR_E, NTR_SE = NTR_SE,
                               P_to_META = NA)
  model_fdr_1_NTR <- find_sig(NTR_P, .1, "NTR",
                              NTR_E = NTR_E, NTR_SE = NTR_SE,
                              P_to_META = NA)
  model_fdr_05_NESDA <- find_sig(NESDA_P, .05, "NESDA",
                                 NESDA_E = NESDA_E, NESDA_SE = NESDA_SE,
                                 P_to_META = NA)
  model_fdr_1_NESDA <- find_sig(NESDA_P, .1, "NESDA",
                                NESDA_E = NESDA_E, NESDA_SE = NESDA_SE,
                                P_to_META = NA)
  
  META_P <- p_meta_analysis(NTR_P, NESDA_P,
                            NTR_E, NESDA_E,
                            NTR_SE, NESDA_SE,
                            NTR_N, NESDA_N,
                            P_to_META = P_to_META)
  model_fdr_05_META <- find_sig(META_P, .05, "META",
                                NTR_E, NTR_SE,
                                NESDA_E, NESDA_SE,
                                P_to_META = P_to_META)
  model_fdr_1_META <- find_sig(META_P, .1, "META",
                               NTR_E, NTR_SE,
                               NESDA_E, NESDA_SE,
                               P_to_META = P_to_META)
  
  meta_top100 <- cbind.data.frame(ens_gene_names, gene_names, 
                                  probeset_id, MB,
                                  META_P[, P_to_META]) %>% filter(., ens_gene_names != "" )
  colnames(meta_top100)[6] <- "P_value"
  order(meta_top100[, "P_value"])
  meta_top100 <- meta_top100[order(meta_top100[, "P_value"]), ]
  meta_top100_unique <- meta_top100[!duplicated(meta_top100[, c("ens_gene_names")]), ][c(1:100),]
  
  pastename <- paste0("./data/models/top100_lists/", modelname, ".txt")
  list <- meta_top100_unique[, "ens_gene_names"] %>% as.data.frame()
  write.table(list, file = pastename, 
              quote = FALSE,
              row.names = FALSE, col.names = FALSE)
  
  #hist_NTR_P <- hist(NTR_P[2,], 100)
  #hist_NESDA_P <- hist(NESDA_P[2,], 100)
  #hist_META_P <- hist(META_P[, P_to_META], 100)
  
  model <- cbind.data.frame(c(1:44241),
                            NTR_P[2, ],
                            NESDA_P[2, ],
                            META_P[ , P_to_META])
  colnames(model) <- c("genenr", "NTR", "NESDA", "META")
  model <- model %>% melt(id.vars = "genenr", measure.vars = c("NTR", "NESDA", "META"))
  hist <- model %>% ggplot(., aes(x = value, fill = variable)) +
    ggtitle(label = paste0("Histogram P-values ", varname))+
    xlab(label = "P-values") +
    ylab(label = "Count") +
    theme(
      plot.title = element_text(hjust = .5),
      panel.background = element_rect(fill = "grey85")) +
    geom_histogram(#color = "black",
                   alpha = .3,
                   position = "identity",
                   binwidth = .01) +
    scale_fill_manual(name = "P-values from:",
                      values = c("firebrick", "green", "blue"))
  
  output <- list()
  output$fdr_05_NTR <- model_fdr_05_NTR
  output$fdr_1_NTR <- model_fdr_1_NTR
  output$fdr_05_NESDA <- model_fdr_05_NESDA
  output$fdr_1_NESDA <- model_fdr_1_NESDA
  output$fdr_05_META <- model_fdr_05_META
  output$fdr_1_META <- model_fdr_1_META
  output$META_P <- META_P
  #output$hist_NTR_P <- hist_NTR_P
  #output$hist_NESDA_P <- hist_NESDA_P
  #output$hist_META_P <- hist_META_P
  output$hist <- hist
  output$p_100 <- meta_top100_unique
  
  return(output)
}

"output_ntr_fdr" <- function(NTR_P, NTR_E, NTR_SE, 
                             varname, modelname){
  model_fdr_05_NTR <- find_sig(NTR_P, .05, "NTR",
                               NTR_E = NTR_E, NTR_SE = NTR_SE,
                               P_to_META = NA)
  model_fdr_1_NTR <- find_sig(NTR_P, .1, "NTR",
                              NTR_E = NTR_E, NTR_SE = NTR_SE,
                              P_to_META = NA)
  
  ntr_top100 <- cbind.data.frame(ens_gene_names, gene_names, 
                                  probeset_id, MB,
                                  NTR_P[2, ]) %>% filter(., ens_gene_names != "" )
  colnames(ntr_top100)[6] <- "P_value"
  order(ntr_top100[, "P_value"])
  ntr_top100 <- ntr_top100[order(ntr_top100[, "P_value"]), ]
  ntr_top100_unique <- ntr_top100[!duplicated(ntr_top100[, c("ens_gene_names")]), ][c(1:100),]
  
  pastename <- paste0("./data/models/top100_lists/", modelname, ".txt")
  list <- ntr_top100_unique[, "ens_gene_names"] %>% as.data.frame()
  write.table(list, file = pastename, 
              quote = FALSE,
              row.names = FALSE, col.names = FALSE)
  
  model <- cbind.data.frame(c(1:44241),
                            NTR_P[2,])
  colnames(model) <- c("genenr", "NTR")
  hist <- model %>% ggplot(., aes(x = NTR)) +
    ggtitle(label = paste0("Histogram P-values ", varname))+
    xlab(label = "P-values") +
    ylab(label = "Count") +
    theme(
      plot.title = element_text(hjust = .5),
      panel.background = element_rect(fill = "grey85")) +
    geom_histogram(color = "black",
      alpha = .3,
      position = "identity",
      binwidth = .01,
      fill = "firebrick") #+
    #scale_fill_manual(name = "P-values from:",
    #                  values = c("firebrick"))
  
  output <- list()
  output$fdr_05_NTR <- model_fdr_05_NTR
  output$fdr_1_NTR <- model_fdr_1_NTR
  #output$hist_NTR_P <- hist_NTR_P
  #output$hist_NESDA_P <- hist_NESDA_P
  #output$hist_META_P <- hist_META_P
  output$hist <- hist
  output$p_100 <- ntr_top100_unique
  
  return(output)
}
# make supplementary data
"make_supp_data" <- function(modelname, meta = TRUE,
                             model0, model1, 
                             model2, model3){
  
  if(!dir.exists(paste0("./data/SuppData/", modelname))){
    dir.create(paste0("./data/SuppData/", modelname))
  }
  
  start <- Sys.time()
  if(meta == TRUE){
    list_of_models0 <- list("Model 0 NESDA E" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model0_", modelname, "_NESDA_E"))[2, ]),
                            "Model 0 NESDA SE" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model0_", modelname, "_NESDA_SE"))[2, ]),
                            "Model 0 NESDA Z" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model0_", modelname, "_NESDA_Z"))[2, ]),
                            "Model 0 NESDA Pval" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model0_", modelname, "_NESDA_P"))[2, ]),
                            "Model 0 NTR E" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model0_", modelname, "_NTR_E"))[2, ]), 
                            "Model 0 NTR SE" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model0_", modelname, "_NTR_SE"))[2, ]),
                            "Model 0 NTR Z" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model0_", modelname, "_NTR_Z"))[2, ]),
                            "Model 0 NTR Pval" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model0_", modelname, "_NTR_P"))[2, ]) )
    list_of_models1 <- list("Model 1 NESDA E" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model1_", modelname, "_NESDA_E"))[2, ]),
                            "Model 1 NESDA SE" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model1_", modelname, "_NESDA_SE"))[2, ]),
                            "Model 1 NESDA Z" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model1_", modelname, "_NESDA_Z"))[2, ]),
                            "Model 1 NESDA Pval" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model1_", modelname, "_NESDA_P"))[2, ]),
                            "Model 1 NTR E" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model1_", modelname, "_NTR_E"))[2, ]), 
                            "Model 1 NTR SE" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model1_", modelname, "_NTR_SE"))[2, ]),
                            "Model 1 NTR Z" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model1_", modelname, "_NTR_Z"))[2, ]),
                            "Model 1 NTR Pval" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model1_", modelname, "_NTR_P"))[2, ]) )
    list_of_models2 <- list("Model 2 NESDA E" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model2_", modelname, "_NESDA_E"))[2, ]),
                            "Model 2 NESDA SE" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model2_", modelname, "_NESDA_SE"))[2, ]),
                            "Model 2 NESDA Z" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model2_", modelname, "_NESDA_Z"))[2, ]),
                            "Model 2 NESDA Pval" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model2_", modelname, "_NESDA_P"))[2, ]),
                            "Model 2 NTR E" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model2_", modelname, "_NTR_E"))[2, ]), 
                            "Model 2 NTR SE" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model2_", modelname, "_NTR_SE"))[2, ]),
                            "Model 2 NTR Z" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model2_", modelname, "_NTR_Z"))[2, ]),
                            "Model 2 NTR Pval" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model2_", modelname, "_NTR_P"))[2, ]) )
    list_of_models3 <- list("Model 3 NTR E" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model3_", modelname, "_NTR_E"))[2, ]), 
                            "Model 3 NTR SE" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model3_", modelname, "_NTR_SE"))[2, ]),
                            "Model 3 NTR Z" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model3_", modelname, "_NTR_Z"))[2, ]),
                            "Model 3 NTR Pval" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model3_", modelname, "_NTR_P"))[2, ]) )
    
    write.xlsx(list_of_models0, file = paste0("./data/SuppData/",
                                              modelname, "/SuppData_", 
                                              modelname, "_models", "0",
                                              "_NTR_NESDA.xlsx"),
               col.names = TRUE, row.names = TRUE,
               overwrite = TRUE)
    write.xlsx(list_of_models1, file = paste0("./data/SuppData/",
                                              modelname, "/SuppData_", 
                                              modelname, "_models", "1",
                                              "_NTR_NESDA.xlsx"),
               col.names = TRUE, row.names = TRUE,
               overwrite = TRUE)
    write.xlsx(list_of_models2, file = paste0("./data/SuppData/",
                                              modelname, "/SuppData_", 
                                              modelname, "_models", "2",
                                              "_NTR_NESDA.xlsx"),
               col.names = TRUE, row.names = TRUE,
               overwrite = TRUE)
    write.xlsx(list_of_models3, file = paste0("./data/SuppData/",
                                              modelname, "/SuppData_", 
                                              modelname, "_models", "3",
                                              "_NTR_NESDA.xlsx"),
               col.names = TRUE, row.names = TRUE,
               overwrite = TRUE)
    
    list_of_datasets2 <- list("Model 0 NTR FDR05" = model0$fdr_05_NTR,
                              "Model 0 NTR FDR10" = model0$fdr_1_NTR,
                              "Model 0 NESDA FDR05" = model0$fdr_05_NESDA,
                              "Model 0 NESDA FDR10" = model0$fdr_1_NESDA,
                              "Model 0 META FDR05" = model0$fdr_05_META,
                              "Model 0 META FDR10" = model0$fdr_1_META,
                              "Model 0 META P" = model0$META_P,
                              "Model 0 META top 100" = model0$p_100,
                              "Model 1 NTR FDR05" = model1$fdr_05_NTR,
                              "Model 1 NTR FDR10" = model1$fdr_1_NTR,
                              "Model 1 NESDA FDR05" = model1$fdr_05_NESDA,
                              "Model 1 NESDA FDR10" = model1$fdr_1_NESDA,
                              "Model 1 META FDR05" = model1$fdr_05_META,
                              "Model 1 META FDR10" = model1$fdr_1_META,
                              "Model 1 META P" = model1$META_P,
                              "Model 1 META top 100" = model1$p_100,
                              "Model 2 NTR FDR05" = model2$fdr_05_NTR,
                              "Model 2 NTR FDR10" = model2$fdr_1_NTR,
                              "Model 2 NESDA FDR05" = model2$fdr_05_NESDA,
                              "Model 2 NESDA FDR10" = model2$fdr_1_NESDA,
                              "Model 2 META FDR05" = model2$fdr_05_META,
                              "Model 2 META FDR10" = model2$fdr_1_META,
                              "Model 2 META P" = model2$META_P,
                              "Model 2 META top 100" = model2$p_100,
                              "Model 3 NTR FDR05" = model3$fdr_05_NTR,
                              "Model 3 NTR FDR10" = model3$fdr_1_NTR,
                              "Model 3 NTR top 100" = model3$p_100) 
    end_name <- "_models_META.xlsx"
    
  } else if(meta == FALSE){
    list_of_models0 <- list("Model 0 NTR E" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model0_", modelname, "_NTR_E"))[2, ]), 
                            "Model 0 NTR SE" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model0_", modelname, "_NTR_SE"))[2, ]),
                            "Model 0 NTR Z" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model0_", modelname, "_NTR_Z"))[2, ]),
                            "Model 0 NTR Pval" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model0_", modelname, "_NTR_P"))[2, ]) )
    list_of_models1 <- list("Model 1 NTR E" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model1_", modelname, "_NTR_E"))[2, ]), 
                            "Model 1 NTR SE" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model1_", modelname, "_NTR_SE"))[2, ]),
                            "Model 1 NTR Z" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model1_", modelname, "_NTR_Z"))[2, ]),
                            "Model 1 NTR Pval" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model1_", modelname, "_NTR_P"))[2, ]) )
    list_of_models2 <- list("Model 2 NTR E" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model2_", modelname, "_NTR_E"))[2, ]), 
                            "Model 2 NTR SE" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model2_", modelname, "_NTR_SE"))[2, ]),
                            "Model 2 NTR Z" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model2_", modelname, "_NTR_Z"))[2, ]),
                            "Model 2 NTR Pval" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model2_", modelname, "_NTR_P"))[2, ]) )
    list_of_models3 <- list("Model 3 NTR E" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model3_", modelname, "_NTR_E"))[2, ]), 
                            "Model 3 NTR SE" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model3_", modelname, "_NTR_SE"))[2, ]),
                            "Model 3 NTR Z" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model3_", modelname, "_NTR_Z"))[2, ]),
                            "Model 3 NTR Pval" = cbind(probeset_id, gene_names, ens_gene_names, get(paste0("model3_", modelname, "_NTR_P"))[2, ]) )
    
    write.xlsx(list_of_models0, file = paste0("./data/SuppData/",
                                              modelname, "/SuppData_", 
                                              modelname, "_models", "0",
                                              "_NTR.xlsx"),
               col.names = TRUE, row.names = TRUE,
               overwrite = TRUE)
    write.xlsx(list_of_models1, file = paste0("./data/SuppData/",
                                              modelname, "/SuppData_", 
                                              modelname, "_models", "1",
                                              "_NTR.xlsx"),
               col.names = TRUE, row.names = TRUE,
               overwrite = TRUE)
    write.xlsx(list_of_models2, file = paste0("./data/SuppData/",
                                              modelname, "/SuppData_", 
                                              modelname, "_models", "2",
                                              "_NTR.xlsx"),
               col.names = TRUE, row.names = TRUE,
               overwrite = TRUE)
    write.xlsx(list_of_models3, file = paste0("./data/SuppData/",
                                              modelname, "/SuppData_", 
                                              modelname, "_models", "3",
                                              "_NTR.xlsx"),
               col.names = TRUE, row.names = TRUE,
               overwrite = TRUE)
    
    list_of_datasets2 <- list("Model 0 NTR FDR05" = model0$fdr_05_NTR,
                              "Model 0 NTR FDR10" = model0$fdr_1_NTR,
                              "Model 0 NTR top 100" = model0$p_100,
                              "Model 1 NTR FDR05" = model1$fdr_05_NTR,
                              "Model 1 NTR FDR10" = model1$fdr_1_NTR,
                              "Model 1 NTR top 100" = model1$p_100,
                              "Model 2 NTR FDR05" = model2$fdr_05_NTR,
                              "Model 2 NTR FDR10" = model2$fdr_1_NTR,
                              "Model 2 NTR top 100" = model2$p_100,
                              "Model 3 NTR FDR05" = model3$fdr_05_N,
                              "Model 3 NTR FDR10" = model3$fdr_1_N,
                              "Model 3 NTR top 100" = model3$p_100)
    end_name <- "_models_NTR.xlsx"
  }
  
  write.xlsx(list_of_datasets2, file = paste0("./data/SuppData/SuppData_", 
                                              modelname, 
                                              end_name),
             col.names = TRUE, row.names = TRUE,
             overwrite = TRUE)
  
  end_full_model <- Sys.time() - start
  cat("Done writing to Excel files. \n"); print(end_full_model, quote = FALSE)
  
  
}
