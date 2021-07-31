### libraries
library("tidyverse")
library("data.table")
library("scales")
library("cowplot")
#library("stringr")
library("openxlsx")


### custom functions
"%!in%" <- function(x, y) {!("%in%"(x, y))}

# cut up long labels x axis
"addline_format" <- function(x,...){
  gsub("\\s", "\n", x)
}

# find eqtls
"eQTL_finder" <- function(eQTLdata, eQTL = c("cis", "trans"), 
                          chr_var = "SNPChr", Z_var = "Zscore", 
                          gene_var = "Gene", snp_var = "SNP"){
  lead_SNP <- c()
  eQTL_lead <- c()
  
  eQTLdata <- eQTLdata %>%
    mutate(Zscore_abs = abs(.[, Z_var]))
  
  if (eQTL == "cis") {
    gene_looped_over <- c()
    
    for (gene in eQTLdata[, gene_var]) {
      if (!(gene %in% gene_looped_over)) {
        start_for_loop_cis <- Sys.time()
        cat("Loop", (length(gene_looped_over)+1), "starts at \n"); print(start_for_loop_cis, quote = FALSE)
        
        eQTLdata_subset <- eQTLdata[which(eQTLdata[, gene_var] == gene), ]
        lead_SNP <- eQTLdata_subset[which.max(eQTLdata_subset[, "Zscore_abs"]), snp_var]
        
        gene_looped_over <- c(gene_looped_over, gene)
        
        eQTL_lead_add <- eQTLdata_subset %>%
          filter(., .[, snp_var] %in% lead_SNP)
        
        eQTL_lead <- rbind(eQTL_lead, eQTL_lead_add)
      }
      end <- Sys.time() - start_for_loop_cis
      cat("Loop", length(gene_looped_over), "has ended \n"); print(end, quote = FALSE)
    }
  } else
    if (eQTL == "trans"){
      gene_looped_over <- c()
      
      for (gene in eQTLdata[, gene_var]) {
        if (!(gene %in% gene_looped_over)) {
          start_for_loop_trans <- Sys.time()
          cat("Loop", (length(gene_looped_over)+1), "starts at \n"); print(start_for_loop_trans, quote = FALSE)
          
          eQTLdata_subset <- eQTLdata[which(eQTLdata[, gene_var] == gene), ]
          chrom_looped_over <- c()
          
          for (chrom in eQTLdata_subset[, chr_var]){
            if (!(chrom %in% chrom_looped_over)){
              eQTLdata_subset_chr <- eQTLdata_subset[which(eQTLdata_subset[, chr_var] == chrom), ]
              
              lead_SNP <- eQTLdata_subset_chr[which.max(eQTLdata_subset_chr[, "Zscore_abs"]), snp_var]
              
              chrom_looped_over <- c(chrom_looped_over, chrom)
              
              eQTL_lead_add <- eQTLdata_subset_chr %>%
                filter(., .[, snp_var] %in% lead_SNP)
              eQTL_lead <- rbind(eQTL_lead, eQTL_lead_add)
            }
            
            gene_looped_over <- c(gene_looped_over, gene)
          }
          end <- Sys.time() - start_for_loop_trans
          cat("Loop", length(gene_looped_over), "has ended \n"); print(end, quote = FALSE)
        }
      }
    }
  else {
    cat("No bueno capitan")
  }
  return(eQTL_lead)
}

## figures FUMA
# Expression heatmaps
"plot_avg_exp" <- function(data, xvar_type = c("age", "dev", "tissue", "tissue_general"), 
                           title) {
  data <- melt(data, id.vars = c("ensg", "symbol"))
  
  if(xvar_type == "dev"){
    levels(data$variable) <- c("Early prenatal", "Early mid-prenatal", "Late mid-prenatal", 
                               "Late prenatal", "Early infancy", "Late infancy", "Early childhood",
                               "Late childhood", "Adolescence", "Young adulthood", "Middle adulthood")
    xlab <- "Developmental Period"
    angle <- 25
    size_labels_x_axis <- 9
    
    
  } else if(xvar_type == "age"){
    levels(data$variable) <- c("8 pcw", "9 pcw", "12 pcw", "13 pcw", "16 pcw", "17 pcw", "19 pcw", "21 pcw", 
                               "24 pcw", "26 pcw", "37 pcw", "4 mos", "10 mos", "1 yrs", 
                               "2 yrs", "3 yrs", "4 yrs", "8 yrs", "11 yrs", "13 yrs", "15 yrs", "18 yrs", 
                               "19 yrs", "21 yrs", "23 yrs", "30 yrs", "36 yrs", "37 yrs",
                               "40 yrs")
    xlab <- "Age"
    angle <- 25
    size_labels_x_axis <- 9
    
    
  }  else if(xvar_type == "tissue"){
    data$variable <- data$variable %>% factor(., levels = c("Adipose_Subcutaneous", "Adipose_Visceral_Omentum", "Adrenal_Gland",
                                                            "Artery_Aorta", "Artery_Coronary", "Artery_Tibial", 
                                                            "Bladder", "Brain_Amygdala", "Brain_Anterior_cingulate_cortex_BA24",
                                                            "Brain_Caudate_basal_ganglia", "Brain_Cerebellar_Hemisphere", "Brain_Cerebellum",                    
                                                            "Brain_Cortex", "Brain_Frontal_Cortex_BA9", "Brain_Hippocampus",               
                                                            "Brain_Hypothalamus", "Brain_Nucleus_accumbens_basal_ganglia", "Brain_Putamen_basal_ganglia", 
                                                            "Brain_Spinal_cord_cervical_c-1", "Brain_Substantia_nigra", "Breast_Mammary_Tissue", 
                                                            "Cells_Cultured_fibroblasts", "Cells_EBV-transformed_lymphocytes", "Cervix_Ectocervix", 
                                                            "Cervix_Endocervix", "Colon_Sigmoid", "Colon_Transverse",                     
                                                            "Esophagus_Gastroesophageal_Junction", "Esophagus_Mucosa", "Esophagus_Muscularis",
                                                            "Fallopian_Tube", "Heart_Atrial_Appendage", "Heart_Left_Ventricle",
                                                            "Kidney_Cortex", "Kidney_Medulla", "Liver",
                                                            "Lung", "Minor_Salivary_Gland", "Muscle_Skeletal", "Nerve_Tibial", "Ovary", "Pancreas",
                                                            "Pituitary", "Prostate", "Skin_Not_Sun_Exposed_Suprapubic", 
                                                            "Skin_Sun_Exposed_Lower_leg", "Small_Intestine_Terminal_Ileum", "Spleen", 
                                                            "Stomach", "Testis", "Thyroid", 
                                                            "Uterus", "Vagina", "Whole_Blood"))
    
    levels(data$variable) <- c("Adipose Subcutaneous", "Adipose Visceral Omentum", "Adrenal Gland",
                               "Artery Aorta", "Artery Coronary", "Artery Tibial", 
                               "Bladder", "Brain Amygdala", "Brain Anterior cingulate cortex BA24",
                               "Brain Caudate basal ganglia", "Brain Cerebellar Hemisphere", "Brain Cerebellum",                    
                               "Brain Cortex", "Brain Frontal Cortex BA9", "Brain Hippocampus",               
                               "Brain Hypothalamus", "Brain Nucleus accumbens basal ganglia", "Brain Putamen basal ganglia", 
                               "Brain Spinal cord cervical c-1", "Brain Substantia nigra", "Breast Mammary Tissue", 
                               "Cells Cultured fibroblasts", "Cells EBV-transformed lymphocytes", "Cervix Ectocervix", 
                               "Cervix Endocervix", "Colon Sigmoid", "Colon Transverse",                     
                               "Esophagus Gastroesophageal Junction", "Esophagus Mucosa", "Esophagus Muscularis",
                               "Fallopian Tube", "Heart Atrial Appendage", "Heart Left Ventricle",
                               "Kidney Cortex", "Kidney Medulla", "Liver",
                               "Lung", "Minor Salivary Gland", "Muscle Skeletal", "Nerve Tibial", "Ovary", "Pancreas",
                               "Pituitary", "Prostate", "Skin Not Sun Exposed Suprapubic", 
                               "Skin Sun Exposed Lower leg", "Small Intestine Terminal Ileum", "Spleen", 
                               "Stomach", "Testis", "Thyroid", 
                               "Uterus", "Vagina", "Whole Blood")
    size_labels_x_axis <- 8
    xlab <- "Tissue Type"
    angle <- 55
    
    
  } else if(xvar_type == "tissue_general"){
    data$variable <- data$variable %>% factor(., levels = c("Adipose_Tissue", "Adrenal_Gland", "Bladder", "Blood", "Blood_Vessel", "Brain", "Breast", "Cervix_Uteri", 
                                                            "Colon", "Esophagus", "Fallopian_Tube", "Heart", "Kidney", "Liver", "Lung", "Muscle", 
                                                            "Nerve", "Ovary", "Pancreas", "Pituitary", "Prostate", "Salivary_Gland", "Skin", "Small_Intestine", 
                                                            "Spleen", "Stomach", "Testis", "Thyroid", "Uterus", "Vagina"))
    levels(data$variable) <- c("Adipose Tissue", "Adrenal Gland", "Bladder", "Blood", "Blood Vessel", "Brain", "Breast", "Cervix Uteri", 
                               "Colon", "Esophagus", "Fallopian Tube", "Heart", "Kidney", "Liver", "Lung", "Muscle", 
                               "Nerve", "Ovary", "Pancreas", "Pituitary", "Prostate", "Salivary Gland", "Skin", "Small Intestine", 
                               "Spleen", "Stomach", "Testis", "Thyroid", "Uterus", "Vagina")
    #size_labels_x_axis <- 8
    xlab <- "General Tissue Type"
    angle <- 25
    size_labels_x_axis <- 8
    
    
  }
  
  data %>% 
    ggplot(aes(variable, factor(symbol, levels = rev(levels(factor(symbol)))))) + 
    geom_tile(aes(fill = value), colour = "white") +
    scale_fill_gradient2(name = "Expression",
                         low = "red", mid = "white", high = "blue") +#,
    # midpoint = 0) +
    labs(x = xlab,
         y = "Gene",
         title = title) +
    theme(
      plot.title = element_text(size = 15, 
                                face = "bold", 
                                margin = margin(10, 0, 10, 0),
                                hjust = .5),
      panel.background = element_rect(fill = "white"),
      axis.text.x = element_text(angle = angle, vjust = 1, hjust = 1, size = size_labels_x_axis))#,
  # axis.text.y = element_text(angle = 10, vjust = 0, hjust = 1))
  
}

# DEG
"plot_DEG" <- function(data,  xvar_type = c("age", "dev", "tissue", "tissue_general"), 
                       title, showlegend = FALSE){
  data$Category <- data$Category %>% factor()
  levels(data$Category) <- c("Down-regulated", "Twosided", "Up-regulated")
  
  
  if(xvar_type == "dev"){
    data$GeneSet <- data$GeneSet %>% factor(., levels = c("Early_prenatal", "Early_mid-prenatal", "Late_mid-prenatal", 
                                                          "Late_prenatal", "Early_infancy", "Late_infancy", "Early_childhood",
                                                          "Late_childhood", "Adolescence", "Young_adulthood", "Middle_adulthood"))
    levels(data$GeneSet) <- c("Early prenatal", "Early mid-prenatal", "Late mid-prenatal", 
                              "Late prenatal", "Early infancy", "Late infancy", "Early childhood",
                              "Late childhood", "Adolescence", "Young adulthood", "Middle adulthood")
    size_labels_x_axis <- 9
    xlab <- "Developmental Period"
    angle <- 25
    
    
    
  } else if(xvar_type == "age"){
    data$GeneSet <- data$GeneSet %>% factor(., levels = c("8_pcw", "9_pcw", "12_pcw", "13_pcw", "16_pcw", "17_pcw", "19_pcw", "21_pcw", 
                                                          "24_pcw", "26_pcw", "37_pcw", "4_mos", "10_mos", "1_yrs", 
                                                          "2_yrs", "3_yrs", "4_yrs", "8_yrs", "11_yrs", "13_yrs", "15_yrs", "18_yrs", 
                                                          "19_yrs", "21_yrs", "23_yrs", "30_yrs", "36_yrs", "37_yrs",
                                                          "40_yrs"))
    levels(data$GeneSet) <- c("8 pcw", "9 pcw", "12 pcw", "13 pcw", "16 pcw", "17 pcw", "19 pcw", "21 pcw", 
                              "24 pcw", "26 pcw", "37 pcw", "4 mos", "10 mos", "1 yrs", 
                              "2 yrs", "3 yrs", "4 yrs", "8 yrs", "11 yrs", "13 yrs", "15 yrs", "18 yrs", 
                              "19 yrs", "21 yrs", "23 yrs", "30 yrs", "36 yrs", "37 yrs",
                              "40 yrs")
    size_labels_x_axis <- 9
    xlab <- "Age"
    angle <- 35
    
    
  } else if(xvar_type == "tissue"){
    data$GeneSet <- data$GeneSet %>% factor(., levels = c("Adipose_Subcutaneous", "Adipose_Visceral_Omentum", "Adrenal_Gland",
                                                          "Artery_Aorta", "Artery_Coronary", "Artery_Tibial", 
                                                          "Bladder", "Brain_Amygdala", "Brain_Anterior_cingulate_cortex_BA24",
                                                          "Brain_Caudate_basal_ganglia", "Brain_Cerebellar_Hemisphere", "Brain_Cerebellum",                    
                                                          "Brain_Cortex", "Brain_Frontal_Cortex_BA9", "Brain_Hippocampus",               
                                                          "Brain_Hypothalamus", "Brain_Nucleus_accumbens_basal_ganglia", "Brain_Putamen_basal_ganglia", 
                                                          "Brain_Spinal_cord_cervical_c-1", "Brain_Substantia_nigra", "Breast_Mammary_Tissue", 
                                                          "Cells_Cultured_fibroblasts", "Cells_EBV-transformed_lymphocytes", "Cervix_Ectocervix", 
                                                          "Cervix_Endocervix", "Colon_Sigmoid", "Colon_Transverse",                     
                                                          "Esophagus_Gastroesophageal_Junction", "Esophagus_Mucosa", "Esophagus_Muscularis",
                                                          "Fallopian_Tube", "Heart_Atrial_Appendage", "Heart_Left_Ventricle",
                                                          "Kidney_Cortex", "Kidney_Medulla", "Liver",
                                                          "Lung", "Minor_Salivary_Gland", "Muscle_Skeletal", "Nerve_Tibial", "Ovary", "Pancreas",
                                                          "Pituitary", "Prostate", "Skin_Not_Sun_Exposed_Suprapubic", 
                                                          "Skin_Sun_Exposed_Lower_leg", "Small_Intestine_Terminal_Ileum", "Spleen", 
                                                          "Stomach", "Testis", "Thyroid", 
                                                          "Uterus", "Vagina", "Whole_Blood"))
    
    levels(data$GeneSet) <- c("Adipose Subcutaneous", "Adipose Visceral Omentum", "Adrenal Gland",
                              "Artery Aorta", "Artery Coronary", "Artery Tibial", 
                              "Bladder", "Brain Amygdala", "Brain Anterior cingulate cortex BA24",
                              "Brain Caudate basal ganglia", "Brain Cerebellar Hemisphere", "Brain Cerebellum",                    
                              "Brain Cortex", "Brain Frontal Cortex BA9", "Brain Hippocampus",               
                              "Brain Hypothalamus", "Brain Nucleus accumbens basal ganglia", "Brain Putamen basal ganglia", 
                              "Brain Spinal cord cervical c-1", "Brain Substantia nigra", "Breast Mammary Tissue", 
                              "Cells Cultured fibroblasts", "Cells EBV-transformed lymphocytes", "Cervix Ectocervix", 
                              "Cervix Endocervix", "Colon Sigmoid", "Colon Transverse",                     
                              "Esophagus Gastroesophageal Junction", "Esophagus Mucosa", "Esophagus Muscularis",
                              "Fallopian Tube", "Heart Atrial Appendage", "Heart Left Ventricle",
                              "Kidney Cortex", "Kidney Medulla", "Liver",
                              "Lung", "Minor Salivary Gland", "Muscle Skeletal", "Nerve Tibial", "Ovary", "Pancreas",
                              "Pituitary", "Prostate", "Skin Not Sun Exposed Suprapubic", 
                              "Skin Sun Exposed Lower leg", "Small Intestine Terminal Ileum", "Spleen", 
                              "Stomach", "Testis", "Thyroid", 
                              "Uterus", "Vagina", "Whole Blood")
    size_labels_x_axis <- 7
    xlab <- "Tissue Type"
    angle <- 55
    
    
  } else if(xvar_type == "tissue_general"){
    data$GeneSet <- data$GeneSet %>% factor(., levels = c("Adipose_Tissue", "Adrenal_Gland", "Bladder", "Blood", "Blood_Vessel", "Brain", "Breast", "Cervix_Uteri", 
                                                          "Colon", "Esophagus", "Fallopian_Tube", "Heart", "Kidney", "Liver", "Lung", "Muscle", 
                                                          "Nerve", "Ovary", "Pancreas", "Pituitary", "Prostate", "Salivary_Gland", "Skin", "Small_Intestine", 
                                                          "Spleen", "Stomach", "Testis", "Thyroid", "Uterus", "Vagina"))
    levels(data$GeneSet) <- c("Adipose Tissue", "Adrenal Gland", "Bladder", "Blood", "Blood Vessel", "Brain", "Breast", "Cervix Uteri", 
                              "Colon", "Esophagus", "Fallopian Tube", "Heart", "Kidney", "Liver", "Lung", "Muscle", 
                              "Nerve", "Ovary", "Pancreas", "Pituitary", "Prostate", "Salivary Gland", "Skin", "Small Intestine", 
                              "Spleen", "Stomach", "Testis", "Thyroid", "Uterus", "Vagina")
    size_labels_x_axis <- 8
    xlab <- "General Tissue Type"
    angle <- 35
    
  }
  
  
  data$p2 <- scales::rescale((1 - data$adjP), to = c(.3, .5))
  for (i in 1:nrow(data)) {
    if(data[i, "adjP"] <= .05){
      data[i, "p2"] <- .9
    }
    
  }
  
  data %>%
    ggplot(.,  aes(GeneSet, -log10(p), fill = Category)) +
    geom_col(position = "identity",
             alpha = data$p2,
             show.legend = showlegend) + #rescale((1 - data$p), to = c(.1, .55))) + #(1 - data$p))  +
    #geom_hline(yintercept = (-log10(.05)),
    #          color = "yellow") +
    labs(x = xlab,
         y = "-log10 of P-value",
         title = title,
         fill = "Regulation") +
    theme(
      plot.title = element_text(size = 15, 
                                face = "bold", 
                                margin = margin(10, 0, 10, 0),
                                hjust = .5),
      panel.background = element_rect(fill = "grey85"),
      axis.text.x = element_text(angle = angle, 
                                 vjust = 1, hjust = 1, 
                                 size = size_labels_x_axis))# + 
  # scale_x_discrete(breaks = unique(levels(data$GeneSet)), 
  #                  labels = addline_format(levels(data$GeneSet)))
}

# GeneSets
"GeneSet_plots" <- function(GeneSetData, rel_height_comboplot = c(1, 2),
                            xlabsize1 = 7, xlabsize2 = 7, xlabsize3 = 7,
                            angle1 = 45, angle2 = 45, angle3 = 45,
                            str_wrap_cutoff = str_wrap_cutoff,
                            show_legend_alt = show_legend_alt){
  data <- GeneSetData
  if(nrow(data) >= 1){
    max_N_overlap <- max(data$N_overlap)
    
    data <- data %>%
      separate(., genes, into = as.character(c(1:(max_N_overlap + 10))), sep = ":") %>%
      melt(., id.vars = c("Category", "GeneSet", "N_genes", 
                          "N_overlap", "p", "adjP", "link")) %>%
      reshape(., timevar = "value",
              idvar = c("Category", "GeneSet", "N_genes", 
                        "N_overlap", "p", "adjP", "link"),
              #drop = "variable",
              direction = "wide") %>%
      melt(., id.vars = c("Category", "GeneSet", "N_genes", 
                          "N_overlap", "p", "adjP", "link")) %>%
      mutate(., variable = as.character(variable)) %>%
      mutate(., 
             variable = substr(variable, start = 10, stop = nchar(variable)),
             Category = gsub("_", " ", Category),
             GeneSet = gsub("_", " ", GeneSet)) %>%
      filter(., variable != "NA")
    for (i in 1:nrow(data)) {
      if(!is.na(data[i, "value"])){
        data[i, "value"] <- data[i, "Category"]
      } else if(is.na(data[i, "value"])){
        data[i, "value"] <- NA
      }
    }
    
    data$rownr <- rownames(data)
    data2 <- data %>% 
      select(., Category, GeneSet, variable, value, rownr) %>%
      filter(., !is.na(value))
    duplicates <- data2[which( 
      (data2[, c("GeneSet", "variable")] %>% 
         duplicated(.) |
         data2[, c("GeneSet", "variable")] %>% 
         duplicated(., fromLast = TRUE))), 
      "rownr"]
    data3 <- data2 %>% filter(., rownr %in% duplicates & !is.na(value))
    
    if(nrow(data3) != 0){
      data4 <- data3 %>%
        reshape(., idvar = c("GeneSet", "variable"),
                direction = "wide",
                timevar = "value",
                drop = c("rownr")) %>% 
        unite(., "Category", 3:ncol(.), sep = " &\n", na.rm = TRUE)
      
      data5 <- data3 %>%
        reshape(., idvar = c("GeneSet", "variable"),
                direction = "wide",
                timevar = "value") %>% 
        select(., GeneSet, variable, starts_with("rownr"))
      
      data6 <- left_join(data4, data5, by = c("GeneSet", "variable"))
      data <- data %>% filter(., !is.na(value))
      
      namelist <- data6 %>% select(., starts_with("rownr")) %>% colnames()
      data8 <- data6 %>% reshape(., idvar = c("GeneSet", "variable", "Category"),
                                 direction = "long",
                                 varying = namelist) %>%
        filter(., !is.na(rownr)) %>%
        select(., -time)
      for (i in 1:nrow(data)) {
        for (j in 1:nrow(data8)) {
          if(data8[j, "rownr"] == data[i, "rownr"]){
            data[i, "value"] <- data8[j, "Category"]
            data[i, "Category"] <- data8[j, "Category"]
          }
        }
      }
    } else if(nrow(data3) == 0){
      cat("No duplicate categories in the GeneSetdata.\nFurther steps not required.\nSkip to constructing figure.")
    }
    
    Ncats <- data[,"value"] %>% factor() %>% levels() %>% length()
    col_scheme <- rainbow(Ncats)
    level_order <- data[order(data$Category),]$GeneSet %>% factor() %>% unique()
    
    plot_overlap <- data %>% 
      ggplot(., aes(str_wrap(factor(GeneSet, 
                                    levels = level_order), str_wrap_cutoff), 
                    factor(variable, 
                           levels = rev(levels(factor(variable)))), 
                    fill = value)) +
      geom_tile(#aes(alpha = value), 
        #colour = "Grey85",
        show.legend = TRUE) +
      labs(x = "Gene Set",
           y = "Gene",
           title = "Overlapping Gene Sets",
           fill = "Overlap Categories") +
      theme(
        plot.title = element_text(size = 15, 
                                  face = "bold", 
                                  margin = margin(10, 0, 10, 0),
                                  hjust = .5),
        panel.background = element_rect(fill = "White"),
        axis.text.x = element_text(angle = 45, 
                                   vjust = 1, hjust = 1, 
                                   size = 9)) + 
      scale_fill_manual(values = col_scheme, na.value = "White")
    #plot_overlap
    # barplot
    data_prop_p <- data %>%
      filter(., !is.na(value)) %>% 
      distinct(., GeneSet, .keep_all = TRUE) %>%
      mutate(.,
             proportion = N_overlap / N_genes,
             sig_bonf = ifelse(adjP < .05, "sig", "nonsig")) %>%
      filter(., sig_bonf == "sig")
    
    plot_proportion <- data_prop_p %>%
      ggplot(., aes(str_wrap(factor(GeneSet, 
                                    levels = level_order), str_wrap_cutoff), 
                    proportion, 
                    fill = value)) +
      geom_col(show.legend = show_legend_alt) +
      labs(x = "Gene Set",
           y = "Proportion",
           title = "Proportion Overlapping Genes in Gene Set",
           fill = "Overlap Categories") +
      theme(
        plot.title = element_text(size = 15, 
                                  face = "bold", 
                                  margin = margin(10, 0, 10, 0),
                                  hjust = .5),
        panel.background = element_rect(fill = "Grey85"),
        axis.text.x = element_text(angle = angle2, 
                                   vjust = 1, hjust = 1, 
                                   size = xlabsize2)) + 
      scale_fill_manual(values = col_scheme)
    
    plot_pval <- data_prop_p %>%
      ggplot(., aes(str_wrap(factor(GeneSet, 
                                    levels = level_order), str_wrap_cutoff), #25
                    (-log10(adjP)), 
                    fill = value)) +
      geom_col(show.legend = show_legend_alt) +
      labs(x = "Gene Set",
           y = "-log10 of adjusted P-value",
           title = "Enrichment P-value",
           fill = "Overlap Categories") +
      theme(
        plot.title = element_text(size = 15, 
                                  face = "bold", 
                                  margin = margin(10, 0, 10, 0),
                                  hjust = .5),
        panel.background = element_rect(fill = "Grey85"),
        axis.text.x = element_text(angle = angle1, 
                                   vjust = 1, hjust = 1, 
                                   size = xlabsize1)) + 
      scale_fill_manual(values = col_scheme)
    
    plot_combo <- plot_grid(plot_grid(plot_proportion, plot_pval, labels = LETTERS[1:2]),
                            plot_overlap, 
                            labels = c(NA, LETTERS[3]),
                            ncol = 1, nrow = 2,
                            rel_heights = rel_height_comboplot)
    output <- list()
    output$plot_proportion <- plot_proportion
    output$plot_pval <- plot_pval
    output$plot_overlap <- plot_overlap
    output$comboplot <- plot_combo
    
    output$data_orig <- data
    output$data_incl_prop <- data_prop_p
    
    return(output)
    
  } else if(nrow(data) == 0){
    cat("Data contains no gene sets")
  }
  
}

# FUMA results visualized (excl. figure gene sets)
"visualize_FUMA" <- function(path_to_model, modelvar,
                             rel_height_gridplot = c(1, 2),
                             xlabsize1 = 7, xlabsize2 = 7, xlabsize3 = 7,
                             angle1 = 45, angle2 = 45, angle3 = 45,
                             model_nr = c("model0", "model1", "model2", "model3"),
                             str_wrap_cutoff = 25, show_legend_alt = FALSE){
  path_model <- file.path(path_to_model)
  if(!dir.exists(paste0("./plots/FUMA/", modelvar))){
    dir.create(paste0("./plots/FUMA/", modelvar))
  }
  if(!dir.exists(paste0("./plots/FUMA/", modelvar, "/model0/"))){
    dir.create(paste0("./plots/FUMA/", modelvar, "/model0/"))
  }
  if(!dir.exists(paste0("./plots/FUMA/", modelvar, "/model1/"))){
    dir.create(paste0("./plots/FUMA/", modelvar, "/model1/"))
  }
  if(!dir.exists(paste0("./plots/FUMA/", modelvar, "/model2/"))){
    dir.create(paste0("./plots/FUMA/", modelvar, "/model2/"))
  }
  if(!dir.exists(paste0("./plots/FUMA/", modelvar, "/model3/"))){
    dir.create(paste0("./plots/FUMA/", modelvar, "/model3/"))
  }
  ### Expression heatmap
  ## Brainspan
  # bs_age = 29 different ages, bs_dev = 11 developmental periods
  bs_age_avg_log2RPKM_exp <- fread(paste0(path_model, "/bs_age_avg_log2RPKM_exp.txt"))
  bs_age_avg_normRPKM_exp <- fread(paste0(path_model, "/bs_age_avg_normRPKM_exp.txt"))
  
  bs_dev_avg_log2RPKM_exp <- fread(paste0(path_model, "/bs_dev_avg_log2RPKM_exp.txt"))
  bs_dev_avg_normRPKM_exp <- fread(paste0(path_model, "/bs_dev_avg_normRPKM_exp.txt"))
  
  ## GTEx (v8)
  # v8_ts = 53 tissue types, general = 30 general tissue types
  # average of log2 transformed TPM after winsolizing at 50 per tissue
  gtex_v8_ts_avg_log2TPM_exp <- fread(paste0(path_model, "/gtex_v8_ts_avg_log2TPM_exp.txt"))
  # average of normalised (zero mean) log2 transformed TPM after winsolizing at 50 per gene per tissue
  gtex_v8_ts_avg_normTPM_exp <- fread(paste0(path_model, "/gtex_v8_ts_avg_normTPM_exp.txt"))
  
  # average of log2 transformed TPM after winsolizing at 50 per tissue (30 general tissues total) from GTEx v8
  gtex_v8_ts_general_avg_log2TPM_exp <- fread(paste0(path_model, "/gtex_v8_ts_general_avg_log2TPM_exp.txt"))
  # average of normalised (zero mean) log2 transformed TPM after winsolizing at 50 per gene per tissue (30 general tissues total) from GTEx v8
  gtex_v8_ts_general_avg_normTPM_exp <- fread(paste0(path_model, "/gtex_v8_ts_general_avg_normTPM_exp.txt"))
  
  # tissue specificity test
  # input genes vs DEG genes (genes sig up/down expression in certain label compared to other samples)
  # GTEx
  bs_age_DEG <- fread(paste0(path_model, "/bs_age_DEG.txt"))
  bs_dev_DEG <- fread(paste0(path_model, "/bs_dev_DEG.txt"))
  # Brainspan
  gtex_v8_ts_DEG <- fread(paste0(path_model, "/gtex_v8_ts_DEG.txt"))
  gtex_v8_ts_general_DEG <- fread(paste0(path_model, "/gtex_v8_ts_general_DEG.txt"))
  
  ### output
  ## top100 gene list
  geneTable <- fread(paste0(path_model, "/geneTable.txt")) 
  ## Gene set enrichment analysis
  # only sig gene sets included
  GS <- fread(paste0(path_model, "/GS.txt"))
  
  
  ### Figures
  ## Heatmaps
  # Brainspan
  # age
  expa <- plot_avg_exp(bs_age_avg_log2RPKM_exp, "age",
                            paste0(modelvar, ": Expression per Age"))
  rexpa <- plot_avg_exp(bs_age_avg_normRPKM_exp, "age",
                             paste0(modelvar, ": Relative Expression per Age"))
  # developmental periods
  expds <- plot_avg_exp(bs_dev_avg_log2RPKM_exp, "dev",
                             paste0(modelvar, ": Expression per Developmental Period"))
  rexpds <- plot_avg_exp(bs_dev_avg_normRPKM_exp, "dev",
                              paste0(modelvar, ": Relative Expression per Developmental Period"))
  # GTEx
  # tissue
  expt <- plot_avg_exp(gtex_v8_ts_avg_log2TPM_exp, "tissue", 
                       paste0(modelvar, ": Expression per Tissue"))
  rexpt <- plot_avg_exp(gtex_v8_ts_avg_normTPM_exp, "tissue", 
                        paste0(modelvar, ": Relative Expression per Tissue"))
  # tissue general
  gexpt <- plot_avg_exp(gtex_v8_ts_general_avg_log2TPM_exp, "tissue_general", 
                        paste0(modelvar, ": Expression per Tissue"))
  rgexpt <- plot_avg_exp(gtex_v8_ts_general_avg_normTPM_exp, "tissue_general", 
                         paste0(modelvar, ": Relative Expression per Tissue"))
  
  # GeneSets

  GeneSets <- GeneSet_plots(GS, rel_height_comboplot = rel_height_gridplot,
                            xlabsize1 = xlabsize1, xlabsize2 = xlabsize2, xlabsize3 = xlabsize3,
                            angle1 = angle1, angle2 = angle2, angle3 = angle3,
                            str_wrap_cutoff = str_wrap_cutoff,
                            show_legend_alt = show_legend_alt)

  
  ## Tissue specifity
  a <- plot_DEG(bs_age_DEG, "age",
                "DEG Age")
  b <- plot_DEG(bs_dev_DEG, "dev",
                "DEG Developmental Period", showlegend = TRUE)
  c <- plot_DEG(gtex_v8_ts_DEG, "tissue",
                "DEG Tissue")
  d <- plot_DEG(gtex_v8_ts_general_DEG, "tissue_general",
                "DEG General Tissue", showlegend = TRUE)
  gridplot_DEG <- plot_grid(a, b, c, d, 
                            nrow = 2, ncol = 2,
                            byrow = TRUE, 
                            labels = LETTERS[1:4])

  output <- list()
  
  # plots
  output$expa <- expa
  output$rexpa <- rexpa
  output$expds <- expds
  output$rexpds <- rexpds
  
  output$expt <- expt
  output$rexpt <- rexpt
  output$gexpt <- gexpt
  output$rgexpt <- rgexpt

  output$plot_proportion <- GeneSets$plot_proportion
  output$plot_pval <- GeneSets$plot_pval
  output$plot_overlap <- GeneSets$plot_overlap
  
  # gridplots
  expression_brainspan <- plot_grid(expa, rexpa,
                                           expds, rexpds,
                                           ncol = 2, nrow = 2, 
                                           labels = LETTERS[1:4])
  expression_GTEx <- plot_grid(expt, rexpt,
                                      gexpt, rgexpt,
                                      ncol = 2, nrow = 2, 
                                      labels = LETTERS[1:4])
  
  
  output$gridplot_DEG <- gridplot_DEG
  output$gridplot_GS <- GeneSets$comboplot
  output$expression_brainspan <- expression_brainspan
  output$expression_GTEx <- expression_GTEx
  
  
  # data 
  output$geneTable <- geneTable
  output$GS <- GS
  output$GS_incl_prop <- GeneSets$data_incl_prop
  
  output$bs_age_avg_log2RPKM_exp <- bs_age_avg_log2RPKM_exp
  output$bs_age_avg_normRPKM_exp <- bs_age_avg_normRPKM_exp
  output$bs_dev_avg_log2RPKM_exp <- bs_dev_avg_log2RPKM_exp
  output$bs_dev_avg_normRPKM_exp <- bs_dev_avg_normRPKM_exp
  
  output$gtex_v8_ts_avg_log2TPM_exp <- gtex_v8_ts_avg_log2TPM_exp
  output$gtex_v8_ts_avg_normTPM_exp <- gtex_v8_ts_avg_normTPM_exp
  output$gtex_v8_ts_general_avg_log2TPM_exp <- gtex_v8_ts_general_avg_log2TPM_exp
  output$gtex_v8_ts_general_avg_normTPM_exp <- gtex_v8_ts_general_avg_normTPM_exp
  
  output$bs_age_DEG <- bs_age_DEG
  output$bs_dev_DEG <- bs_dev_DEG
  output$gtex_v8_ts_DEG <- gtex_v8_ts_DEG
  output$gtex_v8_ts_general_DEG <- gtex_v8_ts_general_DEG
  
  return(output)
}

# make supplementary data for FUMA
"make_supp_data_FUMA" <- function(FUMAdata, modelname){
  
  if(!dir.exists(paste0("./data/SuppData/FUMA/", modelname))){
    dir.create(paste0("./data/SuppData/FUMA/", modelname))
    }
  
  start <- Sys.time()
    list_of_models <- list("bs_age_avg_log2RPKM_exp" = FUMAdata[["bs_age_avg_log2RPKM_exp"]],
                           "bs_age_avg_normRPKM_exp" = FUMAdata[["bs_age_avg_normRPKM_exp"]],
                           "bs_dev_avg_log2RPKM_exp" = FUMAdata[["bs_dev_avg_log2RPKM_exp"]],
                           "bs_dev_avg_normRPKM_exp" = FUMAdata[["bs_dev_avg_normRPKM_exp"]],
                           "gtex_v8_ts_avg_log2TPM_exp" = FUMAdata[["gtex_v8_ts_avg_log2TPM_exp"]],
                           "gtex_v8_ts_avg_normTPM_exp" = FUMAdata[["gtex_v8_ts_avg_normTPM_exp"]],
                           "gtex_v8_ts_general_avg_log2TPM" = FUMAdata[["gtex_v8_ts_general_avg_log2TPM_exp"]],
                           "gtex_v8_ts_general_avg_normTPM" = FUMAdata[["gtex_v8_ts_general_avg_normTPM_exp"]],
                           "bs_age_DEG" = FUMAdata[["bs_age_DEG"]],
                           "bs_dev_DEG" = FUMAdata[["bs_dev_DEG"]],
                           "gtex_v8_ts_DEG" = FUMAdata[["gtex_v8_ts_DEG"]],
                           "gtex_v8_ts_general_DEG" = FUMAdata[["gtex_v8_ts_general_DEG"]],
                           "geneTable" = FUMAdata[["geneTable"]],
                           "GS" = FUMAdata[["GS"]],
                           "GS_incl_prop" = FUMAdata[["GS_incl_prop"]])
    
    
    write.xlsx(list_of_models, file = paste0("./data/SuppData/FUMA/", modelname, "/SuppData_", 
                                              modelname, "_FUMA.xlsx"),
               col.names = TRUE, row.names = FALSE,
               overwrite = TRUE)
    
  end_full_model <- Sys.time() - start
  cat("Done writing to Excel files. \n"); print(end_full_model, quote = FALSE)
}


