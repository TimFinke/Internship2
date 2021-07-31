# load libraries and functions
source("./Script dependencies.R")

# GWAS EA
ea_gwas_full <- fread("./data/GWAS_EA_excl23andMe.txt",
                      stringsAsFactors = FALSE,
                      data.table = FALSE,
                      fill = TRUE)
colnames(ea_gwas_full)[1] <- "SNP"
colnames(ea_gwas_full)[9] <- "P"
fwrite(ea_gwas_full, 
       file = "./data/clump/GWAS_EA_excl23andMe_colname_fix.txt",
       sep = " ")

# 1000 Genomes data
system("tar -xvzf ./data/clump/1000G_Phase3_plinkfiles.tgz --directory ./data/clump/")

# if plink acting up, try
system("bash ./cd_internship.sh") # may not work, or cd in linux commandline
# running ./plink2 gives trouble, however shellscript works fine

# run shell script plink_all.sh for clumping results, call plink.sh for chr 1-22
# old data
system("bash ./plink_clump_all.sh")
# new data
system("bash ./plink_clump_all_newdata.sh")


# plink clumping results old data
clump_chr1.old <- fread("./data/clump/results/EA_clumped.chr.1.clumped")
clump_chr2.old <- fread("./data/clump/results/EA_clumped.chr.2.clumped")
clump_chr3.old <- fread("./data/clump/results/EA_clumped.chr.3.clumped")
clump_chr4.old <- fread("./data/clump/results/EA_clumped.chr.4.clumped")
clump_chr5.old <- fread("./data/clump/results/EA_clumped.chr.5.clumped")
clump_chr6.old <- fread("./data/clump/results/EA_clumped.chr.6.clumped")
clump_chr7.old <- fread("./data/clump/results/EA_clumped.chr.7.clumped")
clump_chr8.old <- fread("./data/clump/results/EA_clumped.chr.8.clumped")
clump_chr9.old <- fread("./data/clump/results/EA_clumped.chr.9.clumped")
clump_chr10.old <- fread("./data/clump/results/EA_clumped.chr.10.clumped")
clump_chr11.old <- fread("./data/clump/results/EA_clumped.chr.11.clumped")
clump_chr12.old <- fread("./data/clump/results/EA_clumped.chr.12.clumped")
clump_chr13.old <- fread("./data/clump/results/EA_clumped.chr.13.clumped")
clump_chr14.old <- fread("./data/clump/results/EA_clumped.chr.14.clumped")
clump_chr15.old <- fread("./data/clump/results/EA_clumped.chr.15.clumped")
clump_chr16.old <- fread("./data/clump/results/EA_clumped.chr.16.clumped")
clump_chr17.old <- fread("./data/clump/results/EA_clumped.chr.17.clumped")
clump_chr18.old <- fread("./data/clump/results/EA_clumped.chr.18.clumped")
clump_chr19.old <- fread("./data/clump/results/EA_clumped.chr.19.clumped")
clump_chr20.old <- fread("./data/clump/results/EA_clumped.chr.20.clumped")
clump_chr21.old <- fread("./data/clump/results/EA_clumped.chr.21.clumped")
clump_chr22.old <- fread("./data/clump/results/EA_clumped.chr.22.clumped")

# plink clumping results
clump_chr1 <- fread("./data/clump/results_newdata/EA_clumped.chr.1.clumped")
clump_chr2 <- fread("./data/clump/results_newdata/EA_clumped.chr.2.clumped")
clump_chr3 <- fread("./data/clump/results_newdata/EA_clumped.chr.3.clumped")
clump_chr4 <- fread("./data/clump/results_newdata/EA_clumped.chr.4.clumped")
clump_chr5 <- fread("./data/clump/results_newdata/EA_clumped.chr.5.clumped")
clump_chr6 <- fread("./data/clump/results_newdata/EA_clumped.chr.6.clumped")
clump_chr7 <- fread("./data/clump/results_newdata/EA_clumped.chr.7.clumped")
clump_chr8 <- fread("./data/clump/results_newdata/EA_clumped.chr.8.clumped")
clump_chr9 <- fread("./data/clump/results_newdata/EA_clumped.chr.9.clumped")
clump_chr10 <- fread("./data/clump/results_newdata/EA_clumped.chr.10.clumped")
clump_chr11 <- fread("./data/clump/results_newdata/EA_clumped.chr.11.clumped")
clump_chr12 <- fread("./data/clump/results_newdata/EA_clumped.chr.12.clumped")
clump_chr13 <- fread("./data/clump/results_newdata/EA_clumped.chr.13.clumped")
clump_chr14 <- fread("./data/clump/results_newdata/EA_clumped.chr.14.clumped")
clump_chr15 <- fread("./data/clump/results_newdata/EA_clumped.chr.15.clumped")
clump_chr16 <- fread("./data/clump/results_newdata/EA_clumped.chr.16.clumped")
clump_chr17 <- fread("./data/clump/results_newdata/EA_clumped.chr.17.clumped")
clump_chr18 <- fread("./data/clump/results_newdata/EA_clumped.chr.18.clumped")
clump_chr19 <- fread("./data/clump/results_newdata/EA_clumped.chr.19.clumped")
clump_chr20 <- fread("./data/clump/results_newdata/EA_clumped.chr.20.clumped")
clump_chr21 <- fread("./data/clump/results_newdata/EA_clumped.chr.21.clumped")
clump_chr22 <- fread("./data/clump/results_newdata/EA_clumped.chr.22.clumped")
# bind all chromosomes
clump_res <- rbind.data.frame(clump_chr1, clump_chr2, clump_chr3, clump_chr4,
                              clump_chr5, clump_chr6, clump_chr7, clump_chr8,
                              clump_chr9, clump_chr10, clump_chr11, clump_chr12,
                              clump_chr13, clump_chr14, clump_chr15, clump_chr16,
                              clump_chr17, clump_chr18, clump_chr19, clump_chr20,
                              clump_chr21, clump_chr22)
rm(clump_chr1, clump_chr2, clump_chr3, clump_chr4,
   clump_chr5, clump_chr6, clump_chr7, clump_chr8,
   clump_chr9, clump_chr10, clump_chr11, clump_chr12,
   clump_chr13, clump_chr14, clump_chr15, clump_chr16,
   clump_chr17, clump_chr18, clump_chr19, clump_chr20,
   clump_chr21, clump_chr22)

clump_res <- clump_res %>%
   separate(., SP2, into = as.character(c(1:941)), sep = ",") %>%
   as.matrix()
clump_res[, 12:ncol(clump_res)] <- substr(as.matrix(clump_res[, 12:ncol(clump_res)]), 1, nchar(as.matrix(clump_res[, 12:ncol(clump_res)])) - 3)
clump_res <- clump_res %>%
   as.data.frame() %>%
   unite(., SP2, 12:ncol(clump_res), sep = ", ", na.rm = TRUE)

#fwrite(clump_res, file = "./data/LD/EA_clumped_full.txt")

# ciseQTL
ciseQTLs <- fread("./data/eQTL/2019-12-11-cis-eQTLsFDR0.05-ProbeLevel-CohortInfoRemoved-BonferroniAdded.txt.gz", 
                  stringsAsFactors = FALSE, 
                  data.table = FALSE, 
                  fill = TRUE)

# transeQTL
transeQTLs <- fread("./data/eQTL/2018-09-04-trans-eQTLsFDR0.05-CohortInfoRemoved-BonferroniAdded.txt.gz",
                    stringsAsFactors = FALSE,
                    data.table = FALSE,
                    fill = TRUE)

# selecting lead SNPs per gene (& per chromosome for trans) for cis- & transeQTLs
start <- Sys.time()
eQTLs_cis <- eQTL_finder(ciseQTLs, eQTL = "cis")
Sys.time() - start # time: 1.085 hours for full dataset

eQTLs_cis$Gene %>% unique() %>% length(); ciseQTLs$Gene %>% unique() %>% length() # all 16987 genes present
eQTLs_cis$SNP %>% unique() %>% length(); ciseQTLs$SNP %>% unique() %>% length() # 16290 of 3663456 SNPs are lead SNPs
eQTLs_cis_duplicated <- eQTLs_cis[duplicated(eQTLs_cis$SNP),]
eQTLs_cis_duplicates <- eQTLs_cis %>%
   filter(., SNP %in% eQTLs_cis_duplicated$SNP); eQTLs_cis_duplicated$SNP %>% unique() %>% length(); rm(eQTLs_cis_duplicated)

start <- Sys.time()
eQTLs_trans <- eQTL_finder(transeQTLs, eQTL = "trans")
Sys.time() - start # time: 1.56 minutes for full dataset

eQTLs_trans$Gene %>% unique() %>% length(); transeQTLs$Gene %>% unique() %>% length() # all 6298 genes present
eQTLs_trans$SNP %>% unique() %>% length(); transeQTLs$SNP %>% unique() %>% length() # 2639 of 3853 SNPs are lead SNPs 
eQTLs_trans$SNPChr %>% unique() %>% length(); transeQTLs$SNPChr %>% unique() %>% length()
eQTLs_trans_duplicated <- eQTLs_trans[duplicated(eQTLs_trans$SNP),]
eQTLs_trans_duplicates <- eQTLs_trans %>%
   filter(., SNP %in% eQTLs_trans_duplicated$SNP); eQTLs_trans_duplicated$SNP %>% unique() %>% length(); rm(eQTLs_trans_duplicated)

# write to .txt
#fwrite(eQTLs_cis, file = "./data/LD/Lead_ciseQTL.txt")
#fwrite(eQTLs_trans, file = "./data/LD/Lead_transeQTL.txt")

eQTLs_cis <- fread("./data/LD/Lead_ciseQTL.txt")
eQTLs_trans <- fread("./data/LD/Lead_transeQTL.txt")
clump_res <- fread("./data/LD/EA_clumped_full.txt")
eQTLs_cis$SNP %>% 
   unique() %>% 
   as.data.frame() %>% 
   as.list() %>% 
   fwrite(., file = "./data/LD/SNPlist_lead_ciseQTL.txt", col.names = FALSE)
eQTLs_trans$SNP %>% 
   unique() %>% 
   as.data.frame() %>% 
   as.list() %>% 
   fwrite(., file = "./data/LD/SNPlist_lead_transeQTL.txt", col.names = FALSE)
clump_res$SNP %>% 
   as.data.frame() %>% 
   as.list() %>% 
   fwrite(., file = "./data/LD/SNPlist_EA_clumped.txt", col.names = FALSE)

# plink calculate LD
# old data
system("bash ./plink_ld_calc_all.sh cis SNPlist_lead_ciseQTL.txt") 
system("bash ./plink_ld_calc_all.sh trans SNPlist_lead_transeQTL.txt") 
# new data
system("bash ./plink_ld_calc_all_newdata.sh cis SNPlist_lead_ciseQTL.txt") 
system("bash ./plink_ld_calc_all_newdata.sh trans SNPlist_lead_transeQTL.txt") 

# LD files
# plink clumping results
# LD ciseQTLs old data
LD_cis_chr1.old <- fread("./data/LD/results/LD.cis.1.ld")
LD_cis_chr2.old <- fread("./data/LD/results/LD.cis.2.ld")
LD_cis_chr3.old <- fread("./data/LD/results/LD.cis.3.ld")
LD_cis_chr4.old <- fread("./data/LD/results/LD.cis.4.ld")
LD_cis_chr5.old <- fread("./data/LD/results/LD.cis.5.ld")
LD_cis_chr6.old <- fread("./data/LD/results/LD.cis.6.ld")
LD_cis_chr7.old <- fread("./data/LD/results/LD.cis.7.ld")
LD_cis_chr8.old <- fread("./data/LD/results/LD.cis.8.ld")
LD_cis_chr9.old <- fread("./data/LD/results/LD.cis.9.ld")
LD_cis_chr10.old <- fread("./data/LD/results/LD.cis.10.ld")
LD_cis_chr11.old <- fread("./data/LD/results/LD.cis.11.ld")
LD_cis_chr12.old <- fread("./data/LD/results/LD.cis.12.ld")
LD_cis_chr13.old <- fread("./data/LD/results/LD.cis.13.ld")
LD_cis_chr14.old <- fread("./data/LD/results/LD.cis.14.ld")
LD_cis_chr15.old <- fread("./data/LD/results/LD.cis.15.ld")
LD_cis_chr16.old <- fread("./data/LD/results/LD.cis.16.ld")
LD_cis_chr17.old <- fread("./data/LD/results/LD.cis.17.ld")
LD_cis_chr18.old <- fread("./data/LD/results/LD.cis.18.ld")
LD_cis_chr19.old <- fread("./data/LD/results/LD.cis.19.ld")
LD_cis_chr20.old <- fread("./data/LD/results/LD.cis.20.ld")
LD_cis_chr21.old <- fread("./data/LD/results/LD.cis.21.ld")
LD_cis_chr22.old <- fread("./data/LD/results/LD.cis.22.ld")
LD_cis_res.old <- rbind.data.frame(LD_cis_chr1.old, LD_cis_chr2.old, LD_cis_chr3.old, LD_cis_chr4.old,
                               LD_cis_chr5.old, LD_cis_chr6.old, LD_cis_chr7.old, LD_cis_chr8.old,
                               LD_cis_chr9.old, LD_cis_chr10.old, LD_cis_chr11.old, LD_cis_chr12.old,
                               LD_cis_chr13.old, LD_cis_chr14.old, LD_cis_chr15.old, LD_cis_chr16.old,
                               LD_cis_chr17.old, LD_cis_chr18.old, LD_cis_chr19.old, LD_cis_chr20.old,
                               LD_cis_chr21.old, LD_cis_chr22.old)
# LD ciseQTLs new data
LD_cis_chr1 <- fread("./data/LD/results_newdata/LD.cis.1.ld")
LD_cis_chr2 <- fread("./data/LD/results_newdata/LD.cis.2.ld")
LD_cis_chr3 <- fread("./data/LD/results_newdata/LD.cis.3.ld")
LD_cis_chr4 <- fread("./data/LD/results_newdata/LD.cis.4.ld")
LD_cis_chr5 <- fread("./data/LD/results_newdata/LD.cis.5.ld")
LD_cis_chr6 <- fread("./data/LD/results_newdata/LD.cis.6.ld")
LD_cis_chr7 <- fread("./data/LD/results_newdata/LD.cis.7.ld")
LD_cis_chr8 <- fread("./data/LD/results_newdata/LD.cis.8.ld")
LD_cis_chr9 <- fread("./data/LD/results_newdata/LD.cis.9.ld")
LD_cis_chr10 <- fread("./data/LD/results_newdata/LD.cis.10.ld")
LD_cis_chr11 <- fread("./data/LD/results_newdata/LD.cis.11.ld")
LD_cis_chr12 <- fread("./data/LD/results_newdata/LD.cis.12.ld")
LD_cis_chr13 <- fread("./data/LD/results_newdata/LD.cis.13.ld")
LD_cis_chr14 <- fread("./data/LD/results_newdata/LD.cis.14.ld")
LD_cis_chr15 <- fread("./data/LD/results_newdata/LD.cis.15.ld")
LD_cis_chr16 <- fread("./data/LD/results_newdata/LD.cis.16.ld")
LD_cis_chr17 <- fread("./data/LD/results_newdata/LD.cis.17.ld")
LD_cis_chr18 <- fread("./data/LD/results_newdata/LD.cis.18.ld")
LD_cis_chr19 <- fread("./data/LD/results_newdata/LD.cis.19.ld")
LD_cis_chr20 <- fread("./data/LD/results_newdata/LD.cis.20.ld")
LD_cis_chr21 <- fread("./data/LD/results_newdata/LD.cis.21.ld")
LD_cis_chr22 <- fread("./data/LD/results_newdata/LD.cis.22.ld")
LD_cis_res <- rbind.data.frame(LD_cis_chr1, LD_cis_chr2, LD_cis_chr11, LD_cis_chr16)# %>% 
#   filter(., SNP_A != SNP_B)
rm(LD_cis_chr1, LD_cis_chr2, LD_cis_chr3, LD_cis_chr4,
   LD_cis_chr5, LD_cis_chr6, LD_cis_chr7, LD_cis_chr8,
   LD_cis_chr9, LD_cis_chr10, LD_cis_chr11, LD_cis_chr12,
   LD_cis_chr13, LD_cis_chr14, LD_cis_chr15, LD_cis_chr16,
   LD_cis_chr17, LD_cis_chr18, LD_cis_chr19, LD_cis_chr20,
   LD_cis_chr21, LD_cis_chr22)
# LD transeQTLs
LD_trans_chr1 <- fread("./data/LD/results_newdata/LD.trans.1.ld")
LD_trans_chr2 <- fread("./data/LD/results_newdata/LD.trans.2.ld")
LD_trans_chr3 <- fread("./data/LD/results_newdata/LD.trans.3.ld")
LD_trans_chr4 <- fread("./data/LD/results_newdata/LD.trans.4.ld")
LD_trans_chr5 <- fread("./data/LD/results_newdata/LD.trans.5.ld")
LD_trans_chr6 <- fread("./data/LD/results_newdata/LD.trans.6.ld")
LD_trans_chr7 <- fread("./data/LD/results_newdata/LD.trans.7.ld")
LD_trans_chr8 <- fread("./data/LD/results_newdata/LD.trans.8.ld")
LD_trans_chr9 <- fread("./data/LD/results_newdata/LD.trans.9.ld")
LD_trans_chr10 <- fread("./data/LD/results_newdata/LD.trans.10.ld")
LD_trans_chr11 <- fread("./data/LD/results_newdata/LD.trans.11.ld")
LD_trans_chr12 <- fread("./data/LD/results_newdata/LD.trans.12.ld")
LD_trans_chr13 <- fread("./data/LD/results_newdata/LD.trans.13.ld")
LD_trans_chr14 <- fread("./data/LD/results_newdata/LD.trans.14.ld")
LD_trans_chr15 <- fread("./data/LD/results_newdata/LD.trans.15.ld")
LD_trans_chr16 <- fread("./data/LD/results_newdata/LD.trans.16.ld")
LD_trans_chr17 <- fread("./data/LD/results_newdata/LD.trans.17.ld")
LD_trans_chr18 <- fread("./data/LD/results_newdata/LD.trans.18.ld")
LD_trans_chr19 <- fread("./data/LD/results_newdata/LD.trans.19.ld")
LD_trans_chr20 <- fread("./data/LD/results_newdata/LD.trans.20.ld")
LD_trans_chr21 <- fread("./data/LD/results_newdata/LD.trans.21.ld")
LD_trans_chr22 <- fread("./data/LD/results_newdata/LD.trans.22.ld")
LD_trans_res <- rbind.data.frame(LD_trans_chr1, LD_trans_chr2, LD_trans_chr3, LD_trans_chr4,
                               LD_trans_chr5, LD_trans_chr6, LD_trans_chr7, LD_trans_chr8,
                               LD_trans_chr9, LD_trans_chr10, LD_trans_chr11, LD_trans_chr12,
                               LD_trans_chr13, LD_trans_chr14, LD_trans_chr15, LD_trans_chr16,
                               LD_trans_chr17, LD_trans_chr18, LD_trans_chr19, LD_trans_chr20,
                               LD_trans_chr21, LD_trans_chr22)
LD_trans_res <- rbind.data.frame(LD_trans_chr6, LD_trans_chr7, LD_trans_chr12, 
                                 LD_trans_chr16, LD_trans_chr20)# %>% 
#   filter(., SNP_A != SNP_B)
rm(LD_trans_chr1, LD_trans_chr2, LD_trans_chr3, LD_trans_chr4,
   LD_trans_chr5, LD_trans_chr6, LD_trans_chr7, LD_trans_chr8,
   LD_trans_chr9, LD_trans_chr10, LD_trans_chr11, LD_trans_chr12,
   LD_trans_chr13, LD_trans_chr14, LD_trans_chr15, LD_trans_chr16,
   LD_trans_chr17, LD_trans_chr18, LD_trans_chr19, LD_trans_chr20,
   LD_trans_chr21, LD_trans_chr22)

# Supplementary Data
#fwrite(LD_cis_res, file = "./data/LD/LD_ciseQTL.txt")
#fwrite(LD_trans_res, file = "./data/LD/LD_transeQTL.txt")
SNPlist_lead_ciseQTL <- eQTLs_cis$SNP %>% 
   unique() %>% 
   as.data.frame() 
SNPlist_lead_transeQTL <- eQTLs_trans$SNP %>% 
   unique() %>% 
   as.data.frame() 
SNPlist_EA_clumped <- clump_res$SNP %>% 
   as.data.frame() 

if(!dir.exists(paste0("./data/SuppData/eQTL/"))){
   dir.create(paste0("./data/SuppData/eQTL/"))
}
list_eQTL <- list("Lead eQTLs cis" = eQTLs_cis,
                  "SNPlist lead ciseQTL" = SNPlist_lead_ciseQTL,
                  "Lead eQTLs trans" = eQTLs_trans,
                  "SNPlist lead transeQTL" = SNPlist_lead_transeQTL,
                  "Clumped EA GWAS" = clump_res,
                  "SNPlist lead EA GWAS clumped" = SNPlist_EA_clumped,
                  "cis eQTLs in LD" = LD_cis_res,
                  "trans eQTLs in LD" = LD_trans_res)


write.xlsx(list_eQTL, file = paste0("./data/SuppData/eQTL/SuppData_eQTL.xlsx"),
           col.names = TRUE, row.names = FALSE,
           overwrite = TRUE)