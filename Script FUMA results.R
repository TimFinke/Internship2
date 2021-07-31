source("./Script dependencies.R")
### paths
path_incocc <- file.path("./data/FUMA results/FUMA_gene2func62787")
path_edu <- file.path("./data/FUMA results/FUMA_gene2func62788")
path_hsvl <- file.path("./data/FUMA results/FUMA_gene2func62789")
path_increg <- file.path("./data/FUMA results/FUMA_gene2func62790")


'''
path_hsvl_model0 <- file.path("./data/FUMA results/FUMA_gene2func61376/")
path_hsvl_model1 <- file.path("./data/FUMA results/FUMA_gene2func61374/")
path_hsvl_model2 <- file.path("./data/FUMA results/FUMA_gene2func61788/")

path_increg_model0 <- file.path("./data/FUMA results/FUMA_gene2func61790/") # 3 DEG sig
path_increg_model1 <- file.path("./data/FUMA results/FUMA_gene2func61791/")
path_increg_model2 <- file.path("./data/FUMA results/FUMA_gene2func61792/") # 1 DEG sig

path_incocc_model0 <- file.path("./data/FUMA results/FUMA_gene2func62292")
path_incocc_model1 <- file.path("./data/FUMA results/FUMA_gene2func62293")
path_incocc_model2 <- file.path("./data/FUMA results/FUMA_gene2func62397")

path_edu_model0 <- file.path("./data/FUMA results/FUMA_gene2func62291")
path_edu_model1 <- file.path("./data/FUMA results/FUMA_gene2func62396")
path_edu_model2 <- file.path("./data/FUMA results/FUMA_gene2func62418")
'''
### FUMA
## income occupation sig results
incocc_model <- visualize_FUMA(path_incocc, "Income Occupation",
                               xlabsize1 = 9, xlabsize2 = 9, xlabsize3 = 10,
                               angle1 = 60, angle2 = 60, angle3 = 60)
make_supp_data_FUMA(incocc_model, "incocc_socdem_model")

png(str_c("./plots/FUMA/Income Occupation/model1/expression_brainspan.png"), 
    width = 20, height = 25, units = "in", res = 800)
incocc_model$expression_brainspan
dev.off()

png(str_c("./plots/FUMA/Income Occupation/model1/expression_GTEx.png"), 
    width = 20, height = 25, units = "in", res = 800)
incocc_model$expression_GTEx
dev.off()

png(str_c("./plots/FUMA/Income Occupation/model1/gridplot_DEG.png"), 
    width = 20, height = 20, units = "in", res = 800)
incocc_model$gridplot_DEG # do not include, no sig
dev.off()

png(str_c("./plots/FUMA/Income Occupation/model1/gridplot_GS.png"), 
    width = 40, height = 20, units = "in", res = 800)
incocc_model$gridplot_GS # results but not for figure, just two gene sets
dev.off()

## education top 100
edu_model <- visualize_FUMA(path_edu, "Education",
                            xlabsize1 = 9, xlabsize2 = 9, xlabsize3 = 10,
                            angle1 = 60, angle2 = 60, angle3 = 60)
make_supp_data_FUMA(edu_model, "edu_socdem_model")

png(str_c("./plots/FUMA/Education/model1/expression_brainspan.png"), 
    width = 20, height = 25, units = "in", res = 800)
edu_model$expression_brainspan
dev.off()

png(str_c("./plots/FUMA/Education/model1/expression_GTEx.png"), 
    width = 20, height = 25, units = "in", res = 800)
edu_model$expression_GTEx
dev.off()

png(str_c("./plots/FUMA/Education/model1/gridplot_DEG.png"), 
    width = 20, height = 20, units = "in", res = 800)
edu_model$gridplot_DEG #  include
dev.off()

png(str_c("./plots/FUMA/Education/model1/gridplot_GS.png"), 
    width = 40, height = 20, units = "in", res = 800)
edu_model$gridplot_GS # include but big, maybe split up or show separately
dev.off()
# split up crazy figure
edu_model_alt <- visualize_FUMA(path_edu, "Education",
                                xlabsize1 = 14, xlabsize2 = 14, xlabsize3 = 14,
                                angle1 = 75, angle2 = 75, angle3 = 60,
                                str_wrap_cutoff = 60,
                                show_legend_alt = TRUE)

edu_model_alt$plot_proportion
edu_model_alt$plot_pval
edu_model_alt$plot_overlap
edu_model_alt$gridplot_GS
png(str_c("./plots/FUMA/Education/model1/plot_alt_proportion.png"), 
    width = 40, height = 20, units = "in", res = 800)
edu_model_alt$plot_proportion
dev.off()

png(str_c("./plots/FUMA/Education/model1/plot_alt_pval.png"), 
    width = 40, height = 20, units = "in", res = 800)
edu_model_alt$plot_pval
dev.off()

png(str_c("./plots/FUMA/Education/model1/plot_alt_overlap.png"), 
    width = 40, height = 20, units = "in", res = 800)
edu_model_alt$plot_overlap
dev.off()
## house value top 100
hsvl_model <- visualize_FUMA(path_hsvl, "House Value",
                            xlabsize1 = 9, xlabsize2 = 9, xlabsize3 = 10,
                            angle1 = 60, angle2 = 60, angle3 = 60)
make_supp_data_FUMA(hsvl_model, "hsvl_socdem_model")

png(str_c("./plots/FUMA/House Value/model1/expression_brainspan.png"), 
    width = 20, height = 25, units = "in", res = 800)
hsvl_model$expression_brainspan
dev.off()

png(str_c("./plots/FUMA/House Value/model1/expression_GTEx.png"), 
    width = 20, height = 25, units = "in", res = 800)
hsvl_model$expression_GTEx
dev.off()

png(str_c("./plots/FUMA/House Value/model1/gridplot_DEG.png"), 
    width = 20, height = 20, units = "in", res = 800)
hsvl_model$gridplot_DEG # do not include, no sig
dev.off()

png(str_c("./plots/FUMA/House Value/model1/gridplot_GS.png"), 
    width = 40, height = 20, units = "in", res = 800)
hsvl_model$gridplot_GS # no gene sets
dev.off()

## income region top 100
increg_model <- visualize_FUMA(path_increg, "Income Region",
                             xlabsize1 = 9, xlabsize2 = 9, xlabsize3 = 10,
                             angle1 = 60, angle2 = 60, angle3 = 60)
make_supp_data_FUMA(increg_model, "increg_socdem_model")

png(str_c("./plots/FUMA/Income Region/model1/expression_brainspan.png"), 
    width = 20, height = 25, units = "in", res = 800)
increg_model$expression_brainspan
dev.off()

png(str_c("./plots/FUMA/Income Region/model1/expression_GTEx.png"), 
    width = 20, height = 25, units = "in", res = 800)
increg_model$expression_GTEx
dev.off()

png(str_c("./plots/FUMA/Income Region/model1/gridplot_DEG.png"), 
    width = 20, height = 20, units = "in", res = 800)
increg_model$gridplot_DEG # do not include, no sig
dev.off()

png(str_c("./plots/FUMA/Income Region/model1/gridplot_GS.png"), 
    width = 40, height = 20, units = "in", res = 800)
increg_model$gridplot_GS # 1 gene set, mention in text, do not include figure
dev.off()


### save models
save(edu_model, hsvl_model, incocc_model, increg_model,
     file = "./data/FUMA results/socdemmodels.Rdata")


'''

make_supp_data_FUMA(edu_model1, "edu_model1")

png(str_c("./plots/FUMA/Education/model1/expression_brainspan.png"), 
    width = 20, height = 25, units = "in", res = 800)
edu_model1$expression_brainspan
dev.off()

png(str_c("./plots/FUMA/Education/model1/expression_GTEx.png"), 
    width = 20, height = 25, units = "in", res = 800)
edu_model1$expression_GTEx
dev.off()

png(str_c("./plots/FUMA/Education/model1/gridplot_DEG.png"), 
    width = 20, height = 20, units = "in", res = 800)
edu_model1$gridplot_DEG #  include
dev.off()

png(str_c("./plots/FUMA/Education/model1/gridplot_GS.png"), 
    width = 40, height = 20, units = "in", res = 800)
edu_model1$gridplot_GS # include
dev.off()


'''


#########################
## education
edu_model0 <- visualize_FUMA(path_edu_model0, "Education",
                             xlabsize1 = 11, xlabsize2 = 11, xlabsize3 = 12,
                             angle1 = 60, angle2 = 60, angle3 = 60)
make_supp_data_FUMA(edu_model0, "edu_model0")

png(str_c("./plots/FUMA/Education/model0/expression_brainspan.png"), 
    width = 20, height = 25, units = "in", res = 800)
edu_model0$expression_brainspan
dev.off()

png(str_c("./plots/FUMA/Education/model0/expression_GTEx.png"), 
    width = 20, height = 25, units = "in", res = 800)
edu_model0$expression_GTEx
dev.off()

png(str_c("./plots/FUMA/Education/model0/gridplot_DEG.png"), 
    width = 20, height = 20, units = "in", res = 800)
edu_model0$gridplot_DEG # include
dev.off()

edu_model0_GS_exl_imm_sig_GO <- edu_model0$GS %>% filter(., Category != "Immunologic_signatures" &
                                                        Category != "GO_bp" &
                                                        Category != "GO_cc" &
                                                        Category != "GO_mf")
GeneSets_exl_imm_sig_GO <- GeneSet_plots(edu_model0_GS_exl_imm_sig_GO, rel_height_comboplot = c(1,2),
                                      xlabsize1 = 6, xlabsize2 = 6, xlabsize3 = 7,
                                      angle1 = 75, angle2 = 75, angle3 = 75)
GeneSets_exl_imm_sig_GO$comboplot
png(str_c("./plots/FUMA/Education/model0/gridplot_GS_excl_imm_sig_GO.png"), 
    width = 40, height = 20, units = "in", res = 800)
GeneSets_exl_imm_sig_GO$comboplot #edu_model0$gridplot_GS # still too large
dev.off()
# model1
edu_model1 <- visualize_FUMA(path_edu_model1, "Education",
                             xlabsize1 = 11, xlabsize2 = 11, xlabsize3 = 12,
                             angle1 = 60, angle2 = 60, angle3 = 60)
make_supp_data_FUMA(edu_model1, "edu_model1")

png(str_c("./plots/FUMA/Education/model1/expression_brainspan.png"), 
    width = 20, height = 25, units = "in", res = 800)
edu_model1$expression_brainspan
dev.off()

png(str_c("./plots/FUMA/Education/model1/expression_GTEx.png"), 
    width = 20, height = 25, units = "in", res = 800)
edu_model1$expression_GTEx
dev.off()

png(str_c("./plots/FUMA/Education/model1/gridplot_DEG.png"), 
    width = 20, height = 20, units = "in", res = 800)
edu_model1$gridplot_DEG #  include
dev.off()

png(str_c("./plots/FUMA/Education/model1/gridplot_GS.png"), 
    width = 40, height = 20, units = "in", res = 800)
edu_model1$gridplot_GS # include
dev.off()
# model2
edu_model2 <- visualize_FUMA(path_edu_model2, "Education",
                             xlabsize1 = 11, xlabsize2 = 11, xlabsize3 = 12,
                             angle1 = 30, angle2 = 30, angle3 = 30)
make_supp_data_FUMA(edu_model2, "edu_model2")

png(str_c("./plots/FUMA/Education/model2/expression_brainspan.png"), 
    width = 20, height = 25, units = "in", res = 800)
edu_model2$expression_brainspan
dev.off()

png(str_c("./plots/FUMA/Education/model2/expression_GTEx.png"), 
    width = 20, height = 25, units = "in", res = 800)
edu_model2$expression_GTEx
dev.off()

png(str_c("./plots/FUMA/Education/model2/gridplot_DEG.png"), 
    width = 20, height = 20, units = "in", res = 800)
edu_model2$gridplot_DEG # not sig
dev.off()

png(str_c("./plots/FUMA/Education/model2/gridplot_GS.png"), 
    width = 40, height = 20, units = "in", res = 800)
edu_model2$gridplot_GS # include
dev.off()

# save models
save(edu_model0, edu_model1, edu_model2,
     file = "./data/FUMA results/models/education.Rdata")

## house value
# model0
hsvl_model0 <- visualize_FUMA(path_hsvl_model0, "House Value",
                              xlabsize1 = 11, xlabsize2 = 11, xlabsize3 = 12,
                              angle1 = 30, angle2 = 30, angle3 = 30)
make_supp_data_FUMA(hsvl_model0, "hsvl_model0")

png(str_c("./plots/FUMA/House Value/model0/expression_brainspan.png"), 
    width = 20, height = 25, units = "in", res = 800)
hsvl_model0$expression_brainspan
dev.off()

png(str_c("./plots/FUMA/House Value/model0/expression_GTEx.png"), 
    width = 20, height = 25, units = "in", res = 800)
hsvl_model0$expression_GTEx
dev.off()

png(str_c("./plots/FUMA/House Value/model0/gridplot_DEG.png"), 
    width = 20, height = 20, units = "in", res = 800)
hsvl_model0$gridplot_DEG # nothing significant 
dev.off()

png(str_c("./plots/FUMA/House Value/model0/gridplot_GS.png"), 
    width = 20, height = 20, units = "in", res = 800)
hsvl_model0$gridplot_GS # as text
dev.off()
# model1
hsvl_model1 <- visualize_FUMA(path_hsvl_model1, "House Value",
                              xlabsize1 = 11, xlabsize2 = 11, xlabsize3 = 12,
                              angle1 = 30, angle2 = 30, angle3 = 30)
make_supp_data_FUMA(hsvl_model1, "hsvl_model1")

png(str_c("./plots/FUMA/House Value/model1/expression_brainspan.png"), 
    width = 20, height = 25, units = "in", res = 800)
hsvl_model1$expression_brainspan
dev.off()

png(str_c("./plots/FUMA/House Value/model1/expression_GTEx.png"), 
    width = 20, height = 25, units = "in", res = 800)
hsvl_model1$expression_GTEx
dev.off()

png(str_c("./plots/FUMA/House Value/model1/gridplot_DEG.png"), 
    width = 20, height = 20, units = "in", res = 800)
hsvl_model1$gridplot_DEG # not sig
dev.off()

png(str_c("./plots/FUMA/House Value/model1/gridplot_GS.png"), 
    width = 20, height = 20, units = "in", res = 800)
hsvl_model1$gridplot_GS # as text
dev.off()
# model2
hsvl_model2 <- visualize_FUMA(path_hsvl_model2, "House Value",
                              xlabsize1 = 11, xlabsize2 = 11, xlabsize3 = 12,
                              angle1 = 30, angle2 = 30, angle3 = 30)
make_supp_data_FUMA(hsvl_model2, "hsvl_model2")

png(str_c("./plots/FUMA/House Value/model2/expression_brainspan.png"), 
    width = 20, height = 25, units = "in", res = 800)
hsvl_model2$expression_brainspan
dev.off()

png(str_c("./plots/FUMA/House Value/model2/expression_GTEx.png"), 
    width = 20, height = 25, units = "in", res = 800)
hsvl_model2$expression_GTEx
dev.off()

png(str_c("./plots/FUMA/House Value/model2/gridplot_DEG.png"), 
    width = 20, height = 20, units = "in", res = 800)
hsvl_model2$gridplot_DEG # niet sig
dev.off()

png(str_c("./plots/FUMA/House Value/model2/gridplot_GS.png"), 
    width = 20, height = 20, units = "in", res = 800)
hsvl_model2$gridplot_GS # 2 categorieen
dev.off()

# save models
save(hsvl_model0, hsvl_model1, hsvl_model2,
     file = "./data/FUMA results/models/house value.Rdata")

## income occupation
# model0
incocc_model0 <- visualize_FUMA(path_incocc_model0, "Income Occupation",
                                xlabsize1 = 11, xlabsize2 = 11, xlabsize3 = 12,
                                angle1 = 30, angle2 = 30, angle3 = 30)
make_supp_data_FUMA(incocc_model0, "incocc_model0")

png(str_c("./plots/FUMA/Income Occupation/model0/expression_brainspan.png"), 
    width = 20, height = 25, units = "in", res = 800)
incocc_model0$expression_brainspan
dev.off()

png(str_c("./plots/FUMA/Income Occupation/model0/expression_GTEx.png"), 
    width = 20, height = 25, units = "in", res = 800)
incocc_model0$expression_GTEx
dev.off()

png(str_c("./plots/FUMA/Income Occupation/model0/gridplot_DEG.png"), 
    width = 20, height = 20, units = "in", res = 800)
incocc_model0$gridplot_DEG # include, 3 DEG sig
dev.off()

png(str_c("./plots/FUMA/Income Occupation/model0/gridplot_GS.png"), 
    width = 20, height = 20, units = "in", res = 800)
incocc_model0$gridplot_GS # include
dev.off()
# model1
incocc_model1 <- visualize_FUMA(path_incocc_model1, "Income Occupation", # Data contains no gene sets
                                xlabsize1 = 11, xlabsize2 = 11, xlabsize3 = 12,
                                angle1 = 30, angle2 = 30, angle3 = 30)
make_supp_data_FUMA(incocc_model1, "incocc_model1")

png(str_c("./plots/FUMA/Income Occupation/model1/expression_brainspan.png"), 
    width = 20, height = 25, units = "in", res = 800)
incocc_model1$expression_brainspan
dev.off()

png(str_c("./plots/FUMA/Income Occupation/model1/expression_GTEx.png"), 
    width = 20, height = 25, units = "in", res = 800)
incocc_model1$expression_GTEx
dev.off()

png(str_c("./plots/FUMA/Income Occupation/model1/gridplot_DEG.png"), 
    width = 20, height = 20, units = "in", res = 800)
incocc_model1$gridplot_DEG # include
dev.off()

png(str_c("./plots/FUMA/Income Occupation/model1/gridplot_GS.png"), 
    width = 20, height = 20, units = "in", res = 800)
incocc_model1$gridplot_GS # nothing
dev.off()
# model2
incocc_model2 <- visualize_FUMA(path_incocc_model2, "Income Occupation",
                                xlabsize1 = 11, xlabsize2 = 11, xlabsize3 = 12,
                                angle1 = 30, angle2 = 30, angle3 = 30)
make_supp_data_FUMA(incocc_model2, "incocc_model2")

png(str_c("./plots/FUMA/Income Occupation/model2/expression_brainspan.png"), 
    width = 20, height = 25, units = "in", res = 800)
incocc_model2$expression_brainspan
dev.off()

png(str_c("./plots/FUMA/Income Occupation/model2/expression_GTEx.png"), 
    width = 20, height = 25, units = "in", res = 800)
incocc_model2$expression_GTEx
dev.off()

png(str_c("./plots/FUMA/Income Occupation/model2/gridplot_DEG.png"), 
    width = 20, height = 20, units = "in", res = 800)
incocc_model2$gridplot_DEG # nothing significant
dev.off()

png(str_c("./plots/FUMA/Income Occupation/model2/gridplot_GS.png"), 
    width = 20, height = 20, units = "in", res = 800)
incocc_model2$gridplot_GS # as text
dev.off()

# save models
save(incocc_model0, incocc_model1, incocc_model2,
     file = "./data/FUMA results/models/income occupation.Rdata")


## income region
# model0
increg_model0 <- visualize_FUMA(path_increg_model0, "Income Region",
                                xlabsize1 = 11, xlabsize2 = 11, xlabsize3 = 12,
                                angle1 = 60, angle2 = 60, angle3 = 60)
make_supp_data_FUMA(increg_model0, "increg_model0")

png(str_c("./plots/FUMA/Income Region/model0/expression_brainspan.png"), 
    width = 20, height = 25, units = "in", res = 800)
increg_model0$expression_brainspan
dev.off()

png(str_c("./plots/FUMA/Income Region/model0/expression_GTEx.png"), 
    width = 20, height = 25, units = "in", res = 800)
increg_model0$expression_GTEx
dev.off()

png(str_c("./plots/FUMA/Income Region/model0/gridplot_DEG.png"), 
    width = 20, height = 20, units = "in", res = 800)
increg_model0$gridplot_DEG
dev.off()

png(str_c("./plots/FUMA/Income Region/model0/gridplot_GS.png"), 
    width = 40, height = 20, units = "in", res = 800)
increg_model0$gridplot_GS
dev.off()
# model1
increg_model1 <- visualize_FUMA(path_increg_model1, "Income Region", # Only Hallmark gene sets for NOTCH2, 3, and PSENEN
                                xlabsize1 = 12, xlabsize2 = 12, xlabsize3 = 12,
                                angle1 = 30, angle2 = 30, angle3 = 30)
make_supp_data_FUMA(increg_model1, "increg_model1")

png(str_c("./plots/FUMA/Income Region/model1/expression_brainspan.png"), 
    width = 20, height = 25, units = "in", res = 800)
increg_model1$expression_brainspan
dev.off()

png(str_c("./plots/FUMA/Income Region/model1/expression_GTEx.png"), 
    width = 20, height = 25, units = "in", res = 800)
increg_model1$expression_GTEx
dev.off()

png(str_c("./plots/FUMA/Income Region/model1/gridplot_DEG.png"), 
    width = 20, height = 20, units = "in", res = 800)
increg_model1$gridplot_DEG
dev.off()

png(str_c("./plots/FUMA/Income Region/model1/gridplot_GS.png"), 
    width = 25, height = 20, units = "in", res = 800)
increg_model1$gridplot_GS
dev.off()
# model2
increg_model2 <- visualize_FUMA(path_increg_model2, "Income Region", 
                                xlabsize1 = 11, xlabsize2 = 11, xlabsize3 = 11,
                                angle1 = 30, angle2 = 30, angle3 = 30)
make_supp_data_FUMA(increg_model2, "increg_model2")

png(str_c("./plots/FUMA/Income Region/model2/expression_brainspan.png"), 
    width = 20, height = 25, units = "in", res = 800)
increg_model2$expression_brainspan
dev.off()

png(str_c("./plots/FUMA/Income Region/model2/expression_GTEx.png"), 
    width = 20, height = 25, units = "in", res = 800)
increg_model2$expression_GTEx
dev.off()

png(str_c("./plots/FUMA/Income Region/model2/gridplot_DEG.png"), 
    width = 20, height = 20, units = "in", res = 800)
increg_model2$gridplot_DEG # 1 DEG sig
dev.off()

png(str_c("./plots/FUMA/Income Region/model2/gridplot_GS.png"), 
    width = 25, height = 20, units = "in", res = 800)
increg_model2$gridplot_GS # only GWAScatalog for CPS1, GCKR, IGFBP5, INO80, and SHROOM3
dev.off()

# save models
save(increg_model0, increg_model1, increg_model2,
     file = "./data/FUMA results/models/income region.Rdata")

'''









hsvl_model0$gridplot_DEG
hsvl_model1$gridplot_DEG
hsvl_model2$gridplot_DEG

increg_model0$gridplot_DEG
increg_model1$gridplot_DEG
increg_model2$gridplot_DEG

increg_model0$gridplot_GS
increg_model1$gridplot_GS
increg_model2$gridplot_GS




test1$expression_brainspan

# save figures
test1 <- visualize_FUMA(path_increg_model0, "Income Region",
                        xlabsize1 = 5, xlabsize2 = 5, xlabsize3 = 6,
                        angle1 = 75, angle2 = 75, angle3 = 60,
                        model_nr = "model0")
save_figs(test1, "Income Region", "model0")



















#
increg_model0$GS$GeneSet %>% unique() %>% length()
GeneSet_plots2(GS)
# deze doet nog moeilijk
################################################
"GeneSet_plots" <- function(GeneSetData, rel_height_comboplot = c(1, 2),
                            xlabsize1 = 7, xlabsize2 = 7, xlabsize3 = 7,
                            angle1 = 45, angle2 = 45, angle3 = 45){
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
                                    levels = level_order), 25), 
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
    plot_overlap
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
                                    levels = level_order), 25), 
                    proportion, 
                    fill = value)) +
      geom_col(show.legend = FALSE) +
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
                                    levels = level_order), 25), 
                    (-log10(adjP)), 
                    fill = value)) +
      geom_col(show.legend = FALSE) +
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


#########################################################
GS <- fread(paste0(path_edu_model0, "/GS.txt"))
data <- GS
max_N_overlap <- max(data$N_overlap)
data$Category %>% unique() %>% length()
data$GeneSet %>% unique() %>% length()

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
####################################################################
# find for each category and geneset the number of observations
data %>% group_by(., Category) %>% group_data(.)
increg_model0$GS_incl_prop %>% group_by(., Category) %>% group_data(.)
xx <- data %>% filter(., Category == "Immunologic signatures")
xx$GeneSet %>% unique() %>% length()


increg_model0$GS_incl_prop$GeneSet %>% unique() %>% length()


?n()
data$Category %>% factor() %>% levels() %>% length()
data$GeneSet %>% factor() %>% levels() %>% length()

increg_model0$GS_incl_prop$Category  %>% factor() %>% levels() %>% length()
increg_model0$GS_incl_prop$GeneSet  %>% factor() %>% levels() %>% length()

increg_model0$GS_incl_prop

GS$Category %>% factor() %>% levels() %>% length()
GS$GeneSet %>% factor() %>% levels() %>% length()
#test <- data[match(data[, ""]),]
# df1[match(df2$id, df1$id), ] <- df2

###
start <- Sys.time()
for (i in 1:nrow(data)) {
  for (j in 1:nrow(data6)) {
    for (k in 1:ncol(data7)) {
      if(!is.na(data7[j, k]) & !is.na(data[, "value"]) & data7[j, k] == data[i, "rownr"]){
        data[i, "value"] <- data6[j, "Category"]
        data[i, "Category"] <- data6[j, "Category"]
        
        # duurt lang
        
      }
    }
  }
}
Sys.time() - start
}
###



########################
edu_model1 <- visualize_FUMA(path_edu_model1, "Education",
                             xlabsize1 = 5, xlabsize2 = 5, xlabsize3 = 6,
                             angle1 = 75, angle2 = 75, angle3 = 60)
make_supp_data_FUMA(edu_model1, "edu_model1")
edu_model2 <- visualize_FUMA(path_edu_model2, "Education",
                             xlabsize1 = 5, xlabsize2 = 5, xlabsize3 = 6,
                             angle1 = 75, angle2 = 75, angle3 = 60)
make_supp_data_FUMA(edu_model2, "edu_model2")







test1$gridplot_GS
test1$gridplot_DEG
test1$GS_incl_prop
test1$expression_brainspan
test1$expression_GTEx
# integreren in de FUMA functie dat ie automatisch alles opslaat
# met custom opties voor groottes enzo doen voor speciale plots, bv met if statement!
png("./plots/FUMA/Income Region/test.png", width = 20, height = 25, units = "in", res = 800)
test1$expression_brainspan
dev.off()

png(str_c("./plots/FUMA/Income Region/model0/", "expression_GTEx_TEST.png"), 
    width = 20, height = 25, units = "in", res = 800)
test1$expression_brainspan
dev.off()


###########

ts <- as.numeric(Sys.time())
out_fname <- sprintf("./plots/association_ses_expr_%.0f", ts)
png(str_c(out_fname, ".png"), width = 13, height = 13, units = "in", res = 600)
plot_multi
dev.off()
###########

# nog functie maken voor opslaan van de plots!
plot_grid(test1$expa, test1$rexpa,
          test1$expds, test1$rexpds,
          ncol = 2, nrow = 2)
plot_grid(test1$expt, test1$rexpt,
          test1$gexpt, test1$rgexpt,
          ncol = 2, nrow = 2)



test2 <- visualize_FUMA(path_increg_model2, "Income Region",
                        xlabsize1 = 9, xlabsize2 = 9, xlabsize3 = 9,
                        angle1 = 0, angle2 = 0, angle3 = 0)
test2$gridplot_GS
GS_increg2 <- fread(paste0(path_increg_model2, "/GS.txt"))
GS_increg0 <- fread(paste0(path_increg_model0, "/GS.txt"))
xx <- GeneSet_plots(GS_increg0)
xx$data_incl_prop
