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

png(str_c("./plots/FUMA/Income Occupation/model1/expression_brainspan_age.png"), 
    width = 20, height = 25, units = "in", res = 800)
incocc_model$expression_brainspan_age
dev.off()

png(str_c("./plots/FUMA/Income Occupation/model1/expression_brainspan_dev.png"), 
    width = 20, height = 25, units = "in", res = 800)
incocc_model$expression_brainspan_dev
dev.off()

png(str_c("./plots/FUMA/Income Occupation/model1/expression_GTEx_tissue.png"), 
    width = 20, height = 25, units = "in", res = 800)
incocc_model$expression_GTEx_tissue
dev.off()

png(str_c("./plots/FUMA/Income Occupation/model1/expression_GTEx_gen_tissue.png"), 
    width = 20, height = 25, units = "in", res = 800)
incocc_model$expression_GTEx_gen_tissue
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

png(str_c("./plots/FUMA/Education/model1/expression_brainspan_age.png"), 
    width = 20, height = 25, units = "in", res = 800)
edu_model$expression_brainspan_age
dev.off()

png(str_c("./plots/FUMA/Education/model1/expression_brainspan_dev.png"), 
    width = 20, height = 25, units = "in", res = 800)
edu_model$expression_brainspan_dev
dev.off()

png(str_c("./plots/FUMA/Education/model1/expression_GTEx_tissue.png"), 
    width = 20, height = 25, units = "in", res = 800)
edu_model$expression_GTEx_tissue
dev.off()

png(str_c("./plots/FUMA/Education/model1/expression_GTEx_gen_tissue.png"), 
    width = 20, height = 25, units = "in", res = 800)
edu_model$expression_GTEx_gen_tissue
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

png(str_c("./plots/FUMA/House Value/model1/expression_brainspan_age.png"), 
    width = 20, height = 25, units = "in", res = 800)
hsvl_model$expression_brainspan_age
dev.off()

png(str_c("./plots/FUMA/House Value/model1/expression_brainspan_dev.png"), 
    width = 20, height = 25, units = "in", res = 800)
hsvl_model$expression_brainspan_dev
dev.off()

png(str_c("./plots/FUMA/House Value/model1/expression_GTEx_tissue.png"), 
    width = 20, height = 25, units = "in", res = 800)
hsvl_model$expression_GTEx_tissue
dev.off()

png(str_c("./plots/FUMA/House Value/model1/expression_GTEx_gen_tissue.png"), 
    width = 20, height = 25, units = "in", res = 800)
hsvl_model$expression_GTEx_gen_tissue
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

png(str_c("./plots/FUMA/Income Region/model1/expression_brainspan_age.png"), 
    width = 20, height = 25, units = "in", res = 800)
increg_model$expression_brainspan_age
dev.off()

png(str_c("./plots/FUMA/Income Region/model1/expression_brainspan_dev.png"), 
    width = 20, height = 25, units = "in", res = 800)
increg_model$expression_brainspan_dev
dev.off()

png(str_c("./plots/FUMA/Income Region/model1/expression_GTEx_tissue.png"), 
    width = 20, height = 25, units = "in", res = 800)
increg_model$expression_GTEx_tissue
dev.off()

png(str_c("./plots/FUMA/Income Region/model1/expression_GTEx_gen_tissue.png"), 
    width = 20, height = 25, units = "in", res = 800)
increg_model$expression_GTEx_gen_tissue
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
