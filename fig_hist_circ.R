#library("lubridate")
library("tidyverse")
library("reshape2")
library("ggplot2")
library("cowplot")
#library("circular")

#ENSG 000 000 572 52
set.seed(201907)
# amount of genes simulated
gene_ass_ses_expr <- paste("ENSG00000", sample(100000:999999, 8, replace = FALSE), sep = "")

# simulate correlations between ses vars and gene expression per gene
ass_ses_expr <- matrix(runif(40, -1, 1), 
                       nrow = 8, ncol = 5) %>%
  as.data.frame()
ass_ses_expr_sig <- matrix(runif(40, 0, 0.10), 
                           nrow = 8, ncol = 5) %>%
  as.data.frame()

# row and colnames
colnames(ass_ses_expr) = colnames(ass_ses_expr_sig) <- c("Income Region", "Income Occupation", "Occupation", 
                                                         "Education", "Average House Price")
rownames(ass_ses_expr) = rownames(ass_ses_expr_sig) <- gene_ass_ses_expr
ass_ses_expr$gene <- rownames(ass_ses_expr)
ass_ses_expr_sig$gene <- rownames(ass_ses_expr_sig)

# reshape wide to long for figures
ass_ses_expr_wide <- melt(ass_ses_expr, 
                          id.vars = c("gene"))
ass_ses_expr_sig_wide <- melt(ass_ses_expr_sig, 
                              id.vars = c("gene"))
ass_ses_expr_wide_final <- left_join(ass_ses_expr_wide, 
                                     ass_ses_expr_sig_wide, 
                                     by = c("gene", "variable"), 
                                     suffix = c("", ".sig"))
ass_ses_expr_wide_final$sig_pos_neg <- rep(NA, nrow(ass_ses_expr_wide_final))
for (i in 1:nrow(ass_ses_expr_wide_final)) {
  if (ass_ses_expr_wide_final[i, 3] > 0) {
    ass_ses_expr_wide_final[i, 5] <- "Positive Association"
  }
  if (ass_ses_expr_wide_final[i, 3] < 0) {
    ass_ses_expr_wide_final[i, 5] <- "Negative Association"
  }
  if (ass_ses_expr_wide_final[i, 4] >= .05) {
    ass_ses_expr_wide_final[i, 5] <- "Not Significant"
  }
}

# define colours
colours <- c(
  "Positive Association" = "green2",
  "Negative Association" = "firebrick",
  "Not Significant" = "Grey70"
)

# break up long variable names
"addline_format" <- function(x,...){
  gsub('\\s','\n', x)
}

# figure ggplot
"fig_association_ses_expr" <- function(what_gene) {
  ass_ses_expr_wide_final %>%
    filter(., gene == what_gene) %>%
    ggplot(., aes(x = factor(variable), y = abs(value), fill = sig_pos_neg)) +
    geom_col(alpha = .7) + 
    coord_polar() + 
    scale_x_discrete(breaks = unique(ass_ses_expr_wide_final$variable),
                     labels = addline_format(c("Income Region", "Income Occupation", 
                                               "Occupation", "Education",
                                             "Average House Price"))) + 
    scale_y_continuous(limits = c(0, 1)) +
    scale_fill_manual(
      values = colours,
      limits = names(colours)) +
    ggtitle(what_gene) +
    theme(plot.title = element_text(hjust = 0.5,
                                    size = 9, 
                                    face = "bold", 
                                    margin = margin(10, 0, 10, 0)),
          axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size = 8),
          legend.title = element_blank(),
          legend.text = element_text(size = 8),
          panel.background = element_rect(fill = 'Grey85')) +
    labs(x = "Indicator of Socioeconomic Status", 
         y = "Absolute Value of Association")
   
  }


# test 1 figure
fig_association_ses_expr("ENSG00000894582")

# multiple figures
plot_multi <- plot_grid(fig_association_ses_expr("ENSG00000894582"), fig_association_ses_expr("ENSG00000530307"),
          fig_association_ses_expr("ENSG00000100607"), fig_association_ses_expr("ENSG00000286136"),
          align = "hv")

# save multiplot
ts = as.numeric(Sys.time())
out_fname = sprintf("./plots/association_ses_expr_%.0f", ts)
png(str_c(out_fname, ".png"), width = 13, height = 13, units = "in", res = 600)
plot_multi
dev.off()
