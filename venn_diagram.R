library("tidyverse")
library("gplots")

############################# hieronder is pas werkend
## construct some fake gene names..
oneName <- function() paste(sample(LETTERS, 5, replace = TRUE), collapse = "")
geneNames <- replicate(1000, oneName())
geneNames <- paste("gene", 1:6000, sep = "")
##
GroupA <- sample(geneNames, 4000, replace = FALSE)
GroupB <- sample(geneNames, 4000, replace = FALSE)
GroupC <- sample(geneNames, 4000, replace = FALSE)
GroupD <- sample(geneNames, 4000, replace = FALSE)
GroupE <- sample(geneNames, 4000, replace = FALSE)

input  <-list("Income Region" = GroupA, 
              "Income Occupation" = GroupB, 
              "Occupation" = GroupC, 
              "Education" = GroupD, 
              "Average House Price" = GroupE)
# graphical parameters changed for size input labels
par(cex = .8)
# venn diagram
tmp <- venn(input)

# save multiplot
ts = as.numeric(Sys.time())
out_fname = sprintf("./plots/venn_ses_expr_%.0f", ts)
png(str_c(out_fname, ".png"), width = 13, height = 13, units = "in", res = 700)
venn(input)
dev.off()


