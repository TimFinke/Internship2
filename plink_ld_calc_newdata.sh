./plink2 --bfile ./data/clump/plink/ALL.chr$1.phase3_v5.shapeit2_mvncall_integrated.noSingleton.genotypes.vcf.gz --extract ./data/LD/SNPlist_EA_clumped.txt --r2 --ld-window-r2 .1 --ld-snp-list ./data/LD/$3 --out ./data/LD/results_newdata/LD.$2.$1