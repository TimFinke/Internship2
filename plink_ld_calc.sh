./plink2 --bfile ./data/clump/1000G_EUR_Phase3_plink/1000G.EUR.QC.$1 --extract ./data/LD/SNPlist_EA_clumped.txt --r2 --ld-window-r2 .1 --ld-snp-list ./data/LD/$3 --out ./data/LD/results/LD.$2.$1