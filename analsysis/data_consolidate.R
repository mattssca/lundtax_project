#set working directory
setwd(dir = "../lundtax_project/")

##TCGA
#load data
load("../DATA/10_datasets/01_tcga/tcga.RData")

#Expression Data
tcga_salmon_getpm = TCGA_object409$salmon39_geTMM
  
#metadata
meta_tcga = TCGA_object409$metadata 

#export datasets
save(tcga_salmon_getpm, file = "datasets/tcga/tcga_salmon_getpm.Rdata")
save(meta_tcga, file = "datasets/tcga/meta_tcga.Rdata")

##UC Genome
#expression data
uc_genome_tpm <- readRDS("datasets/uc/uc_genome_tpm.rds")

#metadata
meta_uc_genome = read.csv("datasets/uc/03_uc_genome_metadata.csv")

#export datasets
save(uc_genome_tpm, file = "datasets/uc/uc_genome_tpm.Rdata")
save(meta_uc_genome, file = "datasets/uc/meta_uc_genome.Rdata")

##Leeds
#load data
load("C:/Users/matts/Desktop/lundtax_project/datasets/leeds/results_Leeds.RData")

#expression data
leeds_rma = results_Leeds$data_v2

#metadata
meta_leeds =  read.csv("datasets/leeds/leeds_metadata.csv")

#export datasets
save(leeds_rma, file = "datasets/leeds/leeds_rma.Rdata")
save(meta_leeds, file = "datasets/leeds/meta_leeds.Rdata")

