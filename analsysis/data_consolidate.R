##TCGA
#load data
load("../DATA/10_datasets/01_tcga/tcga.RData")

#Expression Data
tcga_kallisto_tpm = TCGA_object409$kallisto39_TPM
tcga_kallisto_getpm = TCGA_object409$kallisto39_geTMM
tcga_salmon_tpm = TCGA_object409$salmon39_TPM
tcga_salmon_getpm = TCGA_object409$salmon39_geTMM+
  
#metadata
meta_tcga = TCGA_object409$metadata 

#export datasets
save(tcga_kallisto_tpm, file = "../lundtax_project/datasets/tcga/tcga_kallisto_tpm.Rdata")
save(tcga_kallisto_getpm, file = "../lundtax_project/datasets/tcga/tcga_kallisto_getpm.Rdata")
save(tcga_salmon_tpm, file = "../lundtax_project/datasets/tcga/tcga_salmon_tpm.Rdata")
save(tcga_salmon_getpm, file = "../lundtax_project/datasets/tcga/tcga_salmon_getpm.Rdata")
save(meta_tcga, file = "../lundtax_project/datasets/tcga/meta_tcga.Rdata")

##UC Genome
#expression data
uc_genome_tpm <- readRDS("../lundtax_project/datasets/uc/uc_genome_tpm.rds")

#metadata
meta_uc_genome = read.csv("../lundtax_project/datasets/uc/03_uc_genome_metadata.csv")

#export datasets
save(uc_genome_tpm, file = "../lundtax_project/datasets/uc/uc_genome_tpm.Rdata")
save(meta_uc_genome, file = "../lundtax_project/datasets/uc/meta_uc_genome.Rdata")

##Leeds
#load data
load("C:/Users/matts/Desktop/lundtax_project/datasets/leeds/results_Leeds.RData")

#expression data
leeds_rma = results_Leeds$data_v2

#metadata
meta_leeds =  read.csv("../lundtax_project/datasets/leeds/leeds_metadata.csv")

#export datasets
save(leeds_rma, file = "../lundtax_project/datasets/leeds/leeds_rma.Rdata")
save(meta_leeds, file = "../lundtax_project/datasets/leeds/meta_leeds.Rdata")

