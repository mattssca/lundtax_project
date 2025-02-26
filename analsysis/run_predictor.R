#set working directory
setwd(dir = "C:/Users/matts/Desktop/lundtax_project/")

#load packages
library(devtools)
devtools::load_all(path = "../LundTaxonomy2023Classifier/")

#load datasets
load("datasets/tcga/tcga_salmon_getpm.Rdata")
load("datasets/uc/uc_genome_tpm.Rdata")
load("datasets/leeds/leeds_rma.Rdata")

#run predictor
#TCGA
pred_tcga = lundtax_predict_sub(this_data = tcga_salmon_getpm, 
                                adjust = TRUE, 
                                impute = TRUE, 
                                include_data = TRUE, 
                                log_transform = TRUE)

#UC Genome
pred_uc_genome = lundtax_predict_sub(this_data = uc_genome_tpm, 
                                     adjust = TRUE, 
                                     impute = TRUE, 
                                     include_data = TRUE, 
                                     log_transform = TRUE)

#Leeds
pred_leeds = lundtax_predict_sub(this_data = leeds_rma, 
                                 adjust = TRUE, 
                                 impute = TRUE, 
                                 include_data = TRUE, 
                                 log_transform = TRUE)

#export predicted objects
save(pred_tcga, file = "out/predicted/pred_tcga.Rdata")
save(pred_uc_genome, file = "out/predicted/pred_uc_genome.Rdata")
save(pred_leeds, file = "out/predicted/pred_leeds.Rdata")
