#load packages
library(tibble)
library(ggplot2)
library(dplyr)
devtools::load_all(path = "LundTaxonomy2023Classifier/")

#load data
load("C:/Users/matts/Desktop/lundtax_project/out/predicted/pred_tcga.Rdata")
load("C:/Users/matts/Desktop/lundtax_project/out/predicted/pred_leeds.Rdata")
load("C:/Users/matts/Desktop/lundtax_project/out/predicted/pred_uc_genome.Rdata")
load("C:/Users/matts/Desktop/lundtax_project/datasets/tcga/tcga_salmon_getpm.Rdata")


#source plot_delta function and library size plot
source("lundtax_project/functions/plot_delta.R")
source("lundtax_project/functions/get_library_size.R")

#generate delpa plot data
#tcga
tcga_5 = plot_deltas(these_predictions = pred_tcga, 
                     subtype_class = "5_class", 
                     this_subtype = NULL,
                     return_data = TRUE)

tcga_7 = plot_deltas(these_predictions = pred_tcga, 
                     subtype_class = "7_class", 
                     this_subtype = NULL,
                     return_data = TRUE)

#uc genome
uc_5 = plot_deltas(these_predictions = pred_uc_genome, 
                     subtype_class = "5_class", 
                     this_subtype = NULL,
                     return_data = TRUE)

uc_7 = plot_deltas(these_predictions = pred_uc_genome, 
                     subtype_class = "7_class", 
                     this_subtype = NULL,
                     return_data = TRUE)

#leeds
leeds_5 = plot_deltas(these_predictions = pred_leeds, 
                   subtype_class = "5_class", 
                   this_subtype = NULL,
                   return_data = TRUE)

leeds_7 = plot_deltas(these_predictions = pred_leeds, 
                   subtype_class = "7_class", 
                   this_subtype = NULL,
                   return_data = TRUE)

#define the desired order of levels
desired_levels_5 <- c("Uro", "GU", "BaSq", "Mes", "ScNE")
desired_levels_7 <- c("UroA", "UroB", "UroC")

#update the subtype column to have the specified levels and remove unused levels
tcga_5$subtype <- factor(tcga_5$subtype, levels = desired_levels_5)
tcga_7$subtype <- factor(tcga_7$subtype, levels = desired_levels_7)

uc_5$subtype <- factor(uc_5$subtype, levels = desired_levels_5)
uc_7$subtype <- factor(uc_7$subtype, levels = desired_levels_7)

leeds_5$subtype <- factor(leeds_5$subtype, levels = desired_levels_5)
leeds_7$subtype <- factor(leeds_7$subtype, levels = desired_levels_7)

#remove one BaSq from leeds
leeds_5 = leeds_5 %>% 
  filter(!subtype == "BaSq")

#remove one BaSq from leeds
leeds_7 = leeds_7 %>% 
  filter(!subtype == "UroC")

#remove unused levels
tcga_5$subtype <- droplevels(tcga_5$subtype)
tcga_7$subtype <- droplevels(tcga_7$subtype)

uc_5$subtype <- droplevels(uc_5$subtype)
uc_7$subtype <- droplevels(uc_7$subtype)

leeds_5$subtype <- droplevels(leeds_5$subtype)
leeds_7$subtype <- droplevels(leeds_7$subtype)

#join class data for box
tcga_5 = tcga_5 %>% select(delta, subtype)
tcga_7 = tcga_7 %>% select(delta, subtype)
tcga_box_data = bind_rows(tcga_5, tcga_7)

uc_5 = uc_5 %>% select(delta, subtype)
uc_7 = uc_7 %>% select(delta, subtype)
uc_box_data = bind_rows(uc_5, uc_7)

leeds_5 = leeds_5 %>% select(delta, subtype)
leeds_7 = leeds_7 %>% select(delta, subtype)
leeds_box_data = bind_rows(leeds_5, leeds_7)

#get library sizes
tcga_lib_size = get_library_size(this_data = tcga_salmon_getpm, plot_title = "TCGA", return_plot = FALSE)

#join with delta values
tcga_joined = tcga_5 %>% 
  left_join(tcga_lib_size, by = "sample_id") %>% 
  select(sample_id, library_size, delta)

#order the data frame by the delta column in ascending order
tcga_joined <- tcga_joined[order(tcga_joined$delta), ]

#create the prediction_conf column
tcga_joined$prediction_conf <- ifelse(1:nrow(tcga_joined) <= 50, "low_conf", "high_conf")

#view the updated data frame
head(tcga_joined)

#save the data
save(tcga_joined, file = "lundtax_project/out/manuscript_figs/plot_data/tcga_joined.Rdata")

save(tcga_box_data, file = "lundtax_project/out/manuscript_figs/plot_data/tcga_box_data.Rdata")

save(uc_box_data, file = "lundtax_project/out/manuscript_figs/plot_data/uc_box_data.Rdata")

save(leeds_box_data, file = "lundtax_project/out/manuscript_figs/plot_data/leeds_box_data.Rdata")


