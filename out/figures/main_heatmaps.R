devtools::load_all("../Desktop/LundTaxonomy2023Classifier/")

plot_hm_signatures_manuscript(these_predictions = pred_tcga, 
                              subtype_annotation = "7_class", 
                              norm = TRUE, 
                              ann_height = 7,
                              plot_width = 14,
                              plot_height = 10,
                              plot_title = "TCGA", 
                              out_path = "../Desktop/lundtax_project/out/manuscript_figs/heatmaps/", 
                              out_format = "pdf")

plot_hm_signatures_manuscript(these_predictions = pred_uc_genome, 
                              subtype_annotation = "7_class", 
                              norm = TRUE, 
                              ann_height = 7,
                              plot_width = 6,
                              plot_height = 10,
                              plot_title = "UC Genome", 
                              out_path = "../Desktop/lundtax_project/out/manuscript_figs/heatmaps/", 
                              out_format = "pdf")

plot_hm_signatures_manuscript(these_predictions = pred_leeds, 
                              subtype_annotation = "7_class", 
                              norm = TRUE, 
                              ann_height = 7,
                              plot_width = 7.4,
                              plot_height = 10,
                              plot_title = "Leeds", 
                              out_path = "../Desktop/lundtax_project/out/manuscript_figs/heatmaps/", 
                              out_format = "pdf")

plot_hm_scores_manuscript(these_predictions = pred_tcga, 
                          out_path = "../Desktop/lundtax_project/out/manuscript_figs/heatmaps/",
                          out_format = "pdf", 
                          plot_title = "TCGA_scores", 
                          subtype_annotation = "7_class",
                          plot_width = 14,
                          plot_height = 6,)

plot_hm_scores_manuscript(these_predictions = pred_uc_genome, 
                          out_path = "../Desktop/lundtax_project/out/manuscript_figs/heatmaps/",
                          out_format = "pdf", 
                          plot_title = "UC_Genome_scores", 
                          subtype_annotation = "7_class",
                          plot_width = 6,
                          plot_height = 6,)

plot_hm_scores_manuscript(these_predictions = pred_leeds, 
                          out_path = "../Desktop/lundtax_project/out/manuscript_figs/heatmaps/",
                          out_format = "pdf", 
                          plot_title = "LeedsA_scores", 
                          subtype_annotation = "7_class",
                          plot_width = 7.4,
                          plot_height = 6,)
