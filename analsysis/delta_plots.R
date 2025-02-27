plot_deltas <- function(these_predictions = NULL,
                        subtype_class = "5_class",
                        this_subtype = NULL,
                        return_data = FALSE, 
                        plot_title = NULLL){
  
  #deal with subtype class
  if(subtype_class == "5_class"){
    message("5 Class selected...")
    predictions <- as.data.frame(these_predictions$predictions_5classes) %>% 
      rownames_to_column("sample_id") %>% 
      rename(subtype = `these_predictions$predictions_5classes`)
  } else if(subtype_class == "7_class"){
    message("7 Class selected...")
    predictions <- as.data.frame(these_predictions$predictions_7classes) %>% 
      rownames_to_column("sample_id") %>% 
      rename(subtype = `these_predictions$predictions_7classes`) %>% 
      filter(subtype %in% c("UroA", "UroB", "UroC"))
  }

  #get sample IDs of interest
  these_samples <- predictions$sample_id
  
  #deal with prediction scores
  prediction_scores <- as.data.frame(these_predictions$subtype_scores) %>% 
    rownames_to_column("sample_id")
  
  #subset to predictions cores of interest
  message("Subset prediction scores to selected subtype...")
  if(subtype_class == "5_class"){
    prediction_scores <- prediction_scores %>% 
      select(sample_id, Uro, GU, BaSq, Mes, ScNE) %>% 
      filter(sample_id %in% these_samples)
  }else if(subtype_class == "7_class"){
    prediction_scores <- prediction_scores %>% 
      select(sample_id, UroA, UroB, UroC) %>% 
      filter(sample_id %in% these_samples)
  }
  
  #define helper
  calculate_delta <- function(row) {
    sorted_scores <- sort(row, decreasing = TRUE, na.last = NA)
    highest <- sorted_scores[1]
    second_highest <- sorted_scores[2]
    delta <- highest - second_highest
    return(delta)
  }
  
  #run helper
  message("Running helper...")
  prediction_scores$delta <- apply(prediction_scores[, -1], 1, calculate_delta)
  
  #join with predicted subtype
  prediction_scores <- prediction_scores %>% 
    left_join(predictions, by = "sample_id")
  
  #filter on subtype
  if(!is.null(this_subtype)){
    message("Filtering on selected subtype...")
    prediction_scores = prediction_scores %>% 
      filter(subtype %in% this_subtype)
  }

  #convert subtype to factor
  prediction_scores <- prediction_scores %>% 
    mutate(subtype = as.factor(subtype))
  
  message(paste0(nrow(prediction_scores), " samples kept..."))
  
  #create the plot
  message("Creating plot...")
  
  #get colours
  my_palette <- lund_colors$lund_colors
  
  if(return_data){
    message("returning prediction scores with delta values...")
    return(prediction_scores)
  }
  
  my_plot <- ggplot(prediction_scores, aes(x = reorder(sample_id, delta), y = delta, group = subtype, color = subtype)) +
    geom_line() +
    geom_point(size = 0.5) +
    scale_color_manual(values = my_palette) +
    scale_y_reverse(expand = c(0, 0)) +
    scale_x_discrete(expand = c(0, 0)) +
    labs(title = plot_title,
         x = "Samples",
         y = "Delta") +
    theme(legend.position = c(0.85, 0.3), 
          legend.background = element_rect(fill = alpha('white', 0.5)), 
          axis.text.y = element_text(color = "black", size = 7),
          axis.ticks.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.ticks.y = element_line(linewidth = 0.4),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.4),
          axis.line.x = element_blank(),
          axis.title.y = element_text(angle = 90, colour = "black", size = 10, vjust = 2))
  
  return(my_plot)
}

class_5_plot = plot_deltas(these_predictions = pred_tcga,
                           this_subtype = NULL,
                           subtype_class = "5_class",
                           plot_title = "TCGA", 
                           return_data = FALSE)

class_7_plot = plot_deltas(these_predictions = pred_tcga,
                           this_subtype = NULL,
                           subtype_class = "7_class",
                           plot_title = "TCGA", 
                           return_data = FALSE)

uro_plot = plot_deltas(these_predictions = pred_tcga,
                       this_subtype = "Uro",
                       subtype_class = "5_class",
                       plot_title = "TCGA", 
                       return_data = FALSE)

gu_plot = plot_deltas(these_predictions = pred_tcga,
                      this_subtype = "GU",
                      subtype_class = "5_class",
                      plot_title = "TCGA", 
                      return_data = FALSE)

basq_plot = plot_deltas(these_predictions = pred_tcga,
                        this_subtype = "BaSq",
                        subtype_class = "5_class",
                        plot_title = "TCGA", 
                        return_data = FALSE)

mes_plot = plot_deltas(these_predictions = pred_tcga,
                       this_subtype = "Mes",
                       subtype_class = "5_class",
                       plot_title = "TCGA", 
                       return_data = FALSE)

scne_plot = plot_deltas(these_predictions = pred_tcga,
                        this_subtype = "ScNE",
                        subtype_class = "5_class",
                        plot_title = "TCGA", 
                        return_data = FALSE)

uroa_plot = plot_deltas(these_predictions = pred_tcga,
                        this_subtype = "UroA",
                        subtype_class = "7_class",
                        plot_title = "TCGA", 
                        return_data = FALSE)

urob_plot = plot_deltas(these_predictions = pred_tcga,
                        this_subtype = "UroB",
                        subtype_class = "7_class",
                        plot_title = "TCGA", 
                        return_data = FALSE)

uroc_plot = plot_deltas(these_predictions = pred_tcga,
                        this_subtype = "UroC",
                        subtype_class = "7_class",
                        plot_title = "TCGA", 
                        return_data = FALSE)

pdf(file = "lundtax_project/out/figures/delta_tcga_uro.pdf" ,width = 11, height = 7)
print(uro_plot)
dev.off()
pdf(file = "lundtax_project/out/figures/delta_tcga_gu.pdf" ,width = 11, height = 7)
print(gu_plot)
dev.off()
pdf(file = "lundtax_project/out/figures/delta_tcga_basq.pdf" ,width = 11, height = 7)
print(basq_plot)
dev.off()
pdf(file = "lundtax_project/out/figures/delta_tcga_mes.pdf" ,width = 11, height = 7)
print(mes_plot)
dev.off()
pdf(file = "lundtax_project/out/figures/delta_tcga_scne.pdf" ,width = 11, height = 7)
print(scne_plot)
dev.off()
pdf(file = "lundtax_project/out/figures/delta_tcga_uroa.pdf" ,width = 11, height = 7)
print(uroa_plot)
dev.off()
pdf(file = "lundtax_project/out/figures/delta_tcga_urob.pdf" ,width = 11, height = 7)
print(urob_plot)
dev.off()
pdf(file = "lundtax_project/out/figures/delta_tcga_uroc.pdf" ,width = 11, height = 7)
print(uroc_plot)
dev.off()
pdf(file = "lundtax_project/out/figures/delta_tcga_class_5.pdf" ,width = 11, height = 7)
print(class_5_plot)
dev.off()
pdf(file = "lundtax_project/out/figures/delta_tcga_class_7.pdf" ,width = 11, height = 7)
print(class_7_plot)
dev.off()

