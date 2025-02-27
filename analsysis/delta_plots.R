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
  
  my_plot <- ggplot(prediction_scores, aes(x = reorder(sample_id, -delta), y = delta, group = subtype, color = subtype)) +
    geom_line() +
    geom_point(size = 0.8) +
    scale_color_manual(values = my_palette) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0.0", "0.25", "0.5", "0.75", "1.0")) +
    scale_x_discrete(expand = c(0, 0)) +
    labs(title = plot_title,
         x = "Samples",
         y = "Delta") +
    theme(axis.ticks.x = element_blank(), 
          axis.text.x = element_blank(), 
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.4),
          axis.line.x = element_blank())
  
  return(my_plot)
}

plot_wrapper = function(these_predictions = NULL, 
                        plot_title = NULL,
                        out_path = NULL){
  
  class_5_plot = plot_deltas(these_predictions = these_predictions,
                             this_subtype = NULL,
                             subtype_class = "5_class",
                             plot_title = plot_title, 
                             return_data = FALSE)
  
  class_7_plot = plot_deltas(these_predictions = these_predictions,
                             this_subtype = NULL,
                             subtype_class = "7_class",
                             plot_title = plot_title, 
                             return_data = FALSE)
  
  uro_plot = plot_deltas(these_predictions = these_predictions,
                         this_subtype = "Uro",
                         subtype_class = "5_class",
                         plot_title = plot_title, 
                         return_data = FALSE)
  
  gu_plot = plot_deltas(these_predictions = these_predictions,
                        this_subtype = "GU",
                        subtype_class = "5_class",
                        plot_title = plot_title, 
                        return_data = FALSE)
  
  basq_plot = plot_deltas(these_predictions = these_predictions,
                          this_subtype = "BaSq",
                          subtype_class = "5_class",
                          plot_title = plot_title, 
                          return_data = FALSE)
  
  mes_plot = plot_deltas(these_predictions = these_predictions,
                         this_subtype = "Mes",
                         subtype_class = "5_class",
                         plot_title = plot_title, 
                         return_data = FALSE)
  
  scne_plot = plot_deltas(these_predictions = these_predictions,
                          this_subtype = "ScNE",
                          subtype_class = "5_class",
                          plot_title = plot_title, 
                          return_data = FALSE)
  
  uroa_plot = plot_deltas(these_predictions = these_predictions,
                          this_subtype = "UroA",
                          subtype_class = "7_class",
                          plot_title = plot_title, 
                          return_data = FALSE)
  
  urob_plot = plot_deltas(these_predictions = these_predictions,
                          this_subtype = "UroB",
                          subtype_class = "7_class",
                          plot_title = plot_title, 
                          return_data = FALSE)
  
  uroc_plot = plot_deltas(these_predictions = these_predictions,
                          this_subtype = "UroC",
                          subtype_class = "7_class",
                          plot_title = plot_title, 
                          return_data = FALSE)

  pdf(file = paste0(out_path, "delta_", plot_title, "_uro.pdf"), width = 8, height = 7)
  print(uro_plot)
  dev.off()
  pdf(file = paste0(out_path, "delta_", plot_title, "_gu.pdf"), width = 8, height = 7)
  print(gu_plot)
  dev.off()
  pdf(file = paste0(out_path, "delta_", plot_title, "_basq.pdf"), width = 8, height = 7)
  print(basq_plot)
  dev.off()
  pdf(file = paste0(out_path, "delta_", plot_title, "_mes.pdf"), width = 8, height = 7)
  print(mes_plot)
  dev.off()
  pdf(file = paste0(out_path, "delta_", plot_title, "_scne.pdf"), width = 8, height = 7)
  print(scne_plot)
  dev.off()
  pdf(file = paste0(out_path, "delta_", plot_title, "_uroa.pdf"), width = 8, height = 7)
  print(uroa_plot)
  dev.off()
  pdf(file = paste0(out_path, "delta_", plot_title, "_urob.pdf"), width = 8, height = 7)
  print(urob_plot)
  dev.off()
  pdf(file = paste0(out_path, "delta_", plot_title, "_uroc.pdf"), width = 8, height = 7)
  print(uroc_plot)
  dev.off()
  pdf(file = paste0(out_path, "delta_", plot_title, "_class_5.pdf"), width = 8, height = 7)
  print(class_5_plot)
  dev.off()
  pdf(file = paste0(out_path, "delta_", plot_title, "_class_7.pdf"), width = 8, height = 7)
  print(class_7_plot)
  dev.off()
  
  plot_data_5 = plot_deltas(these_predictions = these_predictions, 
                          subtype_class = "5_class", 
                          this_subtype = NULL,
                          return_data = TRUE)
  
  plot_data_7 = plot_deltas(these_predictions = these_predictions, 
                          subtype_class = "7_class", 
                          this_subtype = NULL,
                          return_data = TRUE)
  
  desired_order_5 <- c("Uro", "GU", "BaSq", "Mes", "ScNE")
  plot_data_5$subtype <- factor(plot_data_5$subtype, levels = desired_order_5)
  
  desired_order_7 <- c("UroA", "UroB", "UroC")
  plot_data_7$subtype <- factor(plot_data_7$subtype, levels = desired_order_7)
  
  
  pdf(file = paste0(out_path, "delta_dist_box_", plot_title, "_5_class.pdf"), width = 8, height = 7)
  print(ggplot(plot_data_5, aes(x = subtype, y = delta, fill = subtype)) +
    geom_boxplot() +
    ylim(0, 1) +
    scale_fill_manual(values = lund_colors$lund_colors) +
    theme_bw() +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank()) +
    labs(title = paste0(plot_title, " 5_class"),
         subtitle = "Distribution of Delta Values by Subtype",
         x = "Subtype",
         y = "Delta"))
  dev.off()
  
  pdf(file = paste0(out_path, "delta_dist_box_", plot_title, "_7_class.pdf"), width = 8, height = 7)
  print(ggplot(plot_data_7, aes(x = subtype, y = delta, fill = subtype)) +
    geom_boxplot() +
    ylim(0, 1) +
    scale_fill_manual(values = lund_colors$lund_colors) +
    theme_bw() +
      theme(panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_blank()) +
    labs(title = paste0(plot_title, " 7_class"),
         subtitle = "Distribution of Delta Values by Subtype",
         x = "Subtype",
         y = "Delta"))
  dev.off()
  
  pdf(file = paste0(out_path, "delta_dens_", plot_title, "_5_class.pdf"), width = 8, height = 7)
  print(ggplot(plot_data_5, aes(x = delta, fill = subtype)) +
    geom_density(alpha = 0.6) +
    scale_fill_manual(values = lund_colors$lund_colors) +
    theme_bw() +
      theme(panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_blank()) +
    labs(title = paste0(plot_title, " 5_class"),
         subtitle = "Delta Values Density by Subtype",
         x = "Delta",
         y = "Density"))
  dev.off()
  
  pdf(file = paste0(out_path, "delta_dens_", plot_title, "_7_class.pdf"), width = 8, height = 7)
  print(ggplot(plot_data_7, aes(x = delta, fill = subtype)) +
    geom_density(alpha = 0.6) +
    scale_fill_manual(values = lund_colors$lund_colors) +
    theme_bw() +
      theme(panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_blank()) +
    labs(title = paste0(plot_title, " 7_class"),
         subtitle = "Delta Values Density by Subtype",
         x = "Delta",
         y = "Density"))
  dev.off()
  
  pdf(file = paste0(out_path, "scatter_", plot_title, "_5_class.pdf"), width = 7, height = 8)
  print(ggplot(plot_data_5, aes(x = sample_id, y = delta, color = subtype)) +
    geom_point() +
    ylim(0, 1) +    
    scale_color_manual(values = lund_colors$lund_colors) +
    theme_bw() +
      theme(panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_blank()) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    labs(title = paste0(plot_title, " 5_class"),
         subtitle = "Scatter Plot of Delta Values by Subtype",
         x = "Sample ID",
         y = "Delta"))
  dev.off()
  
  
  pdf(file = paste0(out_path, "scatter_", plot_title, "_7_class.pdf"), width = 7, height = 8)
  print(ggplot(plot_data_7, aes(x = sample_id, y = delta, color = subtype)) +
    geom_point() +
    ylim(0, 1) +  
    scale_color_manual(values = lund_colors$lund_colors) +
    theme_bw() +
      theme(panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_blank()) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    labs(title = paste0(plot_title, " 7_class"),
         subtitle = "Scatter Plot of Delta Values by Subtype",
         x = "Sample ID",
         y = "Delta"))
  dev.off()
  
  pdf(file = paste0(out_path, "mean_", plot_title, "_5_class.pdf"), width = 7, height = 8)
  print(ggplot(plot_data_5, aes(x = subtype, y = delta, fill = subtype)) +
    stat_summary(fun = mean, geom = "bar") +
    scale_fill_manual(values = lund_colors$lund_colors) +
    ylim(0, 1) +
    theme_bw() +
      theme(panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_blank()) +
    labs(title = paste0(plot_title, " 5_class"),
         subtitle = "Mean Delta Values by Subtype",
         x = "Subtype",
         y = "Mean Delta"))
  dev.off()
  
  pdf(file = paste0(out_path, "mean_", plot_title, "_7_class.pdf"), width = 7, height = 8)
  print(ggplot(plot_data_7, aes(x = subtype, y = delta, fill = subtype)) +
    stat_summary(fun = mean, geom = "bar") +
    scale_fill_manual(values = lund_colors$lund_colors) +
    ylim(0, 1) +
    theme_bw() +
      theme(panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_blank()) +
    labs(title = paste0(plot_title, " 7_class"),
         subtitle = "Mean Delta Values by Subtype",
         x = "Subtype",
         y = "Mean Delta"))
  dev.off()
  
  pdf(file = paste0(out_path, "jitter_", plot_title, "_5_class.pdf"), width = 7, height = 8)
  print(ggplot(plot_data_5, aes(x = subtype, y = delta, color = subtype)) +
    geom_jitter(width = 0.2) +
    ylim(0, 1) +  
    scale_color_manual(values = lund_colors$lund_colors) +
    theme_bw() +
      theme(panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_blank()) +
    labs(title = paste0(plot_title, " 5_class"),
         subtitle = "Jitter Plot of Delta Values by Subtype",
         x = "Subtype",
         y = "Delta"))
  dev.off()
  
  pdf(file = paste0(out_path, "jitter_", plot_title, "_7_class.pdf"), width = 7, height = 8)
  print(ggplot(plot_data_7, aes(x = subtype, y = delta, color = subtype)) +
    geom_jitter(width = 0.2) +
    ylim(0, 1) +  
    scale_color_manual(values = lund_colors$lund_colors) +
    theme_bw() +
      theme(panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_blank()) +
    labs(title = paste0(plot_title, " 7_class"),
         subtitle = "Jitter Plot of Delta Values by Subtype",
         x = "Subtype",
         y = "Delta"))
  dev.off()
  
  facet_data_5 <- plot_data_5 %>%
    select(-delta) %>%
    gather(key = "subtype_score", value = "score", -sample_id, -subtype)
  
  facet_data_7 <- plot_data_7 %>%
    select(-delta) %>%
    gather(key = "subtype_score", value = "score", -sample_id, -subtype)
  
  pdf(file = paste0(out_path, "sub_score_facet_", plot_title, "_5_class.pdf"), width = 8, height = 7)
  print(ggplot(facet_data_5, aes(x = sample_id, y = score)) +
    geom_point(aes(color = subtype)) +
    ylim(0, 1) +
    scale_color_manual(values = lund_colors$lund_colors) +
    theme_bw() +
      theme(panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_blank()) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    facet_wrap(~ subtype_score, scales = "free_x") +
    labs(title = paste0(plot_title, " 5_class"),
         subtitle = "Subtype Prediction Score by Subtype",
         x = "Sample ID",
         y = "Score"))
  dev.off()
  
  pdf(file = paste0(out_path, "sub_score_facet_", plot_title, "_7_class.pdf"), width = 8, height = 7)
  print(ggplot(facet_data_7, aes(x = sample_id, y = score)) +
    geom_point(aes(color = subtype)) +
    ylim(0, 1) +
    scale_color_manual(values = lund_colors$lund_colors) +
    theme_bw() +
      theme(panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_blank()) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    facet_wrap(~ subtype_score, scales = "free_x") +
    labs(title = paste0(plot_title, " 7_class"),
         subtitle = "Subtype Prediction Score by Subtype",
         x = "Sample ID",
         y = "Score"))
  dev.off()
}

plot_wrapper(these_predictions = pred_tcga, 
             plot_title = "tcga", 
             out_path = "lundtax_project/out/figures/tcga/")  

plot_wrapper(these_predictions = pred_uc_genome, 
             plot_title = "uc_genome", 
             out_path = "lundtax_project/out/figures/uc/")  

plot_wrapper(these_predictions = pred_leeds, 
             plot_title = "leeds", 
             out_path = "lundtax_project/out/figures/leeds/") 

plot_wrapper(these_predictions = pred_uroscanseq_high_low_batch_fixed_grade_score, 
             plot_title = "uroscanseq", 
             out_path = "lundtax_project/out/figures/uroscanseq_ref/") 

data = plot_deltas(these_predictions = pred_uroscanseq_high_low_batch_fixed_grade_score, return_data = TRUE, subtype_class = "5_class")
 