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
