get_library_size = function(this_data, 
                            plot_title = NULL,
                            return_plot = TRUE){
  
  #library Size
  library_sizes <- colSums(this_data, na.rm = TRUE)

  #convert to data frame for plotting
  library_sizes_df <- data.frame(
    sample_id = names(library_sizes),
    library_size = library_sizes
  )
  
  #sort the data frame by library size in descending order
  library_sizes_df <- library_sizes_df %>%
    arrange(desc(library_size))
  
  if(return_plot){
    my_plot = ggplot(library_sizes_df, aes(x = reorder(sample, -library_size), y = library_size, group = 1)) +
      geom_line(linewidth = 0.7) +
      theme_bw() +
      theme(axis.text.x = element_blank(), panel.grid = element_blank(), axis.ticks.x = element_blank()) +
      labs(title = plot_title, x = "Samples", y = "Total Reads")
    
    return(my_plot)  
  }else{
    return(library_sizes_df)
  }
}
