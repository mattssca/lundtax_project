#load necessary libraries
library(dplyr)
library(ggplot2)
library(pheatmap)

get_library_size = function(this_data, 
                            plot_title = NULL,
                            return_plot = TRUE){
  
  #library Size
  library_sizes <- colSums(this_data, na.rm = TRUE)
  print(library_sizes)
  
  #convert to data frame for plotting
  library_sizes_df <- data.frame(
    sample = names(library_sizes),
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

#generate tables
tcga_lib_size = get_library_size(this_data = tcga_salmon_getpm, plot_title = "TCGA", return_plot = FALSE)
uc_lib_size = get_library_size(this_data = uc_genome_tpm, plot_title = "UC Genome", return_plot = FALSE)
leeds_lib_size = get_library_size(this_data = leeds_rma, plot_title = "Leeds", return_plot = FALSE)

#export for later use
save(tcga_lib_size, file = "out/tables/tcga_library_size.Rdata")
save(uc_lib_size, file = "out/tables/uc_library_size.Rdata")
save(leeds_lib_size, file = "out/tables/leeds_library_size.Rdata")

