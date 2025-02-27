library(fmsb)
library(dplyr)
library(tidyr)

plot_data = plot_deltas(these_predictions = pred_tcga, 
                        subtype_class = "5_class", 
                        this_subtype = NULL,
                        return_data = TRUE)

# Prepare data for radar chart
prepare_radar_data <- function(plot_data) {
  radar_data <- plot_data %>%
    select(-delta) %>%
    gather(key = "subtype_score", value = "score", -sample_id, -subtype) %>%
    spread(key = "subtype_score", value = "score")
  
  # Add max and min values for radar chart scaling
  radar_data <- rbind(rep(1, ncol(radar_data)-1), rep(0, ncol(radar_data)-1), radar_data)
  return(radar_data)
}

# Function to plot radar chart for a given sample
plot_radar_chart <- function(this_sample_id, radar_data) {
  sample_data <- radar_data %>% filter(sample_id == this_sample_id)
  if (nrow(sample_data) == 0) {
    stop("Sample ID not found in the data.")
  }
  
  # Combine the max, min, and sample data for plotting
  plot_data <- rbind(radar_data[1:2, ], sample_data)
  
  radarchart(plot_data[, -1], axistype = 1,
             pcol = c("red", "blue", "green"),
             pfcol = c("red", "blue", "green"),
             plwd = 2,
             cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0, 1, 0.2), cglwd = 0.8,
             vlcex = 0.8)
  title(main = paste("Radar Chart of Subtype Scores for Sample", this_sample_id))
}

# Prepare the radar data
radar_data <- prepare_radar_data(plot_data)

# Example usage: Plot radar chart for a specific sample
plot_radar_chart(this_sample_id = "TCGA-4Z-AA7Q-01A",
                 radar_data = radar_data)
