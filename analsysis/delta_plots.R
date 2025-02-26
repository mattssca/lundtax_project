predictions = as.data.frame(pred_tcga$predictions_5classes) %>% 
  rownames_to_column("sample_id") %>% 
  rename(subtype = `pred_tcga$predictions_5classes`)

prediction_scores = as.data.frame(pred_tcga$subtype_scores) %>% 
  rownames_to_column("sample_id") %>% 
  select(sample_id, Uro, GU, BaSq, Mes, ScNE)

#library Size
library_sizes <- colSums(pred_tcga$data, na.rm = TRUE)

#convert to data frame for plotting
library_sizes_df <- data.frame(
  sample_id = names(library_sizes),
  library_size = library_sizes
)

#sort the data frame by library size in descending order
library_sizes_df <- library_sizes_df %>%
  arrange(desc(library_size))

calculate_delta <- function(row) {
  sorted_scores <- sort(row, decreasing = TRUE)
  highest <- sorted_scores[1]
  second_highest <- sorted_scores[2]
  delta <- highest - second_highest
  return(delta)
}

prediction_scores$delta <- apply(prediction_scores[, -1], 1, calculate_delta)

ggplot(prediction_scores, aes(x = delta)) +
  geom_histogram(binwidth = 0.01, fill = "blue", color = "black") +
  labs(title = "Distribution of Delta Scores",
       x = "Delta Score",
       y = "Frequency")

# Define a threshold for low delta (e.g., 0.1)
low_delta_threshold <- 0.25

# Filter samples with low delta
less_confident_samples <- prediction_scores[prediction_scores$delta <= low_delta_threshold, ]

# Merge the data frames to identify which samples to highlight
merged_df <- merge(library_sizes_df, less_confident_samples, by = "sample_id", all.x = TRUE)

# Create a new column to indicate whether each sample is in less_confident_samples
merged_df$highlight <- ifelse(is.na(merged_df$delta), "normal", "low_delta")

# Sort the data frame by library size
merged_df <- merged_df[order(merged_df$library_size), ]

# Create the plot
ggplot(merged_df, aes(x = reorder(sample_id, -library_size), y = library_size, color = highlight)) +
  geom_point() +
  scale_color_manual(values = c("low_delta" = "red", "normal" = "black")) +
  theme_bw() +
  theme(axis.text.x = element_blank(), panel.grid = element_blank(), axis.ticks.x = element_blank()) +
  labs(title = "TCGA",
       x = "Samples",
       y = "Library Size",
       color = "Sample Type")

