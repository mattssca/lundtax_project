spit_plots = function(these_predictions = NULL, 
                      out_path = NULL, 
                      data_name = NULL, 
                      file_name = NULL){
  
  class_5_data = plot_deltas(these_predictions = pred_leeds,
                             this_subtype = NULL,
                             subtype_class = "5_class",
                             return_data = TRUE)
  
  class_7_data = plot_deltas(these_predictions = these_predictions,
                             this_subtype = NULL,
                             subtype_class = "7_class",
                             return_data = TRUE)
  
  
  desired_order_5 <- c("Uro", "GU", "BaSq", "Mes", "ScNE")
  class_5_data$subtype <- factor(class_5_data$subtype, levels = desired_order_5)
  
  desired_order_7 <- c("UroA", "UroB", "UroC")
  class_7_data$subtype <- factor(class_7_data$subtype, levels = desired_order_7)
  
  summarized_data_5 <- class_5_data %>%
    group_by(subtype) %>%
    summarize(sum_Uro = sum(Uro),
              sum_GU = sum(GU),
              sum_BaSq = sum(BaSq),
              sum_Mes = sum(Mes),
              sum_ScNE = sum(ScNE))
  
  scaled_data_5 <- summarized_data_5 %>%
    mutate(`Uro Score` = sum_Uro / max(sum_Uro),
           `GU Score` = sum_GU / max(sum_GU),
           `BaSq Score` = sum_BaSq / max(sum_BaSq),
           `Mes Score` = sum_Mes / max(sum_Mes),
           `ScNE Score` = sum_ScNE / max(sum_ScNE))
  
  summarized_data_7 <- class_7_data %>%
    group_by(subtype) %>%
    summarize(sum_UroA = sum(UroA),
              sum_UroB = sum(UroB),
              sum_UroC = sum(UroC))
  
  scaled_data_7 <- summarized_data_7 %>%
    mutate(`UroA Score` = sum_UroA / max(sum_UroA),
           `UroB Score` = sum_UroB / max(sum_UroB),
           `UroC Score` = sum_UroC / max(sum_UroC))
  
  sample_counts_5 <- class_5_data %>%
    group_by(subtype) %>%
    summarise(num_samples = n())
  
  # Join the sample counts with the original data
  scaled_data_5 <- scaled_data_5 %>%
    left_join(sample_counts_5, by = "subtype")
  
  # Normalize the sum of scores by the number of samples in each subtype
  scaled_data_5 <- scaled_data_5 %>%
    mutate(`Uro Score` = sum_Uro / num_samples,
           `GU Score` = sum_GU / num_samples,
           `BaSq Score` = sum_BaSq / num_samples,
           `Mes Score` = sum_Mes / num_samples,
           `ScNE Score` = sum_ScNE / num_samples)
  
  sample_counts_7 <- class_7_data %>%
    group_by(subtype) %>%
    summarise(num_samples = n())
  
  # Join the sample counts with the original data
  scaled_data_7 <- scaled_data_7 %>%
    left_join(sample_counts_7, by = "subtype")
  
  # Normalize the sum of scores by the number of samples in each subtype
  scaled_data_7 <- scaled_data_7 %>%
    mutate(`UroA Score` = sum_UroA / num_samples,
           `UroB Score` = sum_UroB / num_samples,
           `UroC Score` = sum_UroC / num_samples)

  #reshape the data for plotting
  long_data_5 <- scaled_data_5 %>%
    gather(key = "score_type", value = "score", `Uro Score`:`ScNE Score`)
  
  long_data_7 <- scaled_data_7 %>%
    gather(key = "score_type", value = "score", `UroA Score`:`UroC Score`)
  
  # Ensure that 'score_type' and 'subtype' are factors with the desired order
  long_data_5$score_type <- factor(long_data_5$score_type, levels = c("Uro Score", "GU Score", "BaSq Score", "Mes Score", "ScNE Score"))
  long_data_5$subtype <- factor(long_data_5$subtype, levels = c("ScNE", "Mes", "BaSq", "GU", "Uro"))
  long_data_7$score_type <- factor(long_data_7$score_type, levels = c("UroA Score", "UroB Score", "UroC Score" ))
  long_data_7$subtype <- factor(long_data_7$subtype, levels = c("UroC", "UroB", "UroA"))
  
  class_5_delta = plot_deltas(these_predictions = these_predictions,
                              this_subtype = NULL,
                              subtype_class = "5_class",
                              return_data = TRUE)
  
  # Sort the data by the delta column in ascending order
  class_5_delta <- class_5_data %>%
    arrange(delta)
  
  # Create a new column prediction_conf
  class_5_delta <- class_5_delta %>%
    mutate(prediction_conf = ifelse(row_number() <= 50, "low_conf", "high_conf"))
  
  class_7_delta = plot_deltas(these_predictions = these_predictions,
                              this_subtype = NULL,
                              subtype_class = "7_class",
                              return_data = TRUE)
  
  # Sort the data by the delta column in ascending order
  class_7_delta <- class_7_delta %>%
    arrange(delta)
  
  # Create a new column prediction_conf
  class_7_delta <- class_7_delta %>%
    mutate(prediction_conf = ifelse(row_number() <= 50, "low_conf", "high_conf"))
  
  
  library_qual_5 <- ggplot(class_5_delta, aes(x = reorder(sample_id, -delta), y = delta, group = prediction_conf, color = prediction_conf)) +
    geom_line() +
    geom_point(size = 1) +
    scale_color_manual(values = c("low_conf" = "#F44A4A", "high_conf" = "black")) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0.0", "0.25", "0.5", "0.75", "1.0")) +
    scale_x_discrete(expand = c(0, 0)) +
    labs(title = paste0(data_name, " 5 Class"),
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
  
  library_qual_7 <- ggplot(class_7_delta, aes(x = reorder(sample_id, -delta), y = delta, group = prediction_conf, color = prediction_conf)) +
    geom_line() +
    geom_point(size = 1) +
    scale_color_manual(values = c("low_conf" = "#F44A4A", "high_conf" = "black")) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0.0", "0.25", "0.5", "0.75", "1.0")) +
    scale_x_discrete(expand = c(0, 0)) +
    labs(title = paste0(data_name, " 7 Class"),
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
  
  #create the heatmap
  hm_5 = ggplot(long_data_5, aes(x = score_type, y = subtype, fill = score)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "#F44A4A") +
    labs(title = paste0(data_name, " 5 Class"),
         subtitle = "Scaled Prediction Scores",
         x = "Subtype Score",
         y = "Predicted Subtype") +
    theme_minimal() +
    scale_x_discrete(position = "top")
  
  hm_7 = ggplot(long_data_7, aes(x = score_type, y = subtype, fill = score)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "#F44A4A") +
    labs(title = paste0(data_name, " 7 Class"),
         subtitle = "Scaled Prediction Scores",
         x = "Subtype Score",
         y = "Predicted Subtype") +
    theme_minimal() +
    scale_x_discrete(position = "top")
  
  box_5 = ggplot(class_5_data, aes(x = subtype, y = delta, fill = subtype)) +
    geom_boxplot() +
    ylim(0, 1) +
    scale_fill_manual(values = lund_colors$lund_colors) +
    theme_bw() +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank()) +
    labs(title = paste0(data_name, " 5 Class"),
         subtitle = "Subtype Prediction Delta Values by Subtype",
         x = "Subtype",
         y = "Delta Value")
  
  box_7 = ggplot(class_7_data, aes(x = subtype, y = delta, fill = subtype)) +
    geom_boxplot() +
    ylim(0, 1) +
    scale_fill_manual(values = lund_colors$lund_colors) +
    theme_bw() +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank()) +
    labs(title = paste0(data_name, " 7 Class"),
         subtitle = "Subtype Prediction Delta Values by Subtype",
         x = "Subtype",
         y = "Delta Value")
  
  pdf(file = paste0(out_path, file_name, "_hm_5_class.pdf"), width = 9, height = 7)
  print(hm_5)
  dev.off()
  
  pdf(file = paste0(out_path, file_name, "_hm_7_class.pdf"), width = 9, height = 7)
  print(hm_7)
  dev.off()
  
  pdf(file = paste0(out_path, file_name, "_box_5_class.pdf"), width = 7, height = 7)
  print(box_5)
  dev.off()
  
  pdf(file = paste0(out_path, file_name, "_box_7_class.pdf"), width = 7, height = 7)
  print(box_7)
  dev.off()
  
  pdf(file = paste0(out_path, file_name, "_library_quality_class_5.pdf"), width = 9, height = 7)
  print(library_qual_5)
  dev.off()
  
  pdf(file = paste0(out_path, file_name, "_library_quality_class_7.pdf"), width = 9, height = 7)
  print(library_qual_7)
  dev.off()

  }

#run hepler
spit_plots(these_predictions = pred_tcga, out_path = "../Desktop/tmp/", data_name = "TCGA", file_name = "tcga")
spit_plots(these_predictions = pred_uc_genome, out_path = "../Desktop/tmp/", data_name = "UC Genome", file_name = "uc_genome")
spit_plots(these_predictions = pred_leeds, out_path = "../Desktop/tmp/", data_name = "Leeds", file_name = "leeds")


#TCGA analysis
class_5_delta = plot_deltas(these_predictions = pred_tcga,
                            this_subtype = NULL,
                            subtype_class = "5_class",
                            return_data = TRUE)

#sort the data by the delta column in ascending order
class_5_delta <- class_5_delta %>%
  arrange(delta)

#create a new column prediction_conf
class_5_delta <- class_5_delta %>%
  mutate(prediction_conf = ifelse(row_number() <= 50, "low_conf", "high_conf"))

class_5_delta = class_5_delta %>% 
  dplyr::select(sample_id, prediction_conf, delta)

joined = tcga_lib_size %>% 
  left_join(class_5_delta, by = "sample_id")

#plot library size
library_size <- ggplot(joined, aes(x = reorder(sample_id, -library_size), y = library_size, color = prediction_conf)) +
  geom_point(aes(shape = prediction_conf), size = 2) +
  scale_color_manual(values = c("low_conf" = "#F44A4A", "high_conf" = "grey")) +
  scale_shape_manual(values = c("low_conf" = 16, "high_conf" = 16)) + 
  labs(title = paste0("TCGA", " 5 Class"),
       x = "Samples",
       y = "Library Size") +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank())

print(library_size)

# Set the threshold for library size
threshold <- 45000

# Calculate the mean delta for samples above and below the threshold
mean_delta_above <- joined %>%
  filter(library_size > threshold) %>%
  summarise(mean_delta = mean(delta, na.rm = TRUE))

mean_delta_below <- joined %>%
  filter(library_size <= threshold) %>%
  summarise(mean_delta = mean(delta, na.rm = TRUE))

# Combine the results into a single data frame
mean_delta_df <- data.frame(
  threshold = c("Above Threshold", "Below Threshold"),
  mean_delta = c(mean_delta_above$mean_delta, mean_delta_below$mean_delta)
)

# Count the number of low_conf samples below and above the threshold
low_conf_counts <- joined %>%
  filter(prediction_conf == "low_conf") %>%
  mutate(threshold_group = ifelse(library_size <= threshold, "Below Threshold", "Above Threshold")) %>%
  group_by(threshold_group) %>%
  summarise(count = n())

# Create the bar plot
low_conf_plot <- ggplot(low_conf_counts, aes(x = threshold_group, y = count, fill = threshold_group)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Low Confidence Samples Below and Above Threshold",
       x = "Library Size Threshold",
       y = "Number of Low Confidence Samples") +
  scale_fill_manual(values = c("Below Threshold" = "#F44A4A", "Above Threshold" = "#4CAF50")) +
  theme_minimal()

# Create the plot
mean_delta_plot <- ggplot(mean_delta_df, aes(x = threshold, y = mean_delta, fill = threshold)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Delta Above and Below Library Size Threshold",
       x = "Library Size Threshold",
       y = "Mean Delta") +
  scale_fill_manual(values = c("Above Threshold" = "#4CAF50", "Below Threshold" = "#F44A4A")) +
  theme_minimal()

# Calculate the R-squared value
model <- lm(delta ~ library_size, data = joined)
r_squared <- summary(model)$r.squared

# Create the scatter plot with a trend line and R-squared value
scatter_plot <- ggplot(joined, aes(x = library_size, y = delta)) +
  geom_point(aes(color = prediction_conf), size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_text(aes(x = min(library_size), y = min(delta), 
                label = paste("R² =", round(r_squared, 3))), 
            hjust = 0, vjust = -1, color = "black") +
  scale_color_manual(values = c("low_conf" = "#F44A4A", "high_conf" = "black")) +
  labs(title = "Relationship Between Library Size and Delta",
       x = "Library Size",
       y = "Delta") +
  theme_minimal()

# Calculate the total number of samples below and above the threshold
total_counts <- joined %>%
  mutate(threshold_group = ifelse(library_size <= threshold, "Below Threshold", "Above Threshold")) %>%
  group_by(threshold_group) %>%
  summarise(total_count = n())

# Calculate the number of low_conf samples below and above the threshold
low_conf_counts <- joined %>%
  filter(prediction_conf == "low_conf") %>%
  mutate(threshold_group = ifelse(library_size <= threshold, "Below Threshold", "Above Threshold")) %>%
  group_by(threshold_group) %>%
  summarise(low_conf_count = n())

# Merge the total counts and low confidence counts
proportion_data <- merge(total_counts, low_conf_counts, by = "threshold_group")

# Calculate the proportion of low confidence samples
proportion_data <- proportion_data %>%
  mutate(proportion_low_conf = low_conf_count / total_count * 100)

# Create the bar plot
proportion_plot <- ggplot(proportion_data, aes(x = threshold_group, y = proportion_low_conf, fill = threshold_group)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportion of Low Confidence Samples Below and Above Threshold",
       x = "Library Size Threshold",
       y = "Proportion of Low Confidence Samples (%)") +
  scale_fill_manual(values = c("Below Threshold" = "#F44A4A", "Above Threshold" = "#4CAF50")) +
  theme_minimal()

print(library_size)
print(scatter_plot)
print(low_conf_plot)
print(mean_delta_plot)
print(proportion_plot)
