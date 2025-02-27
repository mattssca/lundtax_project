library(reshape2)
library(tidyr)
library(GGally)
library(fmsb)

plot_data = plot_deltas(these_predictions = pred_tcga, 
                        subtype_class = "5_class", 
                        this_subtype = NULL,
                        return_data = TRUE)

desired_order <- c("Uro", "GU", "BaSq", "Mes", "ScNE")

plot_data$subtype <- factor(plot_data$subtype, levels = desired_order)

heatmap_data <- melt(plot_data, id.vars = c("sample_id", "subtype"), measure.vars = "delta")

ggplot(plot_data, aes(x = subtype, y = delta, fill = subtype)) +
  geom_boxplot() +
  scale_fill_manual(values = lund_colors$lund_colors) +
  theme_bw() +
  labs(title = "Distribution of Delta Values by Subtype (TCGA)",
       x = "Subtype",
       y = "Delta")

ggplot(plot_data, aes(x = delta, fill = subtype)) +
  geom_density(alpha = 0.8) +
  scale_fill_manual(values = lund_colors$lund_colors) +
  theme_bw() +
  labs(title = "Density Plot of Delta Values by Subtype",
       x = "Delta",
       y = "Density")

ggplot(plot_data, aes(x = sample_id, y = delta, color = subtype)) +
  geom_point() +
  scale_color_manual(values = lund_colors$lund_colors) +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(title = "Scatter Plot of Delta Values by Sample",
       x = "Sample ID",
       y = "Delta")

ggplot(plot_data, aes(x = subtype, y = delta, fill = subtype)) +
  geom_violin(trim = FALSE, scale = "width") +
  scale_fill_manual(values = lund_colors$lund_colors) +
  theme_bw() +
  labs(title = "Violin Plot of Delta Values by Subtype",
       x = "Subtype",
       y = "Delta")

ggplot(plot_data, aes(x = subtype, y = delta, fill = subtype)) +
  stat_summary(fun = mean, geom = "bar") +
  scale_fill_manual(values = lund_colors$lund_colors) +
  theme_bw() +
  labs(title = "Mean Delta Values by Subtype",
       x = "Subtype",
       y = "Mean Delta")

ggplot(plot_data, aes(x = subtype, y = delta, color = subtype)) +
  geom_jitter(width = 0.2) +
  scale_color_manual(values = lund_colors$lund_colors) +
  theme_bw() +
  labs(title = "Jitter Plot of Delta Values by Subtype",
       x = "Subtype",
       y = "Delta")

heatmap_data <- melt(plot_data, id.vars = c("sample_id", "subtype"), measure.vars = "delta")

ggplot(heatmap_data, aes(x = sample_id, y = subtype, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(title = "Heatmap of Delta Values",
       x = "Sample ID",
       y = "Subtype",
       fill = "Delta")

ggplot(plot_data, aes(x = subtype, y = delta, color = subtype)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.5) +
  scale_color_manual(values = lund_colors$lund_colors) +
  theme_bw() +
  labs(title = "Dot Plot of Delta Values by Subtype",
       x = "Subtype",
       y = "Delta")

# Reshape the data for stacked bar plot
stacked_data <- plot_data %>%
  gather(key = "subtype_score", value = "score", -sample_id, -delta, -subtype) %>% 
  filter(subtype == "Uro")

ggplot(stacked_data, aes(x = sample_id, y = score, fill = subtype_score)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = lund_colors$lund_colors) +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(title = "Stacked Bar Plot of Subtype Scores",
       x = "Sample ID",
       y = "Score",
       fill = "Subtype")

# Reshape the data for parallel coordinates plot
parallel_data <- plot_data %>%
  select(sample_id, subtype, Uro, GU, BaSq, Mes, ScNE) %>%
  gather(key = "subtype_score", value = "score", -sample_id, -subtype) %>%
  spread(key = "subtype_score", value = "score")

# Ensure the columns are correctly specified
ggparcoord(parallel_data, columns = 3:7, groupColumn = 2, scale = "globalminmax") +
  theme_bw() +
  
  labs(title = "Parallel Coordinates Plot of Subtype Scores",
       x = "Subtype",
       y = "Score")

# Reshape the data for heatmap
heatmap_data <- plot_data %>%
  select(-delta) %>%
  gather(key = "subtype_score", value = "score", -sample_id, -subtype)

ggplot(heatmap_data, aes(x = sample_id, y = subtype_score, fill = score)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(title = "Heatmap of Subtype Scores",
       x = "Sample ID",
       y = "Subtype",
       fill = "Score")

# Reshape the data for facet grid
facet_data <- plot_data %>%
  select(-delta) %>%
  gather(key = "subtype_score", value = "score", -sample_id, -subtype)

ggplot(facet_data, aes(x = sample_id, y = score)) +
  geom_point(aes(color = subtype)) +
  scale_color_manual(values = lund_colors$lund_colors) +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  facet_wrap(~ subtype_score, scales = "free_x") +
  labs(title = "Facet Grid of Subtype Scores",
       x = "Sample ID",
       y = "Score")
