#library size plot
library_size <- ggplot(joined, aes(x = reorder(sample_id, -library_size), y = library_size, group = 1)) +
  geom_line(color = "black") +
  geom_point(data = subset(joined, prediction_conf == "low_conf"), aes(fill = prediction_conf), color = "black", shape = 21, size = 2) +
  scale_fill_manual(values = c("low_conf" = "#F44A4A"), labels = c("Delta < 0.2306")) +
  labs(title = "Library Size Per Sample (TCGA)",
       tag = "A",
       subtitle = "Confounding Prediction-calls In Red (\u0394 \u2264 0.2306)",
       x = "Samples",
       y = "Library Size",
       fill = "") +
  theme_bw() +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank(),
        plot.tag = element_text(face = "bold"),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        legend.position = "none", 
        panel.grid.minor.y = element_blank()) +
  coord_cartesian(ylim = c(40000, NA)) +
  scale_y_continuous(breaks = seq(40000, max(joined$library_size, na.rm = TRUE), by = 1000))

delta_size <- ggplot(joined, aes(x = reorder(sample_id, -delta), y = delta, group = 1)) +
  geom_line(color = "black") +
  geom_point(data = subset(joined, prediction_conf == "low_conf"), aes(fill = prediction_conf), color = "black", shape = 21, size = 2) +
  scale_fill_manual(values = c("low_conf" = "#F44A4A"), labels = c("Delta < 0.2306")) +
  labs(title = "Library Size Per Sample (TCGA)",
       tag = "A",
       subtitle = "Confounding Prediction-calls In Red (\u0394 \u2264 0.2306)",
       x = "Samples",
       y = "Library Size",
       fill = "") +
  theme_bw() +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank(),
        plot.tag = element_text(face = "bold"),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        legend.position = "none", 
        panel.grid.minor.y = element_blank()) +
  coord_cartesian(ylim = c(0, NA)) +
  scale_y_continuous(breaks = seq(0, max(joined$delta, na.rm = TRUE), by = 0.2))


save(file = "../BIOINFORMATICS/lundtax_project/out/figures/new_plots/fix/plot_data/tcga_libsize_low_delta.Rdata", joined)

pdf(file = "../BIOINFORMATICS/lundtax_project/out/figures/new_plots/fix/tcga_library_size_5_class.pdf", width = 9, height = 7)
print(library_size)
dev.off()

#scatter plot
scatter_plot <- ggplot(joined, aes(x = library_size, y = delta)) +
  geom_point(aes(color = prediction_conf), size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_text(aes(x = min(library_size), y = min(delta), 
                label = paste("R² =", round(r_squared, 3))), 
            hjust = 0, vjust = -1, color = "black") +
  scale_color_manual(values = c("low_conf" = "#F44A4A", "high_conf" = "black"), labels = c("High", "Low")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  labs(title = "Library Size vs. Delta (TCGA)",
       tag = "B",
       subtitle = "Low-confidence predictions defined as:\n\u0394 \u2264 0.2306",
       x = "Library Size",
       y = "Delta Prediction Score",
       color = "Prediction\nConfidence") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.tag = element_text(face = "bold"))

save(file = "../BIOINFORMATICS/lundtax_project/out/figures/new_plots/fix/plot_data/tcga_scatter_delta.Rdata", joined)

pdf(file = "../BIOINFORMATICS/lundtax_project/out/figures/new_plots/fix/tcga_scatter_delta.pdf", width = 7, height = 9)
print(scatter_plot)
dev.off()

#mean delta plot
mean_delta_plot <- ggplot(mean_delta_df, aes(x = threshold, y = mean_delta, fill = threshold)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(mean_delta, 2)), vjust = -0.5, color = "black") +
  labs(title = "Library Size vs. Delta (TCGA)",
       subtitle = "Mean Delta Above and Below Library\nSize Threshold (45k reads)",
       x = "",
       tag = "C",
       y = "Mean Delta",
       fill = "Library Size\nThreshold") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_fill_manual(values = c("Above Threshold" = "#4CAF50", "Below Threshold" = "#F44A4A"), labels = c("Above", "Below")) +
  theme_bw() +
  theme(plot.tag = element_text(face = "bold"),
        panel.grid = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

save(file = "../BIOINFORMATICS/lundtax_project/out/figures/new_plots/fix/plot_data/tcga_mean_delta_thresh.Rdata", mean_delta_df)

pdf(file = "../BIOINFORMATICS/lundtax_project/out/figures/tcga/tcga_mean_delta_thresh.pdf", width = 7, height = 9)
print(mean_delta_plot)
dev.off()

#create proportions plot
proportion_plot <- ggplot(proportion_data, aes(x = threshold_group, y = proportion_low_conf, fill = threshold_group)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(proportion_low_conf, 2)), vjust = -0.5, color = "black") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 30)) +
  labs(title = "Library Size vs. Delta (TCGA)",
       subtitle = "Proportion of Low Confidence Samples\nBelow and Above Threshold",
       tag = "C",
       x = "",
       y = "Proportion of Low Confidence Samples (%)",
       fill = "Library Size\nThreshold (45k reads)") +
  scale_fill_manual(values = c("Below Threshold" = "#F44A4A", "Above Threshold" = "#4CAF50"), labels = c("Above", "Below")) +
  theme_bw() +
  theme(plot.tag = element_text(face = "bold"),
        panel.grid = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

save(file = "../BIOINFORMATICS/lundtax_project/out/data/plot_data/tcga_prop_con.Rdata", proportion_data)

pdf(file = "../BIOINFORMATICS/lundtax_project/out/figures/tcga/tcga_prop_conf.pdf", width = 7, height = 9)
print(proportion_plot)
dev.off()
