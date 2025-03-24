library(ggplot2)

## Delta Sizes
delta_size <- ggplot(tcga_joined, aes(x = reorder(sample_id, -delta), y = delta, group = 1)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 0.2305), fill = "#F44A4A") +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.2307, ymax = Inf), fill = "#3B48D9") +
  geom_line(color = "white", linewidth = 0.8) + 
  geom_hline(yintercept = 0.2306, colour = "white", linetype = 2, linewidth = 0.8) +
  annotate("text", x = 50, y = 0.1, label = "Low Confident Prediction Calls", color = "white", size = 4, hjust = 0) +
  annotate("text", x = 50, y = 0.6, label = "High Confident Prediction Calls", color = "white", size = 4, hjust = 0) +
  labs(title = "Prediction-calls Delta Distribution (TCGA)",
       tag = "D",
       x = "",
       y = "Delta",
       fill = "") +
  theme_bw() +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank(),
        plot.tag = element_text(face = "bold"),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(hjust = 0, size = 12),
        legend.position = "none", 
        panel.grid.minor.y = element_blank()) +
  coord_cartesian(ylim = c(0, NA)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0))

print(delta_size)


# Create a plot
ggplot(tcga_joined, aes(x = library_size, fill = prediction_conf)) +
  geom_histogram(binwidth = 100, color = "black") +
  scale_fill_manual(values = c("low_conf" = "#F44A4A", "high_conf" = "#3B48D9"),  labels = c("high_conf" = "High", "low_conf" = "Low")) +
  labs(title = "Library Size Distribution (bin = 100)",
       x = "Library Size",
       y = "Frequency",
       fill = "Prediction Confidence") +
  theme_bw() +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank(), 
        legend.position = c(0.8, 0.65),
        legend.background = element_blank(),
        plot.tag = element_text(face = "bold"),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(hjust = 0, size = 12),
        panel.grid.minor.y = element_blank()) +
  coord_cartesian(ylim = c(0, NA)) +
  scale_y_continuous(breaks = seq(0, 500, by = 5), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0))
