#load packages
library(tibble)
library(ggplot2)
library(dplyr)
library(cowplot)
devtools::load_all(path = "LundTaxonomy2023Classifier/")

#load data
load("lundtax_project/out/manuscript_figs/plot_data/tcga_joined.Rdata")
load("lundtax_project/out/manuscript_figs/plot_data/tcga_5.Rdata")
load("lundtax_project/out/manuscript_figs/plot_data/tcga_7.Rdata")
load("lundtax_project/out/manuscript_figs/plot_data/uc_5.Rdata")
load("lundtax_project/out/manuscript_figs/plot_data/uc_7.Rdata")
load("lundtax_project/out/manuscript_figs/plot_data/leeds_5.Rdata")
load("lundtax_project/out/manuscript_figs/plot_data/leeds_7.Rdata")

### Delta box plots
## TCGA
tcga_box = ggplot(tcga_box_data, aes(x = subtype, y = delta, fill = subtype)) +
        geom_boxplot(width = 0.5) +
        scale_fill_manual(values = lund_colors$lund_colors) +
        scale_y_continuous(limits = c(0, 1.2), breaks = seq(0, 1, by = 0.2), expand = c(0, 0)) +
        theme_bw() +
        theme(panel.background = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              plot.background = element_blank(),
              axis.ticks.x = element_blank(), 
              axis.line.x = element_blank(), 
              axis.text.x = element_blank(),
              plot.tag = element_text(face = "bold"),
              legend.position = "none",
              plot.title = element_text(hjust = 0, size = 12),
              panel.grid.minor.y = element_blank()) +
        labs(title = "TCGA",
             tag = "A",
             x = "",
             y = "Delta") +
  geom_vline(xintercept = 5.5, linetype = "dashed") +
  annotate("text", x = 3, y = 1.1, label = "5 Class", size = 3, hjust = 0.5) +
  annotate("text", x = 7, y = 1.1, label = "7 Class", size = 3, hjust = 0.5)


print(tcga_box)

## UC Genome
uc_box = ggplot(uc_box_data, aes(x = subtype, y = delta, fill = subtype)) +
  geom_boxplot(width = 0.5) +
  scale_fill_manual(values = lund_colors$lund_colors) +
  scale_y_continuous(limits = c(0, 1.2), breaks = seq(0, 1, by = 0.2), expand = c(0, 0)) +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.background = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.line.x = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.tag = element_text(face = "bold"),
        legend.position = "none",
        plot.title = element_text(hjust = 0, size = 12),
        panel.grid.minor.y = element_blank()) +
  labs(title = "UC Genome",
       tag = "B",
       x = "",
       y = "") +
  geom_vline(xintercept = 5.5, linetype = "dashed") +
  annotate("text", x = 3, y = 1.1, label = "5 Class", size = 3, hjust = 0.5) +
  annotate("text", x = 7, y = 1.1, label = "7 Class", size = 3, hjust = 0.5)

print(uc_box)

## Leeds
leeds_box = ggplot(leeds_box_data, aes(x = subtype, y = delta, fill = subtype)) +
  geom_boxplot(width = 0.5) +
  scale_fill_manual(values = lund_colors$lund_colors) +
  scale_y_continuous(limits = c(0, 1.2), breaks = seq(0, 1, by = 0.2), expand = c(0, 0)) +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.background = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.line.x = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.tag = element_text(face = "bold"),
        legend.position = "none",
        plot.title = element_text(hjust = 0, size = 12),
        panel.grid.minor.y = element_blank()) +
  labs(title = "Leeds",
       tag = "C",
       x = "",
       y = "") +
  geom_vline(xintercept = 2.5, linetype = "dashed") +
  annotate("text", x = 1.5, y = 1.1, label = "5 Class", size = 3, hjust = 0.5) +
  annotate("text", x = 3.5, y = 1.1, label = "7 Class", size = 3, hjust = 0.5)

print(leeds_box)

## Delta Sizes
delta_size <- ggplot(tcga_joined, aes(x = reorder(sample_id, -delta), y = delta, group = 1)) +
  geom_line(color = "black") +
  geom_point(data = subset(tcga_joined, prediction_conf == "low_conf"), aes(fill = prediction_conf), color = "black", shape = 21, size = 2) +
  scale_fill_manual(values = c("low_conf" = "#F44A4A"), labels = c("Delta < 0.2306")) +
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
  scale_y_continuous(breaks = seq(0, max(tcga_joined$delta, na.rm = TRUE), by = 0.2))

print(delta_size)

## Library Size plot with low conf samples
library_size <- ggplot(tcga_joined, aes(x = reorder(sample_id, -library_size), y = library_size, group = 1)) +
  geom_line(color = "black") +
  geom_point(data = subset(tcga_joined, prediction_conf == "low_conf"), aes(fill = prediction_conf), color = "black", shape = 21, size = 2) +
  scale_fill_manual(values = c("low_conf" = "#F44A4A"), labels = c("Delta < 0.2306")) +
  labs(title = "Library Size With Ambigious Prediction Calls (TCGA)",
       tag = "E",
       x = "",
       y = "Library Size",
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
  coord_cartesian(ylim = c(40000, NA)) +
  scale_y_continuous(breaks = seq(40000, max(tcga_joined$library_size, na.rm = TRUE), by = 2000))

print(library_size)

# Define the plots
plots <- list(tcga_box, uc_box, leeds_box, delta_size, library_size)

# Arrange the plots into the desired layout
combined_plot_1 <- plot_grid(
  plot_grid(plots[[1]], plots[[2]], ncol = 2, rel_widths = c(1, 1)),  # First row
  plot_grid(plots[[3]], plots[[4]], plots[[5]], ncol = 3, rel_widths = c(0.5, 0.5, 1.2)),
  nrow = 2
)

print(combined_plot_1)

combined_plot_2 <- plot_grid(
  plot_grid(plots[[1]], plots[[2]], plots[[3]], ncol = 3, rel_widths = c(1, 1, 0.6)),  # First row
  plot_grid(plots[[4]], plots[[5]], ncol = 2, rel_widths = c(0.7, 1.25)),
  nrow = 2
)

pdf(file = "lundtax_project/out/manuscript_figs/out/fig_1.pdf", width = 15, height = 7)
print(combined_plot_2)
dev.off()


