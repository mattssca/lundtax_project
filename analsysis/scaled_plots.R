library(circlize)

  plot_data = plot_deltas(these_predictions = pred_tcga, 
                          subtype_class = "5_class", 
                          this_subtype = NULL,
                          return_data = TRUE)

summarized_data <- plot_data %>%
  group_by(subtype) %>%
  summarize(sum_Uro = sum(Uro),
            sum_GU = sum(GU),
            sum_BaSq = sum(BaSq),
            sum_Mes = sum(Mes),
            sum_ScNE = sum(ScNE))

scaled_data <- summarized_data %>%
  mutate(scaled_Uro = sum_Uro / max(sum_Uro),
         scaled_GU = sum_GU / max(sum_GU),
         scaled_BaSq = sum_BaSq / max(sum_BaSq),
         scaled_Mes = sum_Mes / max(sum_Mes),
         scaled_ScNE = sum_ScNE / max(sum_ScNE))

#reshape the data for plotting
long_data <- scaled_data %>%
  gather(key = "score_type", value = "score", scaled_Uro:scaled_ScNE)

#create the bar plot
ggplot(long_data, aes(x = score_type, y = score, fill = subtype)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = lund_colors$lund_colors) +
  labs(title = "Scaled Scores for Each Subtype",
       x = "Score Type",
       y = "Scaled Score") +
  theme_minimal()

#create the heatmap
ggplot(long_data, aes(x = score_type, y = subtype, fill = score)) +
  geom_tile() +
  scale_fill_gradient(low = "#8CDAB5", high = "#5C3933") +
  labs(title = "Heatmap of Scaled Scores for Each Subtype",
       x = "Score Type",
       y = "Subtype") +
  theme_minimal()

#create the dot plot
ggplot(long_data, aes(x = score_type, y = score, color = subtype)) +
  geom_point(size = 3) +
  scale_color_manual(values = lund_colors$lund_colors) +
  labs(title = "Dot Plot of Scaled Scores for Each Subtype",
       x = "Score Type",
       y = "Scaled Score") +
  theme_minimal()

# Rename the columns in the scaled_data data frame
scaled_data <- scaled_data %>%
  rename(Uro_score = scaled_Uro,
         GU_score = scaled_GU,
         BaSq_score = scaled_BaSq,
         Mes_score = scaled_Mes,
         ScNE_score = scaled_ScNE)

# Define a custom color palette for subtypes
my_palette <- c("Uro_score" = "#3cb44b65", "GU_score" = "#4363d865", "BaSq_score" = "#CD262665", "Mes_score" = "#f5823165", "ScNE_score" = "#A020F065", 
                "Uro" = "#3cb44b", "GU" = "#4363d8", "BaSq" = "#CD2626", "Mes" = "#f58231", "ScNE" = "#A020F0")

# Prepare data for chord diagram
chord_data <- scaled_data %>%
  gather(key = "score_type", value = "score", Uro_score:ScNE_score) %>%
  select(subtype, score_type, score)

# Create a color vector for the chord diagram
grid.col <- my_palette

# Create the chord diagram with custom colors
chordDiagram(chord_data, transparency = 0.5, annotationTrack = "grid", preAllocateTracks = list(track.height = 0.1), grid.col = grid.col)

# Add labels to the sectors
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  sector.name = get.cell.meta.data("sector.index")
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  circos.text(mean(xlim), ylim[1] + 0.1, sector.name, facing = "inside", niceFacing = TRUE, adj = c(0.5, 0.5))
}, bg.border = NA)

title("Chord Diagram of Scaled Scores for Each Subtype")

