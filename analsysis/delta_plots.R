calculate_delta <- function(row) {
  sorted_scores <- sort(row, decreasing = TRUE)
  highest <- sorted_scores[1]
  second_highest <- sorted_scores[2]
  delta <- highest - second_highest
  return(delta)
}

predictions = as.data.frame(pred_tcga$predictions_5classes) %>% 
  rownames_to_column("sample_id") %>% 
  rename(subtype = `pred_tcga$predictions_5classes`)

prediction_scores = as.data.frame(pred_tcga$subtype_scores) %>% 
  rownames_to_column("sample_id") %>% 
  select(sample_id, Uro, GU, BaSq, Mes, ScNE)

prediction_scores$delta <- apply(prediction_scores[, -1], 1, calculate_delta)

prediction_scores$ranked_delta <- rank(prediction_scores$delta, ties.method = "min")

# Create the plot
ggplot(prediction_scores, aes(x = reorder(sample_id, delta), y = delta)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Ranked Delta Values",
       x = "Sample ID",
       y = "Ranked Delta")
