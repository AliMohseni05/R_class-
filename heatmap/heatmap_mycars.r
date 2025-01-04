install.packages("reshape2")
# Load necessary libraries
library(ggplot2)
library(reshape2)

# Load the mtcars dataset
data(mtcars)

# Melt the mtcars dataset to long format
mtcars_melted <- melt(mtcars)

# Create a heatmap using ggplot2
ggplot(data = mtcars_melted, aes(x = variable, y = rownames(mtcars), fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of mtcars Dataset", x = "Variables", y = "Cars") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

