# Load necessary libraries
library(ggplot2)

# Define the bribe and grade data as vectors
bribe <- c(10, 25, 75, 150, 200)
grade <- c(73, 79, 84, 96, 95)

# Perform linear regression
linear_model <- lm(grade ~ bribe)

# Summary of the linear model
summary_model <- summary(linear_model)

# Extract the intercept and slope
intercept <- coef(linear_model)[1]
slope <- coef(linear_model)[2]

# Extract the R-squared value
r_squared <- summary_model$r.squared

# Create the regression formula text
regression_formula <- paste0("y = ", round(intercept, 2), " + ", round(slope, 2), "x")

# Create the R-squared text
r_squared_text <- paste0("R² = ", round(r_squared, 2))

# Create the plot
plot <- ggplot(data.frame(bribe, grade), aes(x = bribe, y = grade)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  ggtitle("Simple Linear Regression") +
  xlab("Bribe (x)") +
  ylab("Grade (y)") +
  annotate("text", x = max(bribe), y = min(grade), label = regression_formula, hjust = 1, vjust = -1, size = 5, color = "blue") +
  annotate("text", x = max(bribe), y = min(grade) + 5, label = r_squared_text, hjust = 1, vjust = -1, size = 5, color = "blue")

# Print the plot
print(plot)