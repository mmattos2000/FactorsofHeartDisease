# Load required packages
install.packages(c("readxl", "ggplot2"))
library(readxl)
library(ggplot2)

# Load data
heartdisease <- data.frame(readxl::read_excel("Heart Disease Data Cleaned.xlsx", col_names = TRUE))
heartdisease$Heart.Disease <- (heartdisease$Heart.Disease * 1)  # Convert to binary (0 or 1)

# Function to create logistic regression plot
logistic_regression_plot <- function(x_var, x_label) {
  # Fit logistic regression model
  model <- glm(Heart.Disease ~ ., data = heartdisease[, c(x_var, "Heart.Disease")], family = binomial)
  
  # Plot data points and logistic regression line
  ggplot(heartdisease, aes_string(x = x_var, y = "Heart.Disease")) +
    geom_point(alpha = 0.5) +
    stat_smooth(method = "glm", formula = y ~ x, se = FALSE,
                method.args = list(family = binomial), col = 'blue') +
    labs(x = x_label, y = "Heart Disease") +
    ggtitle(paste("Logistic Regression Plot for", x_label)) +
    theme_minimal()
}

# Create logistic regression plots for additional factors
plot_list <- list(
  logistic_regression_plot("BMI", "BMI"),
  logistic_regression_plot("Sleeptime", "Sleep Time"),
  logistic_regression_plot("Cholesterol", "Cholesterol Levels"),
  # Add more factors here as needed
)

# Display all logistic regression plots
gridExtra::grid.arrange(grobs = plot_list, ncol = 2)
