## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(DisaggregateTS)

## -----------------------------------------------------------------------------
# Load the combined data from the package
data(combined_data)

# Extract Data_Y and Data_X from the combined data
Data_Y <- combined_data$Data_Y
Data_X <- combined_data$Data_X

# Select IBM GHG data and dates for Q3 2005 - Q3 2021
Y <- Data_Y$IBM[7:23]
Dates <- as.Date(Data_Y$Dates[7:23])

# Filter high-frequency data (Q3 2005 - Q3 2021)
X <- Data_X[24:91, ]

# Convert all columns to numeric, remove columns with NA values
X <- as.data.frame(lapply(X, as.numeric))
Y <- as.numeric(Y)
X <- X[, colSums(is.na(X)) == 0]

# Remove highly correlated variables (pairwise correlation >= 0.99)
corr_matrix <- cor(X, use = "complete.obs")
corr_matrix[upper.tri(corr_matrix)] <- 0
diag(corr_matrix) <- 0

X_filtered <- X[, !apply(corr_matrix, 2, function(x) any(abs(x) >= 0.99))]

## -----------------------------------------------------------------------------
result <- disaggregate(
  Y = as.matrix(Y),
  X = as.matrix(X_filtered),
  aggMat = "first",
  aggRatio = 4,
  method = "adaptive-spTD"
)

# High-frequency estimates and coefficients
Y_HF <- result$y_Est
beta_Est <- result$beta_Est
rho_Est <- result$rho_Est

# Display estimated rho
print(paste("Estimated rho:", rho_Est))

## ----plot-results, fig.width=8, fig.height=5, echo=FALSE----------------------
# Ensure Dates_Q is in Date format
Dates_Q <- as.Date(Data_X$Dates[24:91])

# Plot the disaggregated and interpolated results
plot(Dates_Q, Y_HF, type = "l", ylab = "GHG Emissions", xlab = "Time", lwd = 2, col = "black")

# Add points on top of the line
points(Dates_Q, Y_HF, col = "black", pch = 16)

points(Dates, Y, col = "red", pch = 16)

# Add legend
legend("topright", legend = c("Observed Annual", "Disaggregated"), 
       col = c("red", "black"), lty = c(NA, 1), pch = c(16, NA))

