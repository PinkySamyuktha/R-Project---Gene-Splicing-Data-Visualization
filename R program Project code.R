#Task-1
#loading the dataset
splicing_df <- read.csv("C:/Users/madee/Downloads/splicing_data.csv", header=TRUE)
#summary statistics
summary(splicing_df)
#printing the datapoints
head(splicing_df)
#as the first column is not important we can delete it
splicing_df <- splicing_df[, -1]
#shape of the dataset
dim(splicing_df)
#checking for missing values
# Count missing values in each column
missing_values <- colSums(is.na(splicing_df))
print(missing_values)
# Function to detect outliers using Z-score
detect_outliers <- function(x, threshold = 3) {
  z <- abs((x - mean(x)) / sd(x))
  return(x[z > threshold])
}
# Apply the function to each variable
outliers <- lapply(splicing_df, detect_outliers)
#print outliers
print(outliers)
#Box plot for seeing the outliers
boxplot(splicing_df)
#sepearating the dataset into x and y(y is the target variable)
x <- splicing_df[, -ncol(splicing_df)]

#target variable
y <- splicing_df[, ncol(splicing_df)]
#correlation matrix
data_combined <- cbind(x,y)

#calculate the correlation
correlation_matrix <- cor(data_combined)
#print correlation matrix
print(correlation_matrix)

#Task-2
# Load the ggplot2 package
library(ggplot2)

# Plot histograms for numerical variables
ggplot(splicing_df, aes(x=SplicingFactor1)) + geom_histogram(fill="skyblue", color="black") + labs(title="SplicingFactor1")
ggplot(splicing_df, aes(x=SplicingFactor2)) + geom_histogram(fill="skyblue", color="black") + labs(title="SplicingFactor2")
ggplot(splicing_df, aes(x=SplicingFactor3)) + geom_histogram(fill="skyblue", color="black") + labs(title="SplicingFactor3")
ggplot(splicing_df, aes(x=y)) + geom_histogram(fill="skyblue", color="black") + labs(title="splicing events")

# Plot scatter plots for numerical features against the target variable
ggplot(splicing_df, aes(x=SplicingFactor1, y=y)) + geom_point() + labs(title="SplicingFactor1 vs. Target Variable (y)", x="SplicingFactor1", y="Target Variable (y)")
ggplot(splicing_df, aes(x=SplicingFactor2, y=y)) + geom_point() + labs(title="SplicingFactor2 vs. Target Variable (y)", x="SplicingFactor2", y="Target Variable (y)")
ggplot(splicing_df, aes(x=SplicingFactor3, y=y)) + geom_point() + labs(title="SplicingFactor3 vs. Target Variable (y)", x="SplicingFactor3", y="Target Variable (y)")

#Task-3
# Set the seed for reproducibility
set.seed(123)

# Generate random indices for the training set
train_indices <- sample(1 : nrow(splicing_df), 0.7 * nrow(splicing_df))

# Create the training and testing sets
training_data <- splicing_df[train_indices, ]
testing_data <- splicing_df[-train_indices, ]

# Assuming 'training_data' and 'testing_data' are the names of your training and testing datasets
# Split the training dataset into X_train (predictor variables) and y_train (target variable)
X_train <- training_data[, -ncol(training_data)]  # Exclude the last column (target variable)
y_train <- training_data[, ncol(training_data)]   # Select only the last column (target variable)

# Split the testing dataset into X_test (predictor variables) and y_test (target variable)
X_test <- testing_data[, -ncol(testing_data)]     # Exclude the last column (target variable)
y_test <- testing_data[, ncol(testing_data)]      # Select only the last column (target variable)

# Train a linear regression model
lm_model <- lm(y_train ~ ., data = X_train)

# Make predictions on the training data
predictions <- predict(lm_model, newdata = X_train)

# Evaluate the model on the training data
mse <- mean((predictions - y_train)^2)
rmse <- sqrt(mse)

# Print the RMSE
print(paste("Root Mean Squared Error (RMSE) on training data:", rmse))

# Make predictions on the testing data
predictions <- predict(lm_model, newdata = X_test)

# Evaluate the model on the testing data
mse <- mean((predictions - y_test)^2)
rmse <- sqrt(mse)

# Print the RMSE
print(paste("Root Mean Squared Error (RMSE) on testing data:", rmse))

# Calculate R-squared (coefficient of determination)
SST <- sum((y_test - mean(y_test))^2)
SSE <- sum((y_test - predictions)^2)
rsquared <- 1 - SSE/SST

# Print RMSE and R-squared
print(paste("Root Mean Squared Error (RMSE) on testing data:", rmse))
print(paste("R-squared on testing data:", rsquared))

# Extract the coefficients from the linear regression model
coefficients <- coef(lm_model)

# Remove the intercept term (if present)
coefficients <- coefficients[-1]

# Create a bar plot of the coefficients
barplot(coefficients, main = "Weights of Features", xlab = "Features", ylab = "Weights")

# Extract the feature names
feature_names <- names(coefficients)

# Print the feature names and their corresponding weights
cat("Feature Name\t\tWeight\n")
cat("--------------------\t------\n")
for (i in seq_along(feature_names)) {
  cat(feature_names[i], "\t\t", coefficients[i], "\n")
}

