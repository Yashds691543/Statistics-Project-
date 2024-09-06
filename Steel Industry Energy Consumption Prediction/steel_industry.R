
library(caret)

# Read the CSV file into a data frame
df <- read.csv("Steel_industry_data.csv", header = TRUE)

# Split the data into training and test sets
X <- subset(df, select = -Usage_kWh)
y <- df$Usage_kWh
set.seed(42)
split <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- X[split, ]
X_test <- X[-split, ]
y_train <- y[split]
y_test <- y[-split]

# Fit a linear regression model on the training set
model <- lm(y_train ~ ., data = X_train)

# Make predictions on the test set
y_pred <- predict(model, newdata = X_test)

# Evaluate the model
rmse <- sqrt(mean((y_pred - y_test)^2))
cat("RMSE:", rmse, "\n")

library(caret)

# Read the CSV file into a data frame
df <- read.csv("Steel_industry_data.csv", header = TRUE)

# Split the data into training and test sets
X <- subset(df, select = -Usage_kWh)
y <- df$Usage_kWh
set.seed(42)
split <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- X[split, ]
X_test <- X[-split, ]
y_train <- y[split]
y_test <- y[-split]

# Fit a boosting model with gradient boosting machines (gbm)
library(gbm)
model_boost <- gbm(y_train ~ ., data = X_train, n.trees = 100, shrinkage = 0.01, interaction.depth = 3)

# Make predictions on the test set
y_pred_boost <- predict(model_boost, newdata = X_test, n.trees = 100)

# Evaluate the model
rmse_boost <- sqrt(mean((y_pred_boost - y_test)^2))
cat("RMSE (boosting):", rmse_boost, "\n")


library(caret)

# Read the CSV file into a data frame
df <- read.csv("Steel_industry_data.csv", header = TRUE)

# Split the data into training and test sets
X <- subset(df, select = -Usage_kWh)
y <- df$Usage_kWh
set.seed(42)
split <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- X[split, ]
X_test <- X[-split, ]
y_train <- y[split]
y_test <- y[-split]

# Fit a random forest model on the training set
library(randomForest)
model <- randomForest(y_train ~ ., data = X_train)

# Make predictions on the test set
y_pred <- predict(model, newdata = X_test)

# Evaluate the model
rmse <- sqrt(mean((y_pred - y_test)^2))
cat("RMSE:", rmse, "\n")
