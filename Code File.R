install.packages("readr")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("janitor")
install.packages("caret")
install.packages("randomForest")
install.packages("knitr")
install.packages("Metrics")
library(readr)
library(tidyverse)
library(janitor)
library(knitr)
library(Metrics)
#read in data
mlb_data <- read.csv("baseball_hitting.csv") %>% 
  clean_names()

# Explore the dataset
head(mlb_data)
summary(mlb_data)

#some feature engineering 
library(dplyr)
colnames
#converting strikeouts to integers
mlb_data$strikeouts <- as.integer(mlb_data$strikeouts)
str(mlb_data)

#creating new features
mlb_features <- mlb_data %>%
  filter(at_bat > 0) %>%
  mutate(ISO = slugging_percentage - ((hits/ at_bat)),
         BABIP = (hits - home_run) / (at_bat - strikeouts - home_run)) %>%
select(player_name, position, on_base_percentage, ISO, BABIP, slugging_percentage, runs, run_batted_in, strikeouts, a_walk, home_run, on_base_plus_slugging)

# Remove rows with NA values and converting column names
mlb_features <- mlb_features[complete.cases(mlb_features), ]
colnames(mlb_features)[c (3,6,7,8,9,10,11,12)] <- c("OBP", "SLG", "R", "RBI", "K", "BB", "HR", "OPS")

# Inspect the new dataset
head(mlb_features)

# Split the data into training and test sets
library(caret)
set.seed(123)
trainIndex <- createDataPartition(mlb_features$OBP, p = .8, list = FALSE)

mlb_train <- mlb_features[trainIndex, ]
mlb_test <- mlb_features[-trainIndex, ]

#creating a random forest model to predict OBP
library(randomForest)

# Train the random forest model
rf_model <- randomForest(OBP ~ ISO + BABIP + SLG + R + RBI + K + BB + HR, data = mlb_train, ntree = 500)

# Check the model's summary
print(rf_model)  

#evaluating model performance using test data
obp_predictions <- predict(rf_model, mlb_test)
rmse(predict(rf_model, mlb_test), mlb_test$OBP)

# Calculate the R-squared value
rsq <- cor(mlb_test$OBP, obp_predictions)^2
print(paste("R-squared:", round(rsq, 2)))

# Visualize predicted vs. actual OBP
library(ggplot2)

ggplot(mlb_test, aes(x = OBP, y = obp_predictions)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Actual vs. Predicted OBP",
    x = "Actual OBP",
    y = "Predicted OBP"
  )

# use caret to pick a value for mtry

tuned_model <- train(OBP ~ ISO + BABIP + SLG + R + RBI + K + BB + HR ,data = mlb_train, ntree = 50, #number of trees passed at random
                     method = "rf") # random forest

print(tuned_model)
  

# plot the rmse for various possible training values
ggplot(tuned_model)     

#Checking original against tuned model
print("base model rmse:")
print(rmse(predict(rf_model, mlb_test), mlb_test$OBP))

print("tuned model rmse:")
print(rmse(predict(tuned_model$finalModel, mlb_test), mlb_test$OBP))

# plot both plots at once
par(mfrow = c(1,2))

varImpPlot(rf_model, n.var = 5)
varImpPlot(tuned_model$finalModel, n.var = 5)



         
