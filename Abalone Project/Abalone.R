install.packages(c("tidyverse", "Metrics", "modelr", "broom", "lmtest", "zoo", "sandwich", "rstatix" , "ggplot2", "rsample"))

library(tidyverse)
library(Metrics)
library(modelr)
library(broom)
library(lmtest)
library(zoo)
library(sandwich)
library(rstatix)
library(ggplot2)
library(rsample)
library(glmnet) #used for ridge regression

data = read.csv("abalone.data")

colnames(data) <- c("Sex","Length", "Diameter", "Height", "Whole_Weight", "Schucked_Weight", "Viscera_Weight", "Shell_Weight", "Rings")
#data


summary(data)

males <- data %>% filter(Sex == "M")
females <- data %>% filter(Sex == "F")
infants <- data %>% filter(Sex == "I")

outliers_males <- males %>%
  select(-Sex) %>%  # Exclude non-numeric column 'Sex'
  gather() %>%
  group_by(key) %>%
  identify_outliers(value)

extreme_outliers_males <- outliers_males %>%
  filter(is.extreme == TRUE)
  
outliers_females <- females %>%
  select(-Sex) %>%
  gather() %>%
  group_by(key) %>%
  identify_outliers(value)

extreme_outliers_females <- outliers_females %>%
  filter(is.extreme == TRUE)

outliers_infants <- infants %>%
  select(-Sex) %>%
  gather() %>%
  group_by(key) %>%
  identify_outliers(value)

extreme_outliers_infants <- outliers_infants %>%
  filter(is.extreme == TRUE)

print(extreme_outliers_males)
print(extreme_outliers_females)
print(extreme_outliers_infants)

#Pierce Parker
# Function to drop rows with extreme outliers
drop_outliers <- function(data, extreme_outliers) {
  # Get the unique values and keys (columns) from extreme_outliers
  for (key in unique(extreme_outliers$key)) {
    values_to_remove <- extreme_outliers %>%
      filter(key == !!key) %>%
      pull(value)
    
    # Remove rows where the column has extreme outlier values
    data <- data %>% filter(!.data[[key]] %in% values_to_remove)
  }
  return(data)
}

# Drop extreme outliers from each subset
males_cleaned <- drop_outliers(males, extreme_outliers_males)
females_cleaned <- drop_outliers(females, extreme_outliers_females)
#infants_cleaned <- drop_outliers(infants, extreme_outliers_infants)

# Combine the cleaned subsets back into a single dataset
data_cleaned <- bind_rows(
  males_cleaned %>% mutate(Sex = "M"),
  females_cleaned %>% mutate(Sex = "F"),
  #infants_cleaned %>% mutate(Sex = "I")
)

# Check the cleaned data
summary(data_cleaned)
summary(infants)

#Thomas Poteet
#Splitting training and test data sets for both males and females seperate
set.seed(123)
males_split <- initial_split(males_cleaned, prop=0.80)
males_training_data <- training(males_split)
males_testing_data <- testing(males_split)

females_split <- initial_split(females_cleaned, prop=0.80)
females_training_data <- training(females_split)
females_testing_data <- testing(females_split)

summary(males_training_data)

#Thomas Poteet
#Finding Predictors of Whole_Weight
#Male
linear_male_weight <- lm(males_cleaned$Whole_Weight ~ males_cleaned$Length + males_cleaned$Diameter + males_cleaned$Height)
tidy(linear_male_weight)

#Whole_Weight ~ Length
 ggplot(males_cleaned, aes(x=Length, y=Whole_Weight)) +
  geom_point(color="blue") +
  geom_smooth(method="lm", formula=y ~ x, color="red") +
  labs(title="Male Length vs Whole Weight", x="Length", y="Whole Weight") +
  theme_minimal()
 #Whole_Weight ~ Diameter
 ggplot(males_cleaned, aes(x=Diameter, y=Whole_Weight)) +
  geom_point(color="blue") +
  geom_smooth(method="lm", formula=y ~ x, color="red") +
  labs(title="Male Diameter vs Whole Weight", x="Diameter", y="Whole Weight") +
  theme_minimal()
 #Whole_Weight ~ Height
 ggplot(males_cleaned, aes(x=Height, y=Whole_Weight)) +
  geom_point(color="blue") +
  geom_smooth(method="lm", formula=y ~ x, color="red") +
  labs(title="Male Height vs Whole Weight", x="Height", y="Whole Weight") +
  theme_minimal()
 
 #Female
 linear_female_weight <- lm(females_cleaned$Whole_Weight ~ females_cleaned$Length + females_cleaned$Diameter + females_cleaned$Height)
 tidy(linear_female_weight)
 
 #Whole_Weight ~ Length
 ggplot(females_cleaned, aes(x=Length, y=Whole_Weight)) +
   geom_point(color="blue") +
   geom_smooth(method="lm", formula=y ~ x, color="red") +
   labs(title="Female Length vs Whole Weight", x="Length", y="Whole Weight") +
   theme_minimal()
 #Whole_Weight ~ Diameter
 ggplot(females_cleaned, aes(x=Diameter, y=Whole_Weight)) +
   geom_point(color="blue") +
   geom_smooth(method="lm", formula=y ~ x, color="red") +
   labs(title="Female Diameter vs Whole Weight", x="Diameter", y="Whole Weight") +
   theme_minimal()
 #Whole_Weight ~ Height
 ggplot(females_cleaned, aes(x=Height, y=Whole_Weight)) +
   geom_point(color="blue") +
   geom_smooth(method="lm", formula=y ~ x, color="red") +
   labs(title="Female Height vs Whole Weight", x="Height", y="Whole Weight") +
   theme_minimal()
 
 #Thomas Poteet- Ridge Regression
 #source - https://www.statology.org/ridge-regression-in-r/
 
 #define response variable
 y <- males_training_data$Rings
 
 #define matrix of predictor variables
 x <- data.matrix(males_training_data[, c('Length', 'Diameter', 'Height', 'Whole_Weight')])
 
 # Extract the predictors from the testing dataset
 x_test <- data.matrix(males_testing_data[, c('Length', 'Diameter', 'Height', 'Whole_Weight')])
 
 # Extract the actual response variable from the testing dataset
 y_test <- males_testing_data$Rings
 
 #fit ridge regression model
 ridge_model <- glmnet(x, y, alpha = 0)
 
 summary(ridge_model)
 
 #perform k-fold cross-validation to find optimal lambda value
 cv_model <- cv.glmnet(x, y, alpha = 0)
 
 #find optimal lambda value that minimizes test MSE
 best_lambda <- cv_model$lambda.min
 best_lambda
 
 #produce plot of test MSE by lambda value
 plot(cv_model)
 
 #find coefficients of best model
 best_model <- glmnet(x,y, alpha = 0, lambda = best_lambda)
 coef(best_model)

 #produce Ridge trace plot
 plot(ridge_model, xvar = "lambda")
 
 #use fitted best model to make predictions
 y_predicted <- predict(ridge_model, s = best_lambda, newx = x)
 
 #find SST and SSE
 sst <- sum((y-mean(y))^2)
 sse <- sum((y_predicted - y)^2)
 mse <- mean((y - y_predicted)^2)
 mse
 
 #find R-Squared
 rsq <- 1 - sse/sst
 rsq

 # Predict using the best ridge regression model
 y_test_predicted <- predict(best_model, newx = x_test)
 
 # Compute the Mean Squared Error
 mse_test <- mean((y_test - y_test_predicted)^2)
 mse_test
 
 sst_test <- sum((y_test - mean(y_test))^2)  # Total Sum of Squares
 sse_test <- sum((y_test - y_test_predicted)^2)  # Sum of Squared Errors
 rsq_test <- 1 - (sse_test / sst_test)
 rsq_test
 
 mae_test <- mean(abs(y_test - y_test_predicted))
 mae_test
 
 rmse_test <- sqrt(mean((y_test - y_test_predicted)^2))
 rmse_test
 
 # Plot actual vs. predicted values
 plot(y_test, y_test_predicted, 
      main = paste("Actual vs Predicted\nR-squared =", round(rsq_test, 4)), 
      xlab = "Actual", ylab = "Predicted", 
      pch = 16, col = "blue", cex = 1.5)
 
 # Add a line representing perfect predictions (45-degree line)
 abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)