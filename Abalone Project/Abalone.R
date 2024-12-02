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
males_split <- initial_split(males_cleaned, prop=0.80)
males_training_data <- training(males_split)

females_split <- initial_split(females_cleaned, prop=0.80)
females_training_data <- training(females_split)

summary(males_training_data)

#Devan Hoover
#Adding Testing Data

males_testing_data <- testing(males_split)
females_testing_data <- testing(females_split)

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

#Predictions for Training/Testing data
 
male_model <- lm(Rings ~ Length + Diameter + Whole_Weight, data = males_training_data)
summary(male_model)

female_model <- lm(Rings ~ Length + Diameter + Whole_Weight, data = females_training_data)
summary(female_model)

male_model2 <- lm(Rings ~ Length + Diameter + Height, data = males_training_data)
summary(male_model2)

male_predictions <- predict(male_model, newdata = males_testing_data)
female_predictions <- predict(female_model, newdata = females_testing_data)

male_predictions2 <- predict(male_model2, newdata = males_testing_data)

mae_male <- Metrics::mae(males_testing_data$Rings, male_predictions)
rmse_male <- Metrics::rmse(males_testing_data$Rings, male_predictions)

mae_male
rmse_male

mae_male2 <- Metrics::mae(males_testing_data$Rings, male_predictions2)
rmse_male2 <- Metrics::rmse(males_testing_data$Rings, male_predictions2)

mae_male2
rmse_male2

mae_female <- Metrics::mae(females_testing_data$Rings, female_predictions)
rmse_female <- Metrics::rmse(females_testing_data$Rings, female_predictions)
#Called Metrics Library due to library conflicts

plot(males_testing_data$Rings, male_predictions,
     main = "Male Model: Actual vs Predicted",
     xlab = "Actual Rings", ylab = "Predicted Rings",
     col = "blue", pch = 20)
abline(0,1,col = "red")

plot(males_testing_data$Rings, male_predictions2,
     main = "Linear Male Model: Actual vs Predicted",
     xlab = "Actual Rings", ylab = "Predicted Rings",
     col = "blue", pch = 20)
abline(0,1,col = "red")

#Devan Hoover
#Random Forest Testing

install.packages("randomForest")
library(randomForest)

male_rf_model <- randomForest(Rings ~ Length + Diameter + Whole_Weight + Height, data = males_training_data, importance = TRUE)
female_rf_model <- randomForest(Rings ~ Length + Diameter + Whole_Weight + Height, data = females_training_data, importance = TRUE)

print(male_rf_model)
print(female_rf_model)

#Poly Testing

male_poly_model <- lm(Rings ~ poly(Length, 2) + poly(Height, 2) + poly(Diameter, 2) + poly(Whole_Weight, 2), data = males_training_data)
summary(male_poly_model)

male_poly_model3 <- lm(Rings ~ poly(Length, 3) + poly(Height, 3) + poly(Diameter, 3) + poly(Whole_Weight, 3), data = males_testing_data)
summary(male_poly_model3)

male_poly_model_weightless <- lm(Rings ~ poly(Length, 2) + poly(Height, 2) + poly(Diameter, 2), data = males_training_data)
summary(male_poly_model_weightless)

#Scaling Poly Model and Retestings
males_training_data_scaled <- males_training_data %>% 
  mutate(
    Length = scale(Length),
    Height = scale(Height),
    Diameter = scale(Diameter)
  )

male_poly_model_scaled <- lm(Rings ~ poly(Length, 2) + poly(Height, 2) + poly(Diameter, 2), data = males_training_data_scaled)
summary(male_poly_model_scaled)

male_poly_model_scaled3 <- lm(Rings ~ poly(Length, 3) + poly(Height, 3) + poly(Diameter, 3), data = males_training_data_scaled)
summary(male_poly_model_scaled3)

male_poly_model_scaled4 <- lm(Rings ~ poly(Length, 4) + poly(Height, 4) + poly(Diameter, 4), data = males_training_data_scaled)
summary(male_poly_model_scaled4)

male_poly_model_scaled5 <- lm(Rings ~ poly(Length, 5) + poly(Height, 5) + poly(Diameter, 5), data = males_training_data_scaled)
summary(male_poly_model_scaled5)

#Iteration Testing

male_model_it <- lm(Rings ~ Length * Length + Height * Height + Whole_Weight + Diameter, data = males_training_data)
summary(male_model_it)

male_model_it3 <- lm(Rings ~ Length + Diameter + (Height * Height), data = males_training_data)
summary(male_model_it3)

#Prediction Tables

predicted_rings_males_poly <- predict(male_poly_model, newdata = males_testing_data)
predicted_rings_males_poly_rounded <- round(predicted_rings_males_poly)

poly_pred_table <- data.frame(
  Actual_Rings = males_testing_data$Rings,
  Predicted_Rings = predicted_rings_males_poly_rounded,
  Actual_Age = males_testing_data$Rings + 1.5,
  Predicted_Age = predicted_rings_males_poly_rounded + 1.5
)

head(poly_pred_table)

mae_pred_poly <- Metrics::mae(poly_pred_table$Actual_Rings, poly_pred_table$Predicted_Rings)
mae_pred_poly

male_predictions2_rounded <- round(male_predictions2)

linear_pred_table <- data.frame(
  Actual_Rings = males_testing_data$Rings,
  Predicted_Rings = male_predictions2_rounded,
  Actual_Age = males_testing_data$Rings + 1.5,
  Predicted_Age = male_predictions2_rounded + 1.5
)

head(linear_pred_table)

mae_pred_linear <- Metrics::mae(linear_pred_table$Actual_Rings, linear_pred_table$Predicted_Rings)
