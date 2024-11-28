install.packages(c("tidyverse", "Metrics", "modelr", "broom", "lmtest", "zoo", "sandwich", "rstatix" , "ggplot2"))

library(tidyverse)
library(Metrics)
library(modelr)
library(broom)
library(lmtest)
library(zoo)
library(sandwich)
library(rstatix)
library(ggplot2)

data = read.csv("abalone.data")

colnames(data) <- c("Sex","Length", "Diameter", "Height", "Whole_Weight", "Schucked_Weight", "Viscera_Weight", "Shell_Weight", "Rings")
#data



summary(data)

males <- data %>% filter(Sex == "M")
females <- data %>% filter(Sex == "F")
infants <- data %>% filter(Sex == "I")

#dropped_infants


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
#Finding Predictors of Whole_Weight
#Male
linear_male_weight <- lm(males_cleaned$Whole_Weight ~ males_cleaned$Length + males_cleaned$Diameter + males_cleaned$Height)
linear_male_weight

#Whole_Weight ~ Length
ggplot(linear_male_weight)
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
 linear_female_weight
 
 #Whole_Weight ~ Length
 ggplot(linear_female_weight)
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

 #Devan Hoover
 #Predictors For Rings With Models
 linear_male_rings <- lm(Rings ~ Length + Diameter + Height + Whole_Weight, data = males_cleaned)
 summary(linear_male_rings)
 
 #Male Rings Equation
 #Rings = 4.7770 - 13.0274 * Length + 17.3689 * Diameter + 38.5909 * Height -0.2957 * Whole_Weight
 
 ggplot(males_cleaned, aes(x=Length, y=Rings)) +
   geom_point(color='blue') +
   geom_smooth(method ='lm', formula=y ~ x, color='red') +
   labs(title='Male Length vs Rings',x='Length', y='Rings')+
   theme_minimal()

 ggplot(males_cleaned, aes(x=Diameter, y=Rings)) +
   geom_point(color='blue') +
   geom_smooth(method ='lm', formula=y ~ x, color='red') +
   labs(title='Male Diameter vs Rings',x='Diameter', y='Rings')+
   theme_minimal()
 
 ggplot(males_cleaned, aes(x=Height, y=Rings)) +
   geom_point(color='blue') +
   geom_smooth(method ='lm', formula=y ~ x, color='red') +
   labs(title='Male Height vs Rings',x='Height', y='Rings')+
   theme_minimal()
 
 ggplot(males_cleaned, aes(x=Whole_Weight, y=Rings)) +
   geom_point(color='blue') +
   geom_smooth(method ='lm', formula=y ~ x, color='red') +
   labs(title='Male Whole_Weight vs Rings',x='Whole_Weight', y='Rings')+
   theme_minimal()
 
 #Predictions and Residuals for Linear Model
 predicted_rings_males <- predict(linear_male_rings, newdata = males_cleaned)
 residuals <- males_cleaned$Rings - predicted_rings_males
 summary(residuals)
 
 #MAE RMSE and R^2
 
 mae_value <- mean(abs(males_cleaned$Rings - predicted_rings_males))
 print(paste("Mean Absolute Error (MAE):", round(mae_value, 2)))
 
 rmse <- sqrt(mean(residuals^2))
 print(paste("Root Mean Squared Error (RMSE):", round(rmse, 2)))
 
 #From Summary of linear_male_rings
 r_squared_linear = 0.21
 
 #Some Additional Interactions for testing
 
 linear_male_rings_int1 <- lm(Rings ~ Length * Diameter + Height + Whole_Weight, data = males_cleaned)
 summary(linear_male_rings_int1)
 
 linear_male_rings_int2 <- lm(Rings ~ Length * Diameter * Height + Whole_Weight, data = males_cleaned)
 summary(linear_male_rings_int2)