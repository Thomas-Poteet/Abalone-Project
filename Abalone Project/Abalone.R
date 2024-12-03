install.packages(c("tidyverse", "Metrics", "modelr", "broom", "lmtest", "zoo", "sandwich", "rstatix" , "ggplot2", "rsample", "rpart.plot"))
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
library(rpart)
library(rpart.plot)
library(ipred)
library(caret)

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
males_testing_data <- testing(males_split)

females_split <- initial_split(females_cleaned, prop=0.80)
females_training_data <- training(females_split)

summary(males_testing_data)

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
 
 
 males_cleaned <- males_cleaned |> select(!Sex)
 
 head(males_training_data)
 
 #first we make a model using all the features to predict the number of rings 
 reg_tree <- rpart(Rings ~ ., data = males_cleaned, method = "anova")
 
 rpart.plot(reg_tree)
 
 #as you can see from the plot
 
 plotcp(reg_tree)
 
 reg_tree <- rpart(Rings ~ ., data = males_cleaned, method = "anova", control = list(cp = 0, xval = 10))
 
 plotcp(reg_tree)
 
 #rpart.plot(reg_tree)
 
 #reg_tree <- rpart(Rings ~ Length + Diameter + Height + Whole_Weight, data = males_training_data, method = "anova", control = list(minsplit = 16, maxdepth = 14, xval = 10))
 
 hyper_grid <- expand.grid(
   minsplit = seq(5, 25, 1),
   maxdepth = seq(5, 15, 1)
 )
 head(hyper_grid) 
 
 
 
 reg_tree_models <- list()
 
 for (i in 1:nrow(hyper_grid)) {
   
   # get minsplit, maxdepth values at row i
   minsplit <- hyper_grid$minsplit[i]
   maxdepth <- hyper_grid$maxdepth[i]
   
   # train a model and store in the list
   reg_tree_models[[i]] <- rpart(
     formula = Rings ~ Length + Diameter + Height + Whole_Weight,
     data    = males_training_data,
     method  = "anova",
     control = list(minsplit = minsplit, maxdepth = maxdepth)
   )
 }
 
 
 # function to get optimal cp
 get_cp <- function(x) {
   min    <- which.min(x$cptable[, "xerror"])
   cp <- x$cptable[min, "CP"] 
 }
 
 # function to get minimum error
 get_min_error <- function(x) {
   min    <- which.min(x$cptable[, "xerror"])
   xerror <- x$cptable[min, "xerror"] 
 }
 
 hyper_grid %>%
   mutate(
     cp    = purrr::map_dbl(reg_tree_models, get_cp),
     error = purrr::map_dbl(reg_tree_models, get_min_error)
   ) %>%
   arrange(error) %>%
   top_n(-5, wt = error)
 
 optimal_tree <- rpart(Rings ~ Length + Diameter + Height + Whole_Weight, 
                       data = males_training_data, 
                       method = "anova",
                       control = list(minsplit = 16, maxdepth = 14, cp = 0.01)
                       )
 
 pred <- predict(optimal_tree, newdata = males_testing_data)
 
 mae <- mean(abs(males_testing_data$Rings - pred))
 mae
 
 rmse <- sqrt(mean((males_testing_data$Rings - pred)^2))
 rmse
 
 ss_total <- sum((males_testing_data$Rings - mean(males_testing_data$Rings))^2)
 ss_residual <- sum((males_testing_data$Rings - pred)^2)
 r_squared <- 1 - (ss_residual / ss_total)
 r_squared
 
 rpart.plot(optimal_tree)
 plotcp(optimal_tree)
 
 