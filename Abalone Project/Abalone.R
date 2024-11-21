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
colnames(data) <- c("Sex","Length", "Diameter", "Height", "Whole_Weight", "Schucked_Weight", "Viscera_Weight", "Shell Weight", "Rings")
#data

summary(data)

males <- data %>% filter(Sex == "M")
females <- data %>% filter(Sex == "F")
infants <- data %>% filter(Sex == "I")

dropped_infants


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
linear_male_weight <- lm(males$Whole_Weight ~ males$Length + males$Diameter + males$Height)
linear_male_weight

#Whole_Weight ~ Length
ggplot(linear_male_weight)
 ggplot(males, aes(x=Length, y=Whole_Weight)) +
  +     geom_point(color="blue") +
  +     geom_smooth(method="lm", formula=y ~ x, color="red") +
  +     labs(title="Length vs Whole Weight", x="Length", y="Whole Weight") +
  +     theme_minimal()
 #Whole_Weight ~ Diameter
 ggplot(males, aes(x=Diameter, y=Whole_Weight)) +
  +     geom_point(color="blue") +
  +     geom_smooth(method="lm", formula=y ~ x, color="red") +
  +     labs(title="Diameter vs Whole Weight", x="Diameter", y="Whole Weight") +
  +     theme_minimal()
 #Whole_Weight ~ Height
 ggplot(males, aes(x=Height, y=Whole_Weight)) +
  +     geom_point(color="blue") +
  +     geom_smooth(method="lm", formula=y ~ x, color="red") +
  +     labs(title="Height vs Whole Weight", x="Height", y="Whole Weight") +
  +     theme_minimal()

