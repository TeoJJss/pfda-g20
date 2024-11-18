## DATA IMPORT  ##
# Install rstudioapi #
# install.packages("rstudioapi")
library(rstudioapi)

# Set working environment #
CSV_FILE <- "5. credit_risk_classification.csv" # csv file name
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path) # get current directory (where R script is stored)
setwd(current_dir) # Set working directory

# Install packages RUN ONCE ONLY #
required_packages <- c("dplyr", "ggplot2", "tidyverse")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) { # If the package is not available
    install.packages(pkg) # Install the package
  }
}
rm(required_packages)
rm(pkg)
 
# Read CSV file and save to data frame #
library(readr) # load from tidyverse
credit_risk_df <- read_csv(CSV_FILE) # load data frame
head(credit_risk_df) # check data frame


## DATA CLEANING / PRE-PROCESSING  ##
# Remove index column
colnames(credit_risk_df)
credit_risk_df["...1"] = NULL;

# Check & Remove Duplicates #
library(dplyr)
nrow(credit_risk_df)# Show how many records this data set has
sum(duplicated(credit_risk_df))# Show the count of duplicated records
nrow(unique(credit_risk_df))# Show the count of unique records
nrow(distinct(credit_risk_df))# Show the count of distinct records

credit_risk_df<- unique(credit_risk_df)# Save into another version without duplicates
print(credit_risk_df)

# Remove unnecessary columns #
selected_cols = c(
  "purpose", "employment", "property_magnitude", "age", "job", 
  "credit_history", "installment_commitment", "existing_credits", "class"
) # columns required
credit_risk_df = credit_risk_df[, selected_cols] # select required
head(credit_risk_df)

# Check & Handle missing value #
empty_columns <- credit_risk_df %>%
  select(where(~ all(is.na(.) | . == "")))
colnames(empty_columns)


## DATA VALIDATION ##
# Standardize Data/Format #

#check variable type
str(credit_risk_df)

#for char variable

char_cols = c(
  "purpose", "employment", "property_magnitude", "job", 
  "credit_history")
credit_risk_df[char_cols]<-lapply(credit_risk_df[char_cols],tolower) #to turn the data to lowercase
credit_risk_df[char_cols]<-lapply(credit_risk_df[char_cols],factor)

#for numerical
#age
summary(credit_risk_df$age)
has_decimal<-any(credit_risk_df$age %% 1!=0)

invalid_age <- sum(credit_risk_df$age != floor(credit_risk_df$age))
invalid_age

if (has_decimal) {
  print("There are decimal values in the age variable.")
} else {
  print("All values in the age variable are whole numbers.")
}

#installment_commitment
summary(credit_risk_df$installment_commitment)
summary(credit_risk_df$existing_credits)

rounddata<-c("age","installment_commitment","existing_credits")
credit_risk_df[rounddata]<-lapply(credit_risk_df[rounddata],function(x) round(x, 0))

check_decimal<-any( credit_risk_df$age %% 1!=0)
check_decimal
check_decimal<-any( credit_risk_df$installment_commitment %% 1!=0)
check_decimal
check_decimal<-any( credit_risk_df$existing_credits %% 1!=0)
check_decimal

# Check Range #
age_range<-range(credit_risk_df$age,na.rm = TRUE)
age_range

ic_range<-range(credit_risk_df$installment_commitment,na.rm = TRUE)
ic_range

ec_range<-range(credit_risk_df$existing_credits,na.rm = TRUE)
ec_range

# Check Categories #
levels(credit_risk_df$purpose)
levels(credit_risk_df$employment)
levels(credit_risk_df$property_magnitude)
levels(credit_risk_df$job)
levels(credit_risk_df$credit_history)

# Outliers Detection & Handling #
# detect outlier for numeric data
numeric_cols <- names(credit_risk_df)[sapply(credit_risk_df, is.numeric)]
numeric_cols

# boxplot before cap
library(ggplot2)
for (col in numeric_cols) {
  plot = ggplot(credit_risk_df, aes_string(x = "1", y = col)) + 
    geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
    labs(title = paste("Boxplot of ", col), y = col) +
    theme_minimal() +
    coord_flip()
  
  ggsave(paste0(col, "_boxplot.png"), plot = plot, width = 12, height = 8, dpi = 300,bg='white')
}


credit_risk_df_capped = credit_risk_df

# drop column that does not require capping
capped_cols = c('age', 'existing_credits')

for (col in capped_cols) {
  
  count_capped = 0
  
  # calculate Q1, Q3, and IQR
  Q1 = quantile(credit_risk_df[[col]], 0.25)
  Q3 = quantile(credit_risk_df[[col]], 0.75)
  IQR = Q3 - Q1
  
  # calculate lowerbound and upperbound
  lowerbound = Q1 - 2 * IQR
  upperbound = Q3 + 2 * IQR
  
  # count how many values will be capped
  count_capped = count_capped + sum(credit_risk_df_capped[[col]] < lowerbound | credit_risk_df_capped[[col]] > upperbound)
  
  # cap the outliers with lowerbound and upperbound
  credit_risk_df_capped[[col]] <- ifelse(credit_risk_df_capped[[col]] < lowerbound, round(lowerbound),
                             ifelse(credit_risk_df_capped[[col]] > upperbound, round(upperbound), credit_risk_df_capped[[col]]))
  
  # print summary
  print(col)
  print(paste("Lowerbound:", round(lowerbound)))
  print(paste("Upperbound:", round(upperbound)))
  print(paste("Capped Count:", count_capped))
  cat("\n")
  
}


# boxplot after cap (blue - outliers that were capped)
for (col in capped_cols) {
  # identify outliers in the original data frame
  outlier_thresholds <- boxplot.stats(credit_risk_df[[col]])$out
  credit_risk_df$previous_outlier <- credit_risk_df[[col]] %in% outlier_thresholds
  
  # identify capped outliers
  capped_outlier_thresholds <- boxplot.stats(credit_risk_df_capped[[col]])$out
  credit_risk_df_capped$capped_outlier <- credit_risk_df_capped[[col]] %in% capped_outlier_thresholds
  
  # create the boxplot
  plot <- ggplot(credit_risk_df_capped, aes_string(x = "1", y = col)) +
    # do not display outliers in the boxplot
    geom_boxplot(fill = "lightblue", outlier.color = NA) +  
    # previous outliers as blue dots from credit_risk_df
    geom_point(data = credit_risk_df[credit_risk_df$previous_outlier, ], aes_string(x = "1", y = col), color = "blue", size = 2) + 
    # present outlier as red dots
    geom_point(data = credit_risk_df_capped[credit_risk_df_capped$capped_outlier, ], aes_string(x = "1", y = col), color = "red", size = 2) +  
    labs(title = paste("Boxplot of capped", col), y = col) +
    theme_minimal() +
    coord_flip()
  
  # save the boxplot
  ggsave(paste0(col, "_cappedboxplot.png"), plot = plot, width = 12, height = 8, dpi = 300, bg = 'white')
}

head(credit_risk_df_capped) # DF for individual component

## DATA ANALYSIS (Individual) ##
# Leong Huey Chian, TP084911


# Chua Song Wen, TP075130



# Teo Jun Jia, TP067775



# Isabelle Gwenca Fong, TP077838
credit_risk_df_capped_isabelle = credit_risk_df_capped

# understand the data 
head(credit_risk_df_capped_isabelle)
summary(credit_risk_df_capped_isabelle)
str(credit_risk_df_capped_isabelle)

# convert credit class to factor
credit_risk_df_capped_isabelle$class <- as.factor(credit_risk_df_capped_isabelle$class)

# Objective 1: To investigate the relationship between property magnitude and credit class

# Distribution Analysis – contingency table and stacked bar plot
# cross-tabulate property magnitude and credit class
property_magnitude_credit_class_table = table(credit_risk_df_capped_isabelle$property_magnitude, credit_risk_df_capped_isabelle$class)

print(property_magnitude_credit_class_table)

# stacked bar plot
property_magnitdue_vs_credit_class_stacked_bar = ggplot(credit_risk_df_capped_isabelle, aes(x = property_magnitude, fill = class)) +
  geom_bar(position = "fill") +
  labs(title = "Bivariate Analysis of Property Magnitude vs Credit Class",
       x = "Property Magnitude",
       y = "Proportion") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

print(property_magnitdue_vs_credit_class_stacked_bar)

# save the stacked bar chart of property magnitude and credit class
ggsave("property_magnitdue_vs_credit_class_stacked_bar.png", plot = property_magnitdue_vs_credit_class_stacked_bar, width = 12, height = 8, dpi = 300, bg = 'white')

# Relationship Analysis
install.packages("DescTools")
library(DescTools)

# Cramér's V 
CramerV(property_magnitude_credit_class_table)

# Hypothesis Testing 
# create binary real estate variable
credit_risk_df_capped_isabelle$real_estate = ifelse(credit_risk_df_capped_isabelle$property_magnitude == "real estate", "yes", "no")
credit_risk_df_capped_isabelle$real_estate = factor(credit_risk_df_capped_isabelle$real_estate, levels = c("no", "yes"))
summary(credit_risk_df_capped_isabelle)

# calculate proportions for each real_estate status
credit_risk_df_capped_isabelle_summary <- credit_risk_df_capped_isabelle %>%
  group_by(real_estate, class) %>%
  tally() %>%å
  group_by(real_estate) %>%
  mutate(proportion = n / sum(n),
         percentage = proportion * 100)

# pie chart for real_estate with percentage
real_estate_status_vs_credit_class_pie = ggplot(credit_risk_df_capped_isabelle_summary, aes(x = "", y = proportion, fill = class)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") + 
  facet_wrap(~ real_estate) +  
  labs(title = "Credit Class Distribution by Real Estate Status",
       fill = "Credit Class") +
  scale_y_continuous(labels = scales::percent_format()) +  
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5), color = "white") 

print(real_estate_status_vs_credit_class_pie)

# save the pie chart of real estate status and credit class
ggsave("real_estate_status_vs_credit_class_pie.png", plot = real_estate_status_vs_credit_class_pie, width = 12, height = 8, dpi = 300, bg = 'white')

# logistic regression
logistic_regression_real_estate_model = glm(class ~ real_estate, data = credit_risk_df_capped_isabelle, family = binomial)
summary(logistic_regression_real_estate_model)

# calculate odds ratios
exp(coef(logistic_regression_real_estate_model))

# Objective 2: To investigate the relationship between age and credit class

# Distribution Analysis
# density plot of age by credit class
age_credit_class_density = ggplot(credit_risk_df_capped_isabelle, aes(x = age, fill = class)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Age by Credit Class (Before Transformation)",
       x = "Age", y = "Density") +
  theme_minimal()

print(age_credit_class_density)

# save the density of age and credit class
ggsave("age_credit_class_density.png", plot = age_credit_class_density, width = 12, height = 8, dpi = 300, bg = 'white')

# Dispersion Analysis – summary stats/variance/standard deviation
#summary statistics for age by credit class
summary_stats = aggregate(age ~ class, data = credit_risk_df_capped_isabelle, summary)
print(summary_stats)

# variance and standard deviation for age by credit class
dispersion_stats = aggregate(age ~ class, data = credit_risk_df_capped_isabelle, function(x) c(var = var(x), sd = sd(x)))
print(dispersion_stats)

# Relationship Analysis - Correlation
# correlation between age and numeric representation of credit class (good=1, bad=0)
credit_risk_df_capped_isabelle$class_numeric = as.numeric(factor(credit_risk_df_capped_isabelle$class)) - 1
age_credit_class_correlation = cor(credit_risk_df_capped_isabelle$age, credit_risk_df_capped_isabelle$class_numeric)
print(age_credit_class_correlation)

str(credit_risk_df_capped_isabelle)

# Hypothesis Testing
# create a new variable binary age above 35
credit_risk_df_capped_isabelle$age_above_35 = ifelse(credit_risk_df_capped_isabelle$age > 35, "yes", "no")
credit_risk_df_capped_isabelle$age_above_35 = factor(credit_risk_df_capped_isabelle$age_above_35, levels = c("no", "yes"))
summary(credit_risk_df_capped_isabelle)

# calculate proportions for age above 35 
credit_risk_df_capped_isabelle_summary = credit_risk_df_capped_isabelle %>%
  group_by(age_above_35, class) %>%
  tally() %>%
  group_by(age_above_35) %>%
  mutate(proportion = n / sum(n),
         percentage = proportion * 100)

# pie chart for age above 35 with percentage
age_above_35_vs_credit_class_pie = ggplot(credit_risk_df_capped_isabelle_summary, aes(x = "", y = proportion, fill = class)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") + 
  facet_wrap(~ age_above_35) +  
  labs(title = "Credit Class Distribution by Age Above 35",
       fill = "Credit Class") +
  scale_y_continuous(labels = scales::percent_format()) +  
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5), color = "white") 

print(age_above_35_vs_credit_class_pie)

# save the pie chart of age above 35 and credit class
ggsave("age_above_35_vs_credit_class_pie.png", plot = age_above_35_vs_credit_class_pie, width = 12, height = 8, dpi = 300, bg = 'white')

# logistic regression
logistic_regression_age_above_35_model = glm(class ~ age_above_35, data = credit_risk_df_capped_isabelle, family = binomial)
summary(logistic_regression_age_above_35_model)

# calculate odds ratios
exp(coef(logistic_regression_age_above_35_model))


# Objective 3: To investigate the interaction effect of property magnitude and age on credit class

#logistic regression model with interactions
age_above_35_real_estate_interaction_logistic_model = glm(class ~ age_above_35 * real_estate, family = binomial, data = credit_risk_df_capped_isabelle)

summary(age_above_35_real_estate_interaction_logistic_model)

