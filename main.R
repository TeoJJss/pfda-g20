## DATA IMPORT  ##
# Install rstudioapi #
install.packages("rstudioapi")
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
    # capped outlier as red dots
    geom_point(data = credit_risk_df_capped[credit_risk_df_capped$capped_outlier, ], aes_string(x = "1", y = col), color = "red", size = 2) +  
    labs(title = paste("Boxplot of capped", col), y = col) +
    theme_minimal() +
    coord_flip()
  
  # save the boxplot
  ggsave(paste0(col, "_cappedboxplot.png"), plot = plot, width = 12, height = 8, dpi = 300, bg = 'white')
}

head(credit_risk_df_capped) # DF for individual component

## DATA ANALYSIS (Individual) ##
# Chua Song Wen, TP075130
# Initial Preparation
credit_risk_df_capped_sw <- credit_risk_df_capped;

# Objective 1
# Distribution of Purpose - contingency table and pie chart
table_purpose <- credit_risk_df_capped_sw %>%
  count(purpose) %>%
  mutate(percentage = n / nrow(credit_risk_df_capped_sw) * 100)
print(table_purpose)

piechart_purpose<-ggplot(credit_risk_df_capped_sw, aes(x = "", fill = factor(purpose))) +
  geom_bar(position = "fill", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Purpose",
       fill = "Purpose") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
print(piechart_purpose)
ggsave("piechart_purpose.png", plot = piechart_purpose, width = 12, height = 8, dpi = 300, bg = 'white')

# Relationship between Purpose and Class - p-value,contingency table and bar chart
purpose_vs_class <- table(credit_risk_df_capped_sw$class, credit_risk_df_capped_sw$purpose)
summary(purpose_vs_class)

table_purpose_vs_class<- credit_risk_df_capped_sw %>%
  count(purpose, class) %>%
  group_by(purpose) %>%
  mutate(percentage = n / sum(n) * 100)
print(table_purpose_vs_class)

barchart_purpose_vs_class<-ggplot(credit_risk_df_capped_sw, aes(x = purpose, fill = factor(class))) +
  geom_bar(alpha = 0.7) +
  labs(title = "Distribution of Purpose by Class",
       x = "Purpose",
       y = "Count",
       fill = "Class") +
  theme_minimal() +
  theme() 
print(barchart_purpose_vs_class)
ggsave("barchart_purpose_vs_class.png", plot = barchart_purpose_vs_class, width = 12, height = 8, dpi = 300, bg = 'white')

# Purpose Category - base on consumption power - summarize table,proportions and pie chart
credit_risk_df_capped_sw <- credit_risk_df_capped_sw %>%
  mutate(
    purpose_HighConsumptionPower = ifelse(purpose %in% c("used car","new car", "business"), "Yes", "No"),
    purpose_MediumConsumptionPower = ifelse(purpose %in% c("furniture/equipment", "radio/tv", "education","retraining"), "Yes", "No"),
    purpose_LowConsumptionPower = ifelse(purpose %in% c("domestic appliance", "repairs", "other"), "Yes", "No"),
      )

summary_purposeCategory_vs_class <- credit_risk_df_capped_sw %>%
  mutate(
    purpose_category = case_when(
      purpose %in% c("used car","new car", "business") ~ "HighConsumptionPower",
      purpose %in% c("furniture/equipment", "radio/tv", "education","retraining") ~ "MediumConsumptionPower",
      purpose %in% c("domestic appliance", "repairs", "other") ~ "LowConsumptionPower"
    )
  ) %>%
  group_by(purpose_category, class) %>%
  summarize(count = n(), .groups = "drop")
print(summary_purposeCategory_vs_class)

proportions_purposeCategory_vs_class <- summary_purposeCategory_vs_class %>%
  group_by(purpose_category) %>%
  mutate(
    total = sum(count),
    proportion = count / total
  ) %>%
  arrange(purpose_category, desc(class))
print(proportions_purposeCategory_vs_class)

piechart_purposeCategory_vs_class<-ggplot(proportions_purposeCategory_vs_class, aes(x = "", y = proportion, fill = class)) +
  geom_bar(stat = "identity", width = 1, color = "white") + 
  coord_polar("y", start = 0) +
  facet_wrap(~purpose_category) + 
  theme_void() + 
  labs(
    title = "Proportion of Good and Bad Credit by Consumption Power Category",
    fill = "Credit Class"
  ) +
  scale_fill_manual(
    values = c("good" = "#00BFC4", "bad" = "#F8766D"), 
    labels = c("Bad Credit", "Good Credit")
  )
print(piechart_purposeCategory_vs_class)
ggsave("piechart_purposeCategory_vs_class.png", plot = piechart_purposeCategory_vs_class, width = 12, height = 8, dpi = 300, bg = 'white')

credit_risk_df_capped_sw <- credit_risk_df_capped_sw %>%
  mutate(
    purpose_category = case_when(
      purpose %in% c("new car", "business") ~ "HighConsumptionPower",
      purpose %in% c("used car","furniture/equipment", "radio/tv", "education") ~ "MediumConsumptionPower",
      purpose %in% c("domestic appliance", "repairs", "other", "retraining") ~ "LowConsumptionPower"
    )
  )
purposeCategory_vs_class <- table(credit_risk_df_capped_sw$class, credit_risk_df_capped_sw$purpose_category)
summary(purposeCategory_vs_class)

# Hypothesis Testing - Logistic Regression
credit_risk_df_capped_sw <- credit_risk_df_capped_sw %>%
  mutate(
    class_numeric = ifelse(class == "good", 1, 0)
  )
logistic_regression_purpose_MediumConsumptionPower_vs_class <- glm(class_numeric ~ purpose_MediumConsumptionPower, 
                                     data = credit_risk_df_capped_sw, 
                                     family = binomial)
summary(logistic_regression_purpose_MediumConsumptionPower_vs_class)

# Objective 2
# Distribution of Employment - contingency table andbar chart
table_employment <- credit_risk_df_capped_sw %>%
  count(employment) %>%
  mutate(percentage = n / nrow(credit_risk_df_capped_sw) * 100)
print(table_employment)

barchart_employment<-ggplot(credit_risk_df_capped, aes(x = employment, fill = factor(employment))) +
  geom_bar(position = "dodge", alpha = 0.7) +
  labs(title = "Distribution of Employment Levels by Class",
       x = "Employment Level",
       y = "Count",
       fill = "Employment Level") +
  theme_minimal()
print(barchart_employment)
ggsave("barchart_employment.png", plot = barchart_employment, width = 12, height = 8, dpi = 300, bg = 'white')

# Relationship between Employment and Class - p-value, contingency table, bar chart
employment_vs_class <- table(credit_risk_df_capped_sw$class, credit_risk_df_capped_sw$employment)
summary(employment_vs_class)

table_employment_vs_class<- credit_risk_df_capped_sw %>%
  count(employment,class) %>%
  group_by(employment) %>%
  mutate(percentage = n / sum(n) * 100)
print(table_employment_vs_class)

barchart_employment_vs_class<-ggplot(credit_risk_df_capped, aes(x = employment, fill = factor(class))) +
  geom_bar(position = "dodge", alpha = 0.7) +
  labs(title = "Distribution of Employment Levels by Class",
       x = "Employment",
       y = "Count",
       fill = "Credit Class") +
  theme_minimal()
print(barchart_employment_vs_class)
ggsave("barchart_employment_vs_class.png", plot = barchart_employment_vs_class, width = 12, height = 8, dpi = 300, bg = 'white')

# Employment Category - base on duration - summarize table,proportions and pie chart
credit_risk_df_capped_sw <- credit_risk_df_capped_sw %>%
  mutate(
    employment_ShortTerm = ifelse(employment == "<1", "Yes", "No"),
    employment_MediumTerm = ifelse(employment %in% c("1<=x<4", "4<=x<7"), "Yes", "No"),
    employment_LongTerm = ifelse(employment == ">=7", "Yes", "No"),
    employment_IsUnemployed = ifelse(employment == "unemployed", "Yes", "No")
  )

summary_employmentCategory_vs_class <- credit_risk_df_capped_sw %>%
  mutate(
    employment_category = case_when(
      employment %in% c("<1") ~ "Short-Term",
      employment %in% c("1<=x<4","4<=x<7") ~ "Medium-Term",
      employment %in% c(">=7") ~ "Long-Term",
      employment %in% c("unemployed") ~ "IsUnemployed"
    )
  ) %>%
  group_by(employment_category, class) %>%
  summarize(count = n(), .groups = "drop")
print(summary_employmentCategory_vs_class)

proportions_employmentCategory_vs_class <- summary_employmentCategory_vs_class %>%
  group_by(employment_category) %>%
  mutate(
    total = sum(count),
    proportion = count / total
  ) %>%
  arrange(employment_category, desc(class))
print(proportions_employmentCategory_vs_class)

piechart_employmentCategory_vs_class<-ggplot(proportions_employmentCategory_vs_class, aes(x = "", y = proportion, fill = class)) +
  geom_bar(stat = "identity", width = 1, color = "white") + 
  coord_polar("y", start = 0) +
  facet_wrap(~employment_category) + 
  theme_void() + 
  labs(
    title = "Proportion of Good and Bad Credit by Employment Duration",
    fill = "Credit Class"
  ) +
  scale_fill_manual(
    values = c("good" = "#00BFC4", "bad" = "#F8766D"), 
    labels = c("Bad Credit", "Good Credit")
  )
print(piechart_employmentCategory_vs_class)
ggsave("piechart_employmentCategory_vs_class.png", plot = piechart_employmentCategory_vs_class, width = 12, height = 8, dpi = 300, bg = 'white')

credit_risk_df_capped_sw <- credit_risk_df_capped_sw %>%
  mutate(
    employment_category = case_when(
      employment %in% c("<1") ~ "Short-Term",
      employment %in% c("1<=x<4","4<=x<7") ~ "Medium-Term",
      employment %in% c(">=7") ~ "Long-Term",
      employment %in% c("unemployed") ~ "IsUnemployed"
    )
  )
employmentCategory_vs_class <- table(credit_risk_df_capped_sw$class, credit_risk_df_capped_sw$employment_category)
summary(employmentCategory_vs_class)

# Hypothesis Testing - Logistic Regression
logistic_regression_employment_LongTerm_vs_class <- glm(class_numeric ~ employment_LongTerm, 
                                                                   data = credit_risk_df_capped_sw, 
                                                                   family = binomial)
summary(logistic_regression_employment_LongTerm_vs_class)

# Objective 3
# Relationship between Employment Category and Purpose Category - contingency table, bar chart
table_employmentCategory_vs_purposeCategory<- credit_risk_df_capped_sw %>%
  count(employment_category,purpose_category) %>%
  group_by(employment_category) %>%
  mutate(percentage = n / sum(n) * 100)
print(table_employmentCategory_vs_purposeCategory)

barchart_employmentCategory_vs_purposeCategory<-ggplot(credit_risk_df_capped_sw, aes(x = employment_category, fill = factor(purpose_category))) +
  geom_bar(position = "dodge", alpha = 0.7) +
  labs(
    title = "Distribution of Employment by Purpose",
    x = "Purpose",
    y = "Count",
    fill = "Employment"
  ) +
  theme_minimal()
print(barchart_employmentCategory_vs_purposeCategory)
ggsave("barchart_employmentCategory_vs_purposeCategory.png", plot = barchart_employmentCategory_vs_purposeCategory, width = 12, height = 8, dpi = 300, bg = 'white')

# Hypothesis Testing - Chi Square Test
table_employmentCategory_vs_purposeCategory<-table(credit_risk_df_capped_sw$employment_category,credit_risk_df_capped_sw$purpose_category)
chisq.test(table_employmentCategory_vs_purposeCategory)

# Objective 4
# Hypothesis Testing - Logistic Regression
logistic_model_purposeMediumConsumptionPower_vs_employmentLongTerm_vs_class <- glm(class_numeric ~ purpose_MediumConsumptionPower*employment_LongTerm, 
                                            data = credit_risk_df_capped_sw, 
                                            family = binomial)
summary(logistic_model_purposeMediumConsumptionPower_vs_employmentLongTerm_vs_class)
