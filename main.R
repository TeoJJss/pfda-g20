# Leong Huey Chian, TP084911
# Chua Song Wen, TP075130
# Teo Jun Jia, TP067775
# Isabelle Gwenca Fong, TP077838

## DATA IMPORT  ##
# Install rstudioapi #
install.packages("rstudioapi")
library(rstudioapi)

# Set working environment #
CSV_FILE <- "5. credit_risk_classification.csv" # csv file name
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path) # get current directory (where R script is stored)
setwd(current_dir) # Set working directory

# Install packages RUN ONCE ONLY #
required_packages <- c("dplyr", "ggplot2", "tidyverse", "caret", "glmnet")

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
ori_credit_risk_df = credit_risk_df
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
# Leong Huey Chian, TP084911


# Chua Song Wen, TP075130



# Objective 3: To examine how the credit history and existing credits affect the resulting credit class. - Teo Jun Jia TP067775 # 
credit_risk_df_capped_jj = credit_risk_df_capped[,c("credit_history", "existing_credits", "class")]
head(credit_risk_df_capped_jj)

unique(credit_risk_df_capped_jj$credit_history)
unique(credit_risk_df_capped_jj$existing_credits)
unique(credit_risk_df_capped_jj$class)

# Descriptive analysis of credit history and existing credits
credit_history_sum <- credit_risk_df_capped_jj %>% 
  group_by(credit_history) %>% 
  summarise(
    n=n()
  )

existing_credits_sum <- credit_risk_df_capped_jj %>% 
  group_by(existing_credits) %>% 
  summarise(
    n=n()
  )

# Visualize descriptive analysis
ggplot(credit_history_sum, aes(x = credit_history, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Distribution of Credit History", x = "Credit History", y = "Count") +
  theme_minimal()

ggplot(existing_credits_sum, aes(x = existing_credits, y = n)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Distribution of Existing Credits", x = "Existing Credits", y = "Count") +
  theme_minimal()

# Analysis 3-1: Do credit history and existing credits affect credit class?
# test between credit history and credit class using Chi-squared test
contingency_table_credit_history <- table(credit_risk_df_capped_jj$credit_history, credit_risk_df_capped_jj$class)
print(contingency_table_credit_history)
chisq_test <- chisq.test(contingency_table_credit_history)
print(chisq_test)

# visualize for credit history vs class using mosaic plot
mosaicplot(contingency_table_credit_history, 
           main = "Mosaic Plot of Credit History vs Class",
           xlab = "Credit History",
           ylab = "Class",
           color = TRUE)

# test between existing credits and credit class using Fisher's Exact Test
contingency_table_existing_credits <- table(credit_risk_df_capped_jj$existing_credits, credit_risk_df_capped_jj$class)
print(contingency_table_existing_credits)
fisher.test(contingency_table_existing_credits)
mosaicplot(contingency_table_existing_credits,
           main = "Mosaic Plot for Existing Credits vs. Class",
           xlab = "Existing Credits",
           ylab = "Class",
           color = TRUE)

# Analysis 3-2: Which type of credit history is most likely to result in a bad credit class?
# test between credit history and credit class using barplot
ggplot(credit_risk_df_capped_jj, aes(x = credit_history, fill = class)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", title = "Credit History vs Class") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Analysis 3-3: Which type of existing credits is most likely to result in a bad credit class?
# test between existing credits and credit class using bar plot
ggplot(credit_risk_df_capped_jj, aes(x = factor(existing_credits), fill = class)) +
  geom_bar(position = "fill") +
  labs(x = "Existing Credits", y = "Proportion", title = "Proportion of Credit Class by Existing Credits") +
  scale_fill_manual(values = c("bad" = "red", "good" = "green"))

# Analysis 3-4: How different types of existing credits in "all paid" credit history causing bad credit class #
# Filter credit history and existing credits
credit_risk_df_capped_jj_all_paid_bad <- subset(credit_risk_df_capped_jj, 
                                            credit_history == 'all paid' & class == 'bad')

# get count of each existing credits
credit_counts <- table(credit_risk_df_capped_jj_all_paid_bad$existing_credits)

# Create pie chart
pie(credit_counts,
    main = "Existing Credits and Bad Class ('All Paid' credit history)",
    col = rainbow(length(credit_counts)),  
    labels = paste(names(credit_counts), "(", credit_counts, ")", sep = "")
)

# Analysis 3-5: Which predictors in credit history and existing credits are significant in affecting credit class 
# Using logistic regression
# convert class to binary (0/1)
credit_risk_df_capped_jj$class <- recode(credit_risk_df_capped_jj$class, 'bad'=0, 'good'=1)
# convert existing credits to factor
credit_risk_df_capped_jj$existing_credits <- as.factor(credit_risk_df_capped_jj$existing_credits)
# generate logistic regression model
credits_model <- glm(class ~ credit_history + existing_credits, data = credit_risk_df_capped_jj, family = binomial)
summary(credits_model)

# visualize logistic regression model using coefficient plot
model_coeffs <- tidy(credits_model, conf.int = TRUE)
ggplot(model_coeffs, aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high)) +
  geom_point() +
  geom_errorbarh(height = 0.2) +
  theme_minimal() +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed")+
  labs(title = "Logistic Regression Coefficients for the credit history and existing credits", x = "Coefficient Estimate", y = "Variable")

# Analysis 3-6: How accurate is the results predicted from the logistic regression model
# test by making prediction using the model
credits_model_predictions <- predict(credits_model, type = "response")
credits_model_predicted_class <- ifelse(credits_model_predictions > 0.5, 1, 0)

# get the actual values
actual_credit_class <- as.numeric(credit_risk_df_capped_jj$class)

# generate confusion matrix
confusion_matrix <- table(Predicted = credits_model_predicted_class, Actual = actual_credit_class)
confusion_matrix

# evaluation of confusion matrix
library(caret)
sensitivity(as.factor(credit_risk_df_capped_jj$class), as.factor(credits_model_predicted_class)) # Calculate true positive
specificity(as.factor(credit_risk_df_capped_jj$class), as.factor(credits_model_predicted_class)) # Calculate true negative
(sum(confusion_matrix) - sum(diag(confusion_matrix)))/sum(confusion_matrix) # calculate misclassification rate

# visualize the performance
credit_risk_df_capped_jj$predicted_probabilities <- credits_model_predictions
ggplot(credit_risk_df_capped_jj, aes(x = predicted_probabilities, fill = as.factor(class))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Predicted Probabilities by Class",
       x = "Predicted Probability of Good Credit Class",
       y = "Density",
       fill = "Actual Class") +
  scale_fill_manual(values = c("red", "blue"), labels = c("Bad", "Good")) +
  theme_minimal()


# Isabelle Gwenca Fong, TP077838


