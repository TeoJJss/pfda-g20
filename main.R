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
# Name1, TP000001


# Chua Song Wen, TP075130



# Name3, TP000003



# Name4, TP000004
