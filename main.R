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
# Leong Huey Chian, TP084911

#job(character) n  installment_commitment(numeric)

credit_risk_df_capped_leong=credit_risk_df_capped
credit_risk_df_capped_leong$job=recode(credit_risk_df_capped_leong$job,"high qualif/self emp/mgmt"="high-qualified","unskilled resident"="unskilled","unemp/unskilled non res"="unskilled")

#job
#summary of job
nrow(credit_risk_df_capped_leong)
summary(credit_risk_df_capped_leong$job)

#GRAPH
#percentage bar plot 1
a=ggplot(credit_risk_df_capped_leong, aes(x = job, fill = class))
job1=a+geom_bar(position = "fill")+
  labs(title = "Credit Classification by Job Skill Category",
       x="Job Skill Category",
       y="Credit Classification")+
  scale_y_continuous(labels = scales::percent)+
  theme_minimal()
job1

ggsave("Percentage of job skill vs Credit Risk Classification.png",plot = job1,width = 8,height = 6,bg="white")

#ungrouped bar
job2=ggplot(credit_risk_df_capped_leong, aes(x = job, fill = class)) +
  geom_bar(position = "dodge") +
  labs(title = "Credit Classification by Job Skill Category",
       x = "Job Skill Category",
       y = "Number of Customers",
       fill = "Credit Classification") +
  theme_minimal()
ggsave("Bar graph of job skill vs Credit Risk Classification.png",plot = job2,width = 8,height = 6,bg="white")
job2

job_conti_table=table(credit_risk_df_capped_leong$job,credit_risk_df_capped_leong$class)
job_conti_table
job_bad=job_conti_table[,1]
job_bad
job_good=job_conti_table[,2]
job_good


create_pie_chart <- function(data, title) {
  percentages=round(data/sum(data)*100,1)
# Create a pie chart
pie(data, 
      labels = paste(names(data), "\n", percentages,"%"), 
      main = title, 
      col = rainbow(length(data)))
}

# Create pie charts for bad and good job categories
pie_bad=create_pie_chart(job_bad, "Job Categories for Bad Class")
pie_good=create_pie_chart(job_good, "Job Categories for Good Class")
ggsave("Percentage of Job Categories for Good Class.png",plot = pie_good,width = 6,height = 6)
ggsave("Percentage of Job Categories for Bad Class.png",plot = pie_bad,width = 6,height = 6)

#HYPOTHESIS TESTING
#change the class to binary (bad=1 , good=0)
credit_risk_df_capped_leong$class_binary <- ifelse(credit_risk_df_capped_leong$class == "bad", 1, 0)

#Chi-squared test
job_test=chisq.test(job_conti_table)
job_test


#predict probability
job_prob=data.frame(job=unique(credit_risk_df_capped_leong$job))
job_prob


predict_job=predict.glm(job_log_model,newdata = job_prob,type = "response")
job_prob$Predicted_prob=predict_job
print(job_prob)


#INSTALMENT COMMITMENT
summary(credit_risk_df_capped_leong$installment_commitment)#numeric

#CHART
 #bar chart
commit_chart=ggplot(credit_risk_df_capped_leong, aes(x = installment_commitment, fill = class)) +
  geom_bar(position = "dodge", stat = "count", alpha = 0.7) +
  labs(title = "Installment Commitment vs Credit Class",
       x = "Installment Commitment",
       y = "Count") +
  theme_minimal()
ggsave("barchart_commit.png", plot = commit_chart, width = 12, height = 8, dpi = 300, bg = 'white')

#boxplot
insta=ggplot(credit_risk_df_capped_leong, aes(x = class, y = installment_commitment, fill = class)) +
  geom_boxplot() +
  labs(title = "Installment vs Class",
       x = "class",
       y = "Installment") +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "salmon"))
insta
ggsave("boxplot_commit.png", plot = insta, width = 12, height = 8, dpi = 300, bg = 'white')


#histogram
c=ggplot(credit_risk_df_capped_leong,aes(x=installment_commitment,fill=class))
commit_hist=c+geom_histogram(position = "identity",
                             alpha=0.7,
                             bins = 30)+
  labs(title = "Install vs Class",
       x = "Install",
       y = "Class") +
  theme_minimal()
commit_hist
ggsave("histogram_.png", plot = commit_hist, width = 12, height = 8, dpi = 300, bg = 'white')


#Hypothesis Testing

#logistic regression 
logistic_model <- glm(class_binary ~ installment_commitment, data = credit_risk_df_capped_leong, family = binomial)

# Print the summary of the model
summary(logistic_model)









# Chua Song Wen, TP075130



# Teo Jun Jia, TP067775



# Isabelle Gwenca Fong, TP077838
