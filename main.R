# Leong Huey Chian, TP084911
# Chua Song Wen, TP075130
# Teo Jun Jia, TP067775
# Isabelle Gwenca Fong, TP077838

## DATA IMPORT  ##
# Install rstudioapi (DO ONCE ONLY) #
# install.packages("rstudioapi")
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
# Leong Huey Chian, TP084911 #

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

#Logistics Regression test
job_log_model<-glm(class_binary~job,family = binomial,data = credit_risk_df_capped_leong)

#predict the job percentage using the result of logistics regression test
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


# Chua Song Wen, TP075130 #
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


# Teo Jun Jia TP067775 # 
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
library(broom)
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


# Isabelle Gwenca Fong, TP077838 #
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
  tally() %>%
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


## GROUP HYPOTHESIS ##
# create new data set with all the variables in the group hypothesis
age_above_35 <- select(credit_risk_df_capped_isabelle, age_above_35)
real_estate <- select(credit_risk_df_capped_isabelle, real_estate)
credit_history <- select(credit_risk_df_capped_jj, credit_history)
existing_credits <- select(credit_risk_df_capped_jj, existing_credits)
installment_commitment <- select(credit_risk_df_capped_leong, installment_commitment)
purpose_MediumConsumptionPower <- select(credit_risk_df_capped_sw, purpose_MediumConsumptionPower)
employment_LongTerm <- select(credit_risk_df_capped_sw, employment_LongTerm)
class <- select(credit_risk_df_capped_isabelle, class)

credit_risk_df_group = cbind(age_above_35, real_estate, credit_history, existing_credits, installment_commitment, purpose_MediumConsumptionPower, employment_LongTerm, class)

head(credit_risk_df_group)
str(credit_risk_df_group)

credit_risk_df_group$installment_commitment <- as.factor(credit_risk_df_group$installment_commitment)
str(credit_risk_df_group)
credit_risk_df_group$purpose_MediumConsumptionPower <- as.factor(credit_risk_df_group$purpose_MediumConsumptionPower)
credit_risk_df_group$employment_LongTerm <- as.factor(credit_risk_df_group$employment_LongTerm)

levels(credit_risk_df_group$credit_history)
levels(credit_risk_df_group$existing_credits)
levels(credit_risk_df_group$installment_commitment)

# recode credit_history into two levels: "All Paid" or "Not All Paid"
credit_risk_df_group$credit_history <- ifelse(credit_risk_df_group$credit_history %in% c("all paid", "existing paid"), "All Paid", "Not All Paid")
credit_risk_df_group$credit_history <- factor(credit_risk_df_group$credit_history, levels = c("Not All Paid", "All Paid"))

# recode installment_commitment to "More than 2" or "Not More than 2"
credit_risk_df_group$installment_commitment <- ifelse(credit_risk_df_group$installment_commitment %in% levels(credit_risk_df_group$installment_commitment)[as.numeric(credit_risk_df_group$installment_commitment) > 2], "More than 2", "Not More than 2")
credit_risk_df_group$installment_commitment <- factor(credit_risk_df_group$installment_commitment, levels = c("Not More than 2", "More than 2"))

# recode existing_credits to "1" or "Not 1"
credit_risk_df_group$existing_credits <- ifelse(credit_risk_df_group$existing_credits == 1, "1", "Not 1")
credit_risk_df_group$existing_credits <- factor(credit_risk_df_group$existing_credits, c("Not 1", "1"))

str(credit_risk_df_group)

# logistic regression model - interaction
group_interaction_logistic_model <- glm(class ~ purpose_MediumConsumptionPower * employment_LongTerm * age_above_35 * real_estate + existing_credits * installment_commitment * credit_history, family = binomial, data = credit_risk_df_group)

summary(group_interaction_logistic_model)



