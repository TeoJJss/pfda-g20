## DATA IMPORT  ##
# Set working environment #
CSV_FILE <- "5. credit_risk_classification.csv" # csv file name
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path) # get current directory (where R script is stored)
setwd(current_dir) # Set working directory

# Install packages dynamically RUN ONCE ONLY #
required_packages <- c("dplyr", "ggplot2", "tidyverse")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) { # If the package is not available
    install.packages(pkg) # Install the package
  }
}
 
# Read CSV file and save to data frame #
library(readr) # load from tidyverse
credit_risk_df <- read_csv(CSV_FILE) # load data frame
head(credit_risk_df) # check data frame


## DATA CLEANING / PRE-PROCESSING  ##
# Remove unnecessary columns #
selected_cols = c(
  "purpose", "employment", "property_magnitude", "age", "job", 
  "credit_history", "installment_commitment", "existing_credits"
  ) # columns required
credit_risk_df = credit_risk_df[, selected_cols] # select required
head(credit_risk_df)


