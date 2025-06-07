# Setting the working directory
setwd("D:\\Teaching\\SCMA632 2025 C51\\Assignments\\R")

# Check whether we successfully are in the working directory
getwd() # after running this line we are sure that working direcory path is correct

# 1. Installing and Importing Necessary libraries ####

# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load required libraries
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA") #vector
lapply(libraries, install_and_load)

# 2. Reading the dataset into R ####

# Option 1: If the file is in the current working directory (WD), load it directly
data <- read.csv("../Data/NSSO68.csv")

# Option 2: If the file is NOT in the working directory, use the full file path instead
# Example: Replace the path below with the actual location of your CSV file
# data <- read.csv("C:/Users/YourUsername/Documents/NSSO68.csv")

# 3. Filtering data for a specific state and writing the file to a path ####
# Replace "AP" with the desired state abbreviation (e.g., "MH", "KA") , 
# Note please check the pdf file for state code in github

state_name <- "AP"
state_data <- data %>%
  filter(state_1 == state_name)

unique(data$state_1)
unique(state_data$state_1)

# write.csv(data, 'path') 
write.csv(state_data, '../Data/state_filtered_data.csv')

#optional reading the filtered data
state_data = read.csv('../Data/state_filtered_data.csv')

# 4. Display dataset information ####
cat("Dataset Information:\n")
print(names(state_data))
print(head(state_data))
print(dim(state_data))

sum(is.na(state_data))

# 5. Check for missing values ####
missing_info <- colSums(is.na(state_data))
cat("Missing Values Information:\n")
print(missing_info)

# 6. Select relevant columns for analysis ####
state_subset <- state_data %>%
  select(state_1, District, Region, Sector, State_Region,
         Meals_At_Home, ricetotal_v, wheattotal_v, Milktotal_v,
         pulsestot_v,nonvegtotal_v, fruitstt_v, No_of_Meals_per_day)
names(state_data)
# 7. Impute missing values with mean ####

impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}

missing_info <- colSums(is.na(state_subset))
cat("Missing Values Information:\n")
print(missing_info)

# Meals_At_Home var has 122 missing values , lets impute
state_subset$Meals_At_Home <- impute_with_mean(state_subset$Meals_At_Home)

missing_info <- colSums(is.na(state_subset))
cat("Missing Values Information:\n")
print(missing_info)


# 8. Remove outliers from specific columns ####
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  df <- subset(df, df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold)
  return(df)
}


outlier_columns <- c('Meals_At_Home', 'ricetotal_v', 'wheattotal_v', 'Milktotal_v',
                     'pulsestot_v','nonvegtotal_v', 'fruitstt_v', 'No_of_Meals_per_day')
for (col in outlier_columns) {
  state_subset <- remove_outliers(state_subset, col)
}



names(state_subset)
# 9. Create total consumption variable ####
state_subset$total_consumption <- rowSums(state_subset[, c('ricetotal_v', 'wheattotal_v', 'Milktotal_v',
                                                           'pulsestot_v','nonvegtotal_v', 'fruitstt_v')], na.rm = TRUE)

# 10. Summarize consumption by district and region ####
summarize_consumption <- function(group_col) {
  summary <- state_subset %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption)) %>%
    arrange(desc(total))
  return(summary)
}

district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")
sector_summary <- summarize_consumption("Sector")

cat("Top Consuming Districts:\n")
print(head(district_summary, 4))
cat("Region Consumption Summary:\n")
print(region_summary)
cat("Sector Consumption Summary:\n")
print(sector_summary)

cat("Bottom Consuming Districts:\n")
print(tail(district_summary, 4))
cat("Region Consumption Summary:\n")
print(region_summary)
cat("Sector Consumption Summary:\n")
print(sector_summary)

# 11. Rename district and sector codes ####
# Refer the District-codes.pdf in github for getting district codes
district_mapping <- c("23" = "Chittoor", "6" = "Rangareddi", "5" = "Hyderabad and Rangareddi",
                      "17" = "Guntur",'1'='Adilabad', '12'='Vizinagaram',
                      '11'='Srikakulam','2'='Nizamabad')
                      
# sector (rural-1, urban-2) offical documentation
sector_mapping <- c("2" = "URBAN", "1" = "RURAL")

state_subset$District <- as.character(state_subset$District)
state_subset$Sector <- as.character(state_subset$Sector)
state_subset$District <- ifelse(state_subset$District %in% names(district_mapping),
                                district_mapping[state_subset$District],
                                state_subset$District)
state_subset$Sector <- ifelse(state_subset$Sector %in% names(sector_mapping),
                              sector_mapping[state_subset$Sector],
                              state_subset$Sector)

district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")
sector_summary <- summarize_consumption("Sector")


cat("Top Consuming Districts:\n")
print(head(district_summary, 4))
cat("Region Consumption Summary:\n")
print(region_summary)
cat("Sector Consumption Summary:\n")
print(sector_summary)


cat("Top Consuming Districts:\n")
print(tail(district_summary, 4))
cat("Region Consumption Summary:\n")
print(region_summary)
cat("Sector Consumption Summary:\n")
print(sector_summary)


# 12. Test for mean difference between Urban and Rural consumption ####
rural <- state_subset %>%
  filter(Sector == "RURAL") %>%
  select(total_consumption)

urban <- state_subset %>%
  filter(Sector == "URBAN") %>%
  select(total_consumption)

# Perform z-test (requires BSDA package)
library(BSDA)
z_test_result <- z.test(rural, urban, alternative = "two.sided",
                        mu = 0, sigma.x = 2.56, sigma.y = 2.34, conf.level = 0.95)

# Report test result
if (z_test_result$p.value < 0.05) {
  cat("P value is <", 0.05, ", Therefore we reject the null hypothesis.\n")
  cat("There is a difference between mean consumptions of urban and rural.\n")
} else {
  cat("P value is >=", 0.05, ", Therefore we fail to reject the null hypothesis.\n")
  cat("There is no significant difference between mean consumptions of urban and rural.\n")
}


# 13. Test for mean difference between Bottom and Top consumption ####
top_district <- state_subset %>%
  filter(District == "Hyderabad and Rangareddi") %>%
  select(total_consumption)

bottom_district <- state_subset %>%
  filter(District == "Nizamabad") %>%
  select(total_consumption)

# Perform z-test (requires BSDA package)
library(BSDA)
z_test_result <- z.test(top_district, bottom_district, alternative = "two.sided",
                        mu = 0, sigma.x = 2.56, sigma.y = 2.34, conf.level = 0.95)

# Report test result
if (z_test_result$p.value < 0.05) {
  cat("P value is <", 0.05, ", Therefore we reject the null hypothesis.\n")
  cat("There is a difference between mean consumptions of top and bottom districts of andhrapradesh.\n")
} else {
  cat("P value is >=", 0.05, ", Therefore we fail to reject the null hypothesis.\n")
  cat("There is no significant difference between mean consumptions of top and bottom districts of andhrapradesh.\n")
}



