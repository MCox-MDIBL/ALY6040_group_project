# load libraries
library(tidyverse)
library(janitor)
library(ggplot2)
library(e1071)
library(caret)
library(pROC)
library(factoextra)

# import data
hr_data <- read_csv(
  "Course Work/ALY 6040 - Data Mining Applications/Group Project/Data/HR Data.xlsx - HR data.csv" # insert your filepath here
)

# preview data
head(hr_data)

# check data types
str(hr_data)

# check for missing values (none found)
sum(is.na(hr_data))

# remove unnecessary/redundant columns
hr_data <- hr_data |>
  select(-`0`, -`-2`, -`emp no`, -`CF_attrition label`, -`Employee Number`, 
         -`CF_age band`, -`Employee Count`, -`Over18`)

# standardize column names
names(hr_data) <- gsub("`", "", names(hr_data))   # Remove backticks
names(hr_data) <- gsub(" ", "_", names(hr_data))  # Replace spaces with underscores
names(hr_data) <- tolower(names(hr_data))         # Convert to lowercase

names(hr_data)

### define ordinal numeric variables as factors (optional, may not work well w/ mining techniques)
#hr_data <- hr_data |>
#  mutate(environment_satisfaction = factor(
#    environment_satisfaction,
#    levels = c(1, 2, 3, 4),
#    ordered = TRUE
#  )) |>
# mutate(job_involvement = factor(
#    job_involvement,
#    levels = c(1, 2, 3, 4),
#    ordered = TRUE
#  )) |> 
#  mutate(job_level = factor(
#    job_level,
#    levels = c(1, 2, 3, 4),
#    ordered = TRUE
#  )) |> 
#  mutate(job_satisfaction = factor(
#    job_satisfaction,
#   levels = c(1, 2, 3, 4),
#    ordered = TRUE
#  )) |> 
#  mutate(performance_rating = factor(
#    performance_rating,
#    levels = c(1, 2, 3, 4),
#    ordered = TRUE
#  )) |> 
#  mutate(relationship_satisfaction = factor(
#    relationship_satisfaction,
#    levels = c(1, 2, 3, 4),
#    ordered = TRUE
#  )) |> 
#  mutate(stock_option_level = factor(
#    stock_option_level,
#    levels = c(0, 1, 2, 3),
#    ordered = TRUE
#  )) |> 
# mutate(work_life_balance = factor(
#    work_life_balance,
#    levels = c(1, 2, 3, 4),
#    ordered = TRUE
#  ))
