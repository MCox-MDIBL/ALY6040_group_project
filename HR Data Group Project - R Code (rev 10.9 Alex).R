# load libraries
library(pacman)
p_load(tidyverse)
p_load(janitor)
p_load(knitr)
p_load(ggplot2)
p_load(e1071)
p_load(caret)
p_load(pROC)
p_load(factoextra)
p_load(corrplot)
p_load(rpart)
p_load(rpart.plot)
p_load(randomForest)


# import data
hr_data <- read_csv("HR Data.xlsx - HR data.csv" # insert your filepath here
)

# preview data
head(hr_data)

# check data types
str(hr_data)

# check for missing values and blank entries (none found)
sum(is.na(hr_data))
colSums(hr_data == "")

# remove unnecessary/redundant columns
hr_data <- hr_data |>
  select(-`0`, -`-2`, -`emp no`, -`CF_attrition label`, -`Employee Number`, 
         -`CF_age band`, -`Employee Count`, -`Over18`, -`Standard Hours`)

# Remove percent_salary_hike to reduce overfitting of models
hr_data <- hr_data |> select(-`Percent Salary Hike`)

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



#####################
#
#  Basic exploration 
#
#####################

# Specific variables are required to be factors, namely the variable we're predicting

# Defining variable types for ease of modeling
exclude_cols <- c(
  "age",
  "daily_rate",
  "distance_from_home",
  "hourly_rate",
  "monthly_income",
  "monthly_rate",
  #"percent_salary_hike",
  "total_working_years",
  "years_at_company",
  "years_in_current_role",
  "years_since_last_promotion",
  "years_with_curr_manager"
)
# Convert select columns to factors fro NB categorical variable assumption
nb_hr_data <- hr_data |> 
  mutate(across(-all_of(exclude_cols), as.factor))



# Cramer's V for categorical variables
library(vcd)

# Function for Cramer's V
cramers_v <- function(x, y) {
  tab <- table(x, y)
  res <- assocstats(tab)
  return(res$cramer)
}

# Identify the target variable and the other categorical variables
target_variable <- "performance_rating"
other_variables <- setdiff(names(hr_data), target_variable)  # Get other variable names

# Create a data frame to store the results
crv_results_df <- data.frame(Variable = character(), Cramers_V = numeric(), stringsAsFactors = FALSE)

# Calculate Cramér's V for each categorical variable against performance_rating
for (var in other_variables) {
  if (is.factor(hr_data[[var]]) || is.character(hr_data[[var]])) {
    v <- cramers_v(hr_data[[target_variable]], hr_data[[var]])
    crv_results_df <- rbind(crv_results_df, data.frame(Variable = var, Cramers_V = v))
  }
}

# Tabulate associations between categorical variables and the variable of interest (performance_rating)
# 0 - 1 : low = no association, high = more association
kable(crv_results_df, format = "rst")


#####################
#
# Numerical correlation
#
#####################

# Kruskal-Wallis test

# Define the numerical variables you want to test
numerical_variables <- c("age", 
                         "daily_rate", 
                         "distance_from_home", 
                         "hourly_rate", 
                         "monthly_income",
                         "monthly_rate",
                         #"percent_salary_hike",
                         "total_working_years",
                         "years_at_company",
                         "years_in_current_role",
                         "years_since_last_promotion",
                         "years_with_curr_manager")

# Create a list to store results
kruskal_results <- list()

# Loop through each numerical variable and perform Kruskal-Wallis test
for (var in numerical_variables) {
  test_result <- kruskal.test(as.formula(paste(var, "~ performance_rating")), data = nb_hr_data)
  kruskal_results[[var]] <- test_result
}

# Print the results
kruskal_results




#####################
#
# Basic visualizations
#
#####################

ggplot(balanced_data, aes(x = performance_rating)) +
  geom_bar(fill = c("darkgreen", "red")) +
  labs(x = "Performance Rating", 
       y = "Count", 
       title = "Distribution of Performance Rating - Balanced") +
  theme(plot.title = element_text(hjust = 0.5))
  





#####################
#
# Naive Bayes 
#
#####################


# Splitting the dataset
set.seed(123)
nb_train_index <- createDataPartition(nb_hr_data$performance_rating, p = 0.8, list = FALSE)
nb_train_data <- nb_hr_data[nb_train_index, ]
nb_test_data <- nb_hr_data[-nb_train_index, ]

# Naive Bayes model
nb_model <- naiveBayes(performance_rating ~ ., data = nb_train_data)

# Predict on the test set
nb_predictions <- predict(nb_model, newdata = nb_test_data)

# Confusion matrix
nb_results <- confusionMatrix(nb_predictions, nb_test_data$performance_rating)

#Calculating accuracy
nb_cm <- table(nb_test_data$performance_rating, nb_predictions) 
confusionMatrix(nb_cm)


#####################
#
# Decision Tree
#
#####################

dt_hr_data <- nb_hr_data

# Splitting the dataset
set.seed(123)
dt_train_index <- createDataPartition(dt_hr_data$performance_rating, p = 0.8, list = FALSE)
dt_train_data <- dt_hr_data[dt_train_index, ]
dt_test_data <- dt_hr_data[-dt_train_index, ]



# Train a decision tree model
dt_model <- rpart(performance_rating ~ ., data = dt_train_data, method = "class")

# Predict on the test set
dt_predictions <- predict(dt_model, newdata = dt_test_data, type = "class")

# Confusion matrix
confusionMatrix(dt_predictions, dt_test_data$performance_rating)

# Build tree diagram
rpart.plot(dt_model, nn=TRUE)

# Check inportant variables
# Broken on imbalanced datasets
#varImp(dt_model)


###############
#
# Troubleshooting Decision tree
# Decision tree can often be impacted by overfitting, which is what the percent_salary_hike variable is doing
#
###############

# Check distribution of percent_salary_hike by performance_rating
#boxplot(percent_salary_hike ~ performance_rating, data = hr_data)

# Cross-validate the decision tree model
control <- trainControl(method = "cv", number = 10)
dt_cv_model <- train(performance_rating ~ ., data = dt_train_data, method = "rpart", trControl = control)
# Check results
dt_cv_model


#####################
#
# Random Forest
# Improving upon the decision tree model
#
#####################


rf_hr_data <- nb_hr_data

# Splitting the dataset
set.seed(123)
rf_train_index <- createDataPartition(rf_hr_data$performance_rating, p = 0.8, list = FALSE)
rf_train_data <- rf_hr_data[rf_train_index, ]
rf_test_data <- rf_hr_data[-rf_train_index, ]

# Train a random forest model
rf_model <- randomForest(performance_rating ~ ., data = rf_train_data, ntree = 500) # Build 500 trees

# Predict on test data
rf_predictions <- predict(rf_model, newdata = rf_test_data)

# Evaluate performance
confusionMatrix(rf_predictions, rf_test_data$performance_rating)

# Check variable importance
importance(rf_model)

###############
#
# Troubleshooting Random forest
# Overfitting is still an issue when percent_salary_hike is included
# Predictions are wildly skewed when percent_salary_hike is excluded, possibly due to class imbalance?
#
###############


# Set up cross-validation
rf_control <- trainControl(method = "cv", number = 10)

# Train random forest with cross-validation
rf_cv_model <- train(performance_rating ~ ., data = rf_train_data, method = "rf", trControl = rf_control)

# Check cross-validation results
rf_cv_model


#########################################################################################
#
# Attempting to balance classes
# Random Upsampling to create more minority class entries (performance_rating = 4)
#
#########################################################################################

# Class distribution may lead to biased modeling
table(nb_hr_data$performance_rating)


# Create a balanced dataset using random oversampling
balanced_data <- upSample(nb_hr_data[, -ncol(nb_hr_data)], nb_hr_data$performance_rating)

# Check for balance
table(balanced_data$performance_rating)




#####################
#
# Naive Bayes - balanced
#
#####################

nb_balanced_hr_data <- balanced_data |> select(-Class) # Aberant column added which doubles the performance_rating column

# Splitting the dataset
set.seed(123)
nb_balanced_train_index <- createDataPartition(nb_balanced_hr_data$performance_rating, p = 0.8, list = FALSE)
nb_balanced_train_data <- nb_balanced_hr_data[nb_balanced_train_index, ]
nb_balanced_test_data <- nb_balanced_hr_data[-nb_balanced_train_index, ]

# Naive Bayes model
nb_balanced_model <- naiveBayes(performance_rating ~ ., data = nb_balanced_train_data)

# Predict on the test set
nb_balanced_predictions <- predict(nb_balanced_model, newdata = nb_balanced_test_data)

# Confusion matrix
nb_balanced_results <- confusionMatrix(nb_balanced_predictions, nb_balanced_test_data$performance_rating)

#Calculating accuracy
nb_balanced_cm <- table(nb_balanced_test_data$performance_rating, nb_balanced_predictions) 
confusionMatrix(nb_balanced_cm)


#####################
#
# Decision Tree - balanced
#
#####################

dt_balanced_hr_data <- nb_balanced_hr_data

# Splitting the dataset
set.seed(123)
dt_balanced_train_index <- createDataPartition(dt_balanced_hr_data$performance_rating, p = 0.8, list = FALSE)
dt_balanced_train_data <- dt_balanced_hr_data[dt_balanced_train_index, ]
dt_balanced_test_data <- dt_balanced_hr_data[-dt_balanced_train_index, ]



# Train a decision tree model
dt_balanced_model <- rpart(performance_rating ~ ., data = dt_balanced_train_data, method = "class")

# Predict on the test set
dt_balanced_predictions <- predict(dt_balanced_model, newdata = dt_balanced_test_data, type = "class")

# Confusion matrix
confusionMatrix(dt_balanced_predictions, dt_balanced_test_data$performance_rating)

# Build tree diagram
rpart.plot(dt_balanced_model, nn=TRUE, cex = 0.65)

# Check inportant variables
varImp(dt_balanced_model)


####################
#
# Decision tree visualization
#
####################

library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# Function to convert `rpart` tree into a Graphviz-compatible structure
tree_to_graphviz <- function(model) {
  capture.output(rpart.plot::rpart.plot(model, type = 4, digits = 2, extra = 104), file = NULL)
}

# Capture the tree structure in Graphviz format
graphviz_tree <- tree_to_graphviz(dt_balanced_model)

# Create a DiagrammeR graph object
graph <- DiagrammeR::grViz(graphviz_tree)

# Render the tree
DiagrammeRsvg::export_svg(graph) %>% charToRaw() %>% rsvg::rsvg_png("decision_tree_diagram.png")




###############
#
# Troubleshooting Decision tree - balanced
# Decision tree can often be impacted by overfitting, which is what the percent_salary_hike variable is doing
#
###############

# Cross-validate the decision tree model
control_balanced <- trainControl(method = "cv", number = 10)
dt_balanced_cv_model <- train(performance_rating ~ ., data = dt_balanced_train_data, method = "rpart", trControl = control_balanced)
# Check results
dt_balanced_cv_model


#####################
#
# Random Forest - balanced
# Improving upon the decision tree model
#
#####################


rf_balanced_hr_data <- nb_balanced_hr_data

# Splitting the dataset
set.seed(123)
rf_balanced_train_index <- createDataPartition(rf_balanced_hr_data$performance_rating, p = 0.8, list = FALSE)
rf_balanced_train_data <- rf_balanced_hr_data[rf_balanced_train_index, ]
rf_balanced_test_data <- rf_balanced_hr_data[-rf_balanced_train_index, ]

# Train a random forest model
rf_balanced_model <- randomForest(performance_rating ~ ., data = rf_balanced_train_data, ntree = 500, localImp = TRUE) # Build 500 trees

# Predict on test data
rf_balanced_predictions <- predict(rf_balanced_model, newdata = rf_balanced_test_data)

# Evaluate performance
confusionMatrix(rf_balanced_predictions, rf_balanced_test_data$performance_rating)

# Check variable importance
importance(rf_balanced_model)


####################
#
# random forest Decision tree visualization
#
####################

# Load the package
p_load(randomForestExplainer)

# Plot the most important trees from the forest
explain_forest(rf_balanced_model, interactions = TRUE, data = rf_balanced_hr_data)










###############
#
# Troubleshooting Random forest - balanced
#
###############


# Set up cross-validation
rf_balanced_control <- trainControl(method = "cv", number = 10)

# Train random forest with cross-validation
rf_balanced_cv_model <- train(performance_rating ~ ., data = rf_balanced_train_data, method = "rf", trControl = rf_balanced_control)

# Check cross-validation results
rf_balanced_cv_model




