# Load required packages
library(dplyr)
library(ggplot2)
library(tidyr)

#1 Load the dataset
setwd("C:/Users/dhara/GBC/R_Assignment_4066/")
dataset <- read.csv("ds_salaries.csv")

#2 Print the structure of the dataset
str(dataset)

#3 List the variables in the dataset
variables <- names(dataset)
print(variables)

#4 Print the top 15 rows of the dataset
head(dataset, n=15)

#5 Making a dataframe
df_salaries <- data.frame(dataset)

#6 User-defined function using a variable from the dataset
myFunction <- function(df, var) {
  return(max(df[[var]]))
}
myFunction(df_salaries, "salary_in_usd")

fun1 <- function(df, var) {
  return(unique(df[[var]]))
}
fun1(df_salaries, "job_title")

#7 Filter rows based on logical criteria
filtered_data <- df_salaries %>% filter(job_title == "Data Analyst", company_size=="L")
filtered_data

#8 Identify dependent and independent variables and create a new data frame
dependent_var <- "salary"
independent_vars <- c("job_title", "experience_level", "company_size")

dependent_df <- df_salaries[c("salary", dependent_var)]
independent_df <- df_salaries[c("salary", independent_vars)]

reshaped_df <- reshape(
  independent_df,
  idvar = "id",
  varying = list(independent_vars),
  v.names = "value",
  timevar = "variable",
  times = independent_vars,
  direction = "long"
)

merged_df <- merge(dependent_df, reshaped_df, by = "salary")
print(merged_df)


#9 Remove missing values
dataset <- na.omit(dataset)

#10 Identify and Remove duplicated data
duplicate_values <- duplicated(dataset)
duplicate_values
dataset <- distinct(dataset)

#11 Reorder rows in descending order
dataset %>% arrange(desc(salary) , desc(salary_in_usd))

#12 Rename column names
dataset <- dataset %>% rename(currency = salary_currency)
dataset

#13 Add new variables using a mathematical function
dataset <- dataset %>% mutate(Biweekly_salary = salary / 26)
dataset

#14 Create a training set using random number generator engine
set.seed(123)
n_train <- 3
train_indices <- sample(1:nrow(df_salaries), n_train)
train_set <- df_salaries[train_indices, ]
print(train_set)

#15 Summary statistics
summary(dataset)


#16 Statistical functions on numerical variables
mean_value <- mean(dataset$salary)
median_value <- median(dataset$salary)
mode_value <- table(dataset$salary)[which.max(table(dataset$salary))]
range_value <- range(dataset$salary)

print(paste0("mean: ", mean_value))
print(paste0("median: ", median_value))
print(paste0("mode: ", mode_value))
print(paste0("range: ", range_value))

#17 Scatter plot
ggplot(dataset, aes(x = experience_level, y = salary)) +
  geom_point()+
  labs(x = "Level of experience", y = "Salary", title = "Scatter Plot")

#18 Bar plot
ggplot(dataset, aes(x = company_size, fill = remote_ratio)) +
  geom_bar(fill = "aquamarine4")+
  labs(x = "Company Size", y = "Remote ration", title = "Bar Plot")

#19 Correlation between two variables using linear regression
ggplot(dataset, aes(x = work_year, y = salary)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Year", y = "Salary", title = "Linear Regression Correlation Plot")


