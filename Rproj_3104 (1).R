install.packages("ggplot2")
library(ggplot2)
library(dplyr)
library(tidyr)

#loading the Dataset
hrm<-read.csv('C:\\Users\\Comp\\Desktop\\New folder (3)\\Book1.csv',
              header = T, stringsAsFactors = F)
str(hrm)

hrm$left <- as.factor(hrm$left)
hrm$salary <- as.factor(hrm$salary)
hrm$sales <- as.factor(hrm$sales)
hrm$work_accident <- as.factor(hrm$work_accident)
hrm$promotion_last_5years <- as.factor(hrm$promotion_last_5years)

colSums((is.na(hrm)))


summary(hrm)
#Satisfaction level statistics splitted by salary ranges
by(hrm$satisfaction_level,hrm$salary,summary)
#Histogram
p1<-ggplot(aes(x=satisfaction_level),data=hrm) +
  geom_histogram(color="black",fill="red",bins = 30) +
  labs(title="Satisfaction level Histogram",x='Satisfaction Level of
Employees', y="Frequency")
p1

p2 = p1 + facet_wrap(~salary)
p2

by(hrm$satisfaction_level,hrm$number_project,summary)
p3 = p1 + facet_wrap(~number_project)
p3

by(hrm$satisfaction_level,hrm$left,summary)
p4 = p1 + facet_wrap(~left)
p4

#Boxplot for Satisfaction level vs left
ggplot(aes(x = left,y=satisfaction_level),data= hrm) +
  geom_boxplot() +
  ylab('Satisfaction Level') +
  xlab("Employee left") +
  labs(fill="Salary Classes")

#Boxplot for Satisfaction level vs left facetted by Salary Ranges
ggplot(aes(x = left,y=satisfaction_level),data= hrm) +
  geom_boxplot() +
  ylab('Satisfaction Level') +
  xlab("Employee left") +
  facet_wrap(~salary)

table(hrm$left , hrm$salary)
table(hrm$left , hrm$number_project)

#Analysis on number of Projects
hrm$number_project<-factor(hrm$number_project)
ggplot(aes(x=number_project),data = hrm) +
  geom_bar(color='black',fill='#234338') +
  xlab("Number of Projects") +
  ylab("Frequency") +
  labs(title="Barplot of Number of projects")

#boxplot of number of projects vs Average monthly hours at workplace of
#employees
p3=ggplot(aes(x=number_project, y = average_monthly_hours),data=hrm)+
  geom_boxplot()
p3
p4=p3+facet_wrap(~salary)
p4

#facetted by salary
ggplot(aes(x=number_project),data = hrm) +
  geom_bar(color='black',fill='#834338') +
  xlab("Number of Projects") +
  ylab("Frequency") +
  labs(title="Barplot of Number of projects faceted by Salary") +
  facet_wrap(~salary)

#faceted by If a employee left or not
ggplot(aes(x=number_project),data = hrm) +
  geom_bar(color='black',fill='#547398') +
  xlab("Number of Projects") +
  ylab("Frequency") +
  labs(title="Barplot of Number of projects faceted by Left")+
  facet_wrap(~left)

cor.test(hrm$satisfaction_level,hrm$average_monthly_hours)

ggplot(aes(x = average_monthly_hours),data =hrm ) +
  geom_histogram(color='black',fill='#443332',bins = 30) +
  facet_wrap(~left)

by(hrm$average_monthly_hours , hrm$left ,summary)
ggplot(aes(y = average_monthly_hours, x = hrm$left),data=hrm)+
  geom_boxplot() +
  xlab("Employee left or not") +
  ylab("Average Montly hours worked")
#A thing to notice is that employee who left the company worked more hours
#than those who did not leave, hence it might be possible that they left bacause
#they were over pressurized by their peers or bosses or over worked or stressed
#with lots of work

#Analysis for variable Time spend at company
table(hrm$time_spend_company)

ggplot(aes(x = factor(time_spend_company)),data = hrm) +
  geom_bar(fill = 'purple',color='black') +
  xlab("Time spend at compnay in years") +
  ylab("Frequency")+
  labs(title = "Barplot of Time spend at Company")

#Time spend at company vs Left or not
ggplot(aes(x = factor(time_spend_company)),data = hrm) +
  geom_bar(fill = 'grey',color='black') +
  xlab("Time spend at compnay in years") +
  ylab("Frequency")+
  labs(title = "Barplot of Time spend at Company faceted by Left") +
  facet_wrap(~left)

#Time Spend at company vs Satisfaaction level
by(hrm$satisfaction_level,factor(hrm$time_spend_company),summary)
cor.test(hrm$satisfaction_level,hrm$time_spend_company)
table(hrm$left,hrm$time_spend_company)

#Time spend at compnay vs Promotion in last 5 years
table(Promotion=hrm$promotion_last_5years,Time_Spend=factor
      (hrm$time_spend_company))



#Employees who have had promotion are very less
ggplot(aes(x = factor(time_spend_company)),data = hrm)+
  geom_bar()+
  facet_wrap(~promotion_last_5years) +
  scale_y_continuous(limits=c(0,4000),breaks=seq(0,4000,500))

table(hrm$salary,hrm$promotion_last_5years)

#Time spend vs Department of Work
by(hrm$time_spend_company,hrm$sales,summary)

ggplot(aes(x =sales),data = hrm ) +
  geom_bar() +
  xlab('Department') +
  ylab('Counts') +
  coord_flip() 

ggplot(aes(x =sales),data = hrm ) +
  geom_bar(aes(fill=salary)) +
  xlab('Department') +
  ylab('Counts') +
  coord_flip()

# Load necessary libraries
install.packages("caret")
install.packages("glmnet")
install.packages("caTools")
library(caTools)
library(dplyr)
library(caret)
library(glmnet)

# Load the dataset (assuming it's stored in a data frame df)
salary_numeric <- factor(hrm$salary, levels = c("low", "medium", "high"), labels = c(1, 2, 3))
# Preprocess the data: Convert categorical variables to numerical using one-hot encoding
df <- hrm %>%
  mutate(
    work_accident = as.numeric(as.character(work_accident)),
    left = as.numeric(as.character(left)),
    promotion_last_5years = as.numeric(as.character(promotion_last_5years))
  ) %>%
  select(-sales,-work_accident,promotion_last_5years) 

# Define features and target
X_reg <- df %>%
  select(-satisfaction_level)  # Exclude the target variable
y_reg <- df$satisfaction_level

# Split data into training and testing sets
set.seed(42)  # For reproducibility
split <- sample.split(y_reg, SplitRatio = 0.7)
X_train_reg <- X_reg[split, ]
X_test_reg <- X_reg[!split, ]
y_train_reg <- y_reg[split]
y_test_reg <- y_reg[!split]

# Create and train the linear regression model
regression_model <- lm(y_train_reg ~ ., data = data.frame(X_train_reg, y_train_reg))

# Make predictions
y_pred_reg <- predict(regression_model, newdata = data.frame(X_test_reg))

# Evaluate the model
mse_reg <- mean((y_pred_reg - y_test_reg)^2)
r_squared_reg <- 1 - (sum((y_test_reg - y_pred_reg)^2) / sum((y_test_reg - mean(y_test_reg))^2))
print(paste("Mean Squared Error:", round(mse_reg, 2)))
print(paste("R-squared:", round(r_squared_reg, 2)))

# Plot the regression results (optional)
library(ggplot2)
ggplot(data.frame(y_test_reg, y_pred_reg), aes(x = y_test_reg, y = y_pred_reg)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Actual Satisfaction Level", y = "Predicted Satisfaction Level") +
  ggtitle("Linear Regression: Actual vs. Predicted Satisfaction Levels")

str(hrm)

# Load necessary libraries
install.packages("randomForest")
library(randomForest)


# Assuming 'hrm' is your dataset

# Preprocess the dataset: Convert 'number_project' from factor to numeric
hrm$number_project <- as.numeric(as.character(hrm$number_project))

# Split the dataset into training and testing sets
set.seed(42)  # For reproducibility
split <- createDataPartition(hrm$left, p = 0.8, list = FALSE)
train_data <- hrm[split, ]
test_data <- hrm[-split, ]

# Create and train the Random Forest model
rf_model <- randomForest(left ~ ., data = train_data, ntree = 100)

# Make predictions on the test set
predictions <- predict(rf_model, newdata = test_data)
# Evaluate the model
confusion_matrix <- table(test_data$left, predictions)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(confusion_matrix)
print(paste("Accuracy:", round(accuracy, 4)))

