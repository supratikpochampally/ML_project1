# Read the Titanic data set
df <- read.csv("titanic_project.csv")

# Data exploration functions
num_survived <- sum(df$survived == 1)
print(paste("Number of passengers that survived =", num_survived))
average_age <- mean(df$age)
print(paste("Average age of passengers =", average_age))

# Visual data exploration (graph) functions
par(mfrow = c(1, 2))
hist(df$age, main = "Histogram of Age", xlab = "Age")
cdplot(as.factor(df$age), as.factor(df$survived), main = "Conditional density plot of Survived vs. Age", xlab = "Age", ylab = "Survived")

# Split into train and test data, with 900 observations used for training and the remaining for testing
train <- df[1:900,]
test <- df[901:1046,]

# Train a logistic regression model on the data and calculate runtime
start_time <- Sys.time()
glm1 <- glm(survived ~ pclass, data=train, family=binomial)
end_time <- Sys.time()

# Print the coefficients of the model
glm1$coefficients

# Test on the test data
probs <- predict(glm1, newdata=test, type="response")
pred <- ifelse(probs > 0.5, 1, 0)
table(pred, test$survived)

# Calculate metrics for accuracy, sensitivity, and specificity
acc <- mean(pred==test$survived)
sensitivity <- (67) / (67 + 12)
specificity <- (31) / (31 + 36)

# Print metrics for accuracy, sensitivity, and specificity
print(paste("Accuracy =", acc))
print(paste("Sensitivity =", sensitivity))
print(paste("Specificity =", specificity))

# Calculate and print the run time
run_time = end_time - start_time
print(paste("Run time =", run_time))


