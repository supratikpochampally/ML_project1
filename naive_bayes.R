library(e1071)

# Read the Titanic data set
df <- read.csv("titanic_project.csv")

# Data exploration functions
num_males <- sum(df$sex == 1)
print(paste("Number of passengers that are male =", num_males))
num_females <- sum(df$sex == 0)
print(paste("Number of passengers that are female =", num_females))

# Visual data exploration (graph) functions
par(mfrow = c(1, 2))
hist(df$pclass, main = "Histogram of Passenger Class", xlab = "Passenger Class")
cdplot(as.factor(df$pclass), as.factor(df$survived), main = "Conditional density plot of Survived vs. Passenger Class", xlab = "Passenger Class", ylab = "Survived")

# Factor sex, pclass, and survived
df$sex <- factor(df$sex)
df$pclass <- factor(df$pclass)
df$survived <- factor(df$survived)

# Split into train and test data, with 900 observations used for training and the remaining for testing
train <- df[1:900,]
test <- df[901:1046,]

# Train a naive Bayes model on the data and calculate runtime
start_time <- Sys.time()
nb1 <- naiveBayes(survived~pclass+sex+age, data=train)
end_time <- Sys.time()

# Print the model and probabilities learned from the data
nb1

# Calculate metrics for accuracy, sensitivity, and specificity
probs <- predict(nb1, newdata = test, type = "raw")
probs <- probs[, 2]
pred <- ifelse(probs > 0.5, 1, 0)
table(pred, test$survived)

acc <- (69 + 42) / (69 + 42 + 25 + 10)
sensitivity <- (69) / (69 + 10)
specificity <- (42) / (42 + 25)

# Print metrics for accuracy, sensitivity, and specificity
print(paste("Accuracy =", acc))
print(paste("Sensitivity =", sensitivity))
print(paste("Specificity =", specificity))

# Calculate and print the run time
run_time = end_time - start_time
print(paste("Run time =", run_time))

