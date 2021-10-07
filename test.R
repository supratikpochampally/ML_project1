library(e1071)

# Read the Titanic data set
df <- read.csv("titanic_project.csv")

# Factor sex, pclass, and survived
df$sex <- factor(df$sex)
df$pclass <- factor(df$pclass)
df$survived <- factor(df$survived)

# Split into train and test data, with 900 observations used for training and the remaining for testing
train <- df[1:900,]
test <- df[901:1046,]

apriori <- c(
  nrow(train[train$survived=="0",])/nrow(train),
  nrow(train[train$survived=="1",])/nrow(train)
)

count_survived <- c(
  length(train$survived[train$survived=="0"]),
  length(train$survived[train$survived=="1"])
)

lh_pclass <- matrix(rep(0,6), ncol=3)
for (sv in c("0", "1")){
  for (pc in c("1","2","3")) {
    lh_pclass[as.integer(sv)+1, as.integer(pc)] <-
      nrow(train[train$pclass==pc & train$survived==sv,]) /
      count_survived[as.integer(sv)+1]
  }
}

# likelihood for sex
lh_sex <- matrix(rep(0,4), ncol=2)
for (sv in c("0", "1")){
  for (sx in c(2, 3)) {
    lh_sex[as.integer(sv)+1, sx-1] <-
      nrow(train[as.integer(train$sex)==sx & train$survived==sv,]) /
      count_survived[as.integer(sv)+1]
  }
}

age_mean <- c(0, 0)
age_var <- c(0, 0)
for (sv in c("0", "1")){
  age_mean[as.integer(sv)+1] <-
    mean(train$age[train$survived==sv])
  age_var[as.integer(sv)+1] <-
    var(train$age[train$survived==sv])
}

calc_age_lh <- function(v, mean_v, var_v){
  # run like this: calc_age_lh(6, 25.9, 138)
  1 / sqrt(2 * pi * var_v) * exp(-((v-mean_v)^2)/(2 * var_v))
}

calc_raw_prob <- function(pclass, sex, age) {
  # pclass=1,2,3 sex=1,2 age=numeric
  num_s <- lh_pclass[2, pclass] * lh_sex[2, sex] * apriori[2] *
    calc_age_lh(age, age_mean[2], age_var[2])
  num_p <- lh_pclass[1, pclass] * lh_sex[1, sex] * apriori[1] *
    calc_age_lh(age, age_mean[1], age_var[1])
  denominator <- lh_pclass[2, pclass] * lh_sex[2, sex] *
    calc_age_lh(age, age_mean[2], age_var[2]) * apriori[2] +
    lh_pclass[1, pclass] * lh_sex[1, sex] *
    calc_age_lh(age, age_mean[1], age_var[1]) * apriori[1]
  
  print(paste(pclass))
  print(paste(sex))
  print(paste(age))
  
  print(paste())
  
  print(paste(lh_pclass[2, pclass]))
  print(paste(lh_sex[2, sex]))
  print(paste(apriori[2]))
  print(paste(calc_age_lh(age, age_mean[2], age_var[2])))
  
  print(paste())
  
  print(paste(lh_pclass[1, pclass]))
  print(paste(lh_sex[1, sex]))
  print(paste(apriori[1]))
  print(paste(calc_age_lh(age, age_mean[1], age_var[1])))
  
  print(paste())
  
  print(paste(num_s))
  print(paste(num_p))
  print(paste(denominator))
  
  print(paste())
  
  return (list(prob_survived <- num_s / denominator,
               prob_perished <- num_p / denominator))
}


for (i in 1:1){
  raw <- calc_raw_prob(test[i,2], as.integer(test[i,4]), test[i,5])
  print(paste(raw[2], raw[1]))
}



