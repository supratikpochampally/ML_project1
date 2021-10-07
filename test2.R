df <- read.csv("titanic_project.csv", header=TRUE, stringsAsFactors = FALSE)
# subset to just columns survived, pclass, sex, and age
df <- df[,c(1,2,4,5)]
# pclass and survived  and sex should be factors
df$pclass <- factor(df$pclass)
df$survived <- factor(df$survived)
df$sex <- factor(df$sex, levels=c("female", "male"))
# remove NAs
df <- df[!is.na(df$pclass),]
df <- df[!is.na(df$survived),]
df$age[is.na(df$age)] <- median(df$age,na.rm=T)
# divide into train and test
set.seed(1234)
i <- sample(1:nrow(df), 0.75*nrow(df), replace=FALSE)
train <- df[i,]
test <- df[-i,]
# perform Naive Bayes
library(e1071)
nb1 <- naiveBayes(df[,-2], df[,2], data=train)
pred <- predict(nb1, newdata=test[,-2], type="raw")
# look at first 5 (actual: 0 1 1 1 0)
pred[1:5,]




### Calculate priors

apriori <- c(
  nrow(df[df$survived=="0",])/nrow(df),
  nrow(df[df$survived=="1",])/nrow(df)
)
print("Prior probability, survived=no, survived=yes:")
apriori

### Calculate likelihoods for qualitative data


# get survived counts for no and yes
count_survived <- c(
  length(df$survived[df$survived=="0"]),
  length(df$survived[df$survived=="1"])
)
# likelihood for pclass
lh_pclass <- matrix(rep(0,6), ncol=3)
for (sv in c("0", "1")){
  for (pc in c("1","2","3")) {
    lh_pclass[as.integer(sv)+1, as.integer(pc)] <- 
      nrow(df[df$pclass==pc & df$survived==sv,]) / count_survived[as.integer(sv)+1]
  }
}
# likelihood for sex
lh_sex <- matrix(rep(0,4), ncol=2)
for (sv in c("0", "1")){
  for (sx in c(1, 2)) {
    lh_sex[as.integer(sv)+1, sx] <- 
      nrow(df[as.integer(df$sex)==sx & df$survived==sv,]) /
      count_survived[as.integer(sv)+1]
  }
}

### likelihood p(survived|pclass)

print("Likelihood values for p(pclass|survived):")
lh_pclass

### likelihood p(survived|sex)

print("Likelihood values for p(sex|survived):")
lh_sex


### Calculate likelihoods for quantitative data


age_mean <- c(0, 0)
age_var <- c(0, 0)
for (sv in c("0", "1")){
  age_mean[as.integer(sv)+1] <- 
    mean(df$age[df$survived==sv])
  age_var[as.integer(sv)+1] <- 
    var(df$age[df$survived==sv])
}

### Probability density for quantitative data


calc_age_lh <- function(v, mean_v, var_v){
  # run like this: calc_age_lh(6, 25.9, 138)
  1 / sqrt(2 * pi * var_v) * exp(-((v-mean_v)^2)/(2 * var_v))
}




### Function for scratch model


calc_raw_prob <- function(pclass, sex, age) {
  # pclass=1,2,3  sex=1,2   age=numeric
  num_s <- lh_pclass[2, pclass] * lh_sex[2, sex] * apriori[2] *
    calc_age_lh(age, age_mean[2], age_var[2])
  num_p <- lh_pclass[1, pclass] * lh_sex[1, sex] * apriori[1] *
    calc_age_lh(age, age_mean[1], age_var[1])
  denominator <- lh_pclass[2, pclass]  * lh_sex[2, sex] * calc_age_lh(age, age_mean[2], age_var[2]) * apriori[2] + 
    lh_pclass[1, pclass]  * lh_sex[1, sex] * calc_age_lh(age, age_mean[1], age_var[1]) * apriori[1]
  return (list(prob_survived <- num_s / denominator, prob_perished <- num_p / denominator))
}


### Apply to the first 5 test observations



for (i in 1:5){
  raw <- calc_raw_prob(test[i,1], as.integer(test[i,3]), test[i,4])
  print(paste(raw[2], raw[1]))
}
pred[1:5,]

