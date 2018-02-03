# BAX 452 HW
# Q1.1
library(freqparcoord)
library(regtools)
data(mlb)
xvalpart <- function(data, p) {
  n <- nrow(mlb)
  ntrain <- round(p*n)
  trainidxs <- sample(1:n, ntrain, replace=FALSE)
  list(train=data[trainidxs ,],
       valid=data[-trainidxs ,])
}

xvallm <- function(data, ycol, predvars, p, meanabs=TRUE){
  tmp <- xvalpart(data,p)
  train <- tmp$train
  valid <- tmp$valid
  trainy <- train[ , ycol]
  trainpreds <- train[ , predvars]
  trainpreds <- as.matrix(trainpreds)
  lmout <- lm(trainy ~ trainpreds)
  validpreds <- as.matrix(valid[ , predvars])
  predy <- cbind(1, validpreds)%*% coef(lmout)
  realy <- valid[ , ycol]
  if (meanabs) return(mean(abs(predy - realy)))
  list( predy = predy , realy = realy)
}

# using age and height as predictors to predicti weight
set.seed (9999)
xvallm(mlb, 5, c(4,6), 2/3)
# we would be off by about 13.67486 pounds

# KNN Case
xvalknn <- function(data,ycol ,predvars ,k,p,meanabs=TRUE) {
  # cull out just Y and the Xs
  data <- data[, c(predvars, ycol)] 
  ycol <- length(predvars) + 1
  tmp <- xvalpart(data,p)
  train <- tmp$train
  valid <- tmp$valid
  valid <- as.matrix(valid)
  xd <- preprocessx(train[,-ycol],k)
  kout <- knnest(train[,ycol],xd,k)
  predy <- predict(kout, valid[, -ycol], TRUE) 
  realy <- valid[, ycol]
  if (meanabs) return(mean(abs(predy - realy))) 
  list (predy = predy , realy = realy)
}

# KNN Predict

set.seed (9999)
xvalknn(mlb, 5, c(4 ,6), 5, 2/3)
# we would be off by about 14.86746 pounds

# Q 1.2
prgeng <- read.csv('prgeng.csv')
prgeng$age2 <- prgeng$age^2
edu <- prgeng$educ
prgeng$ms <- as.integer(edu == 14)
prgeng$phd <- as.integer(edu == 16)
prgeng$fem <- prgeng$sex-1
tmp_1_2 <- prgeng[edu >= 13,]
pe_1_2 <- tmp_1_2[,c(9,2,13,14,15,16)]
pe_1_2$agefem <- pe_1_2$age * pe_1_2$fem
pe_1_2$age2fem <- pe_1_2$age2 * pe_1_2$fem
model_1_2 = lm(wageinc ~ age + age2 + ms + phd + fem + agefem + age2fem, data = pe_1_2)
summary(model_1_2)
age <- 32
age2 <- 32 * 32
ms <- 1
phd <- 0
fem <- 1
agefem <- 32
age2fem <- 32*32
predictor_1_2 <- data.frame(age, age2, ms, phd, fem, agefem, age2fem)
predict(model, predictor_1_2)

# Wageinc for a 32-year-old female with a Masters degree is 58888.26

#Q 1.3
bodyfat <- read.csv("bodyfat.csv")
model_1_3 = lm(density ~ age + weight + height + neck + chest + abdomen + hip + 
                 thigh + knee + ankle + biceps + forearm + wrist, data = bodyfat)
summary(model_1_3)
# From the summary we can see that, the p-value of age, height, chest, hip, thigh, knee, 
# ankle, biceps, are larger than 0.05, which shows that these variables are not significant. 
# Since the adjusted R-square value is greater than 0.7238, 
# the indirect method is feasible for this model.

# Q 1.4
# Part a
# The overall height of people is equal to the weighted average of female and male's mean heights.

# Part b
# The weighted average proportion of female and male's mean heights are taller than 70 inches.

# Q 2.1
# Part a

prgeng <- read.csv('prgeng.csv')
prgeng$age2 <- (prgeng$age)^2
edu <- prgeng$educ
prgeng$ms <- as.integer(edu == 14)
prgeng$phd <- as.integer(edu == 16)
prgeng$fem <- prgeng$sex-1
tmp_2_1_a <- prgeng[edu >= 13,]
pe_2_1_a <- tmp_2_1_a[,c(9,2,13,10,14,15,16)]
model_2_1_a <- lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem, data = pe_2_1_a)
summary(model_2_1_a)
UCL_2_1_a <- -11176.74 + 1.96 * 912.206
LCL_2_1_a <- -11176.74 - 1.96 * 912.206
data.frame(LCL_2_1_a, UCL_2_1_a)
# The confidence interval of β6 is (-12964.66,-9388.816)

# Part b

prgeng$msfem <- prgeng$ms * prgeng$fem
prgeng$phdfem <- prgeng$phd * prgeng$fem
tmp_2_1_b <- prgeng[edu >= 13,]
pe_2_1_b <- tmp_2_1_b[,c(9,2,13,10,14,15,16,17,18)]
model_2_1_b <- lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem + msfem + phdfem, data = pe_2_1_b)
summary(model_2_1_b)
UCL_2_1_b <- 1.96 * 1975.841 - 5088.779
LCL_2_1_b <- -5088.779 - 1.96 * 1975.841
data.frame(LCL_2_1_b, UCL_2_1_b)
# The confidence interval of β6+β7 is (-8961.427, -1216.131)

# Q 2.2
day <- read.csv('day.csv')
names(day)
day$temp2 <- (day$temp)^2
day$clearday <- as.integer(day$weathersit == 1)
model_bike <- lm(registered ~ temp + temp2 + workingday + clearday + yr, data = day)
summary(model_bike)
LCL_2_2 <- 1716.25 - 1.96 * 56.58
UCL_2_2 <- 1716.25 + 1.96 * 56.58
data.frame(LCL_2_2, UCL_2_2)
### The confidence interval is (1605.353, 1827.147)

# 7

# 8
# (rho)^2 = 1 - Var(Epsilon) / Var(Y) = 1 - Var(Epsilon) / (Var(mean(x)) + Var(Epsilon)) = 1 - p/(p+p) = 0.5
