
install.packages("rstanarm")
install.packages("rstan")
install.packages("loo")
install.packages("stargazer")
install.packages("caret")
install.packages("boot")
library("rstanarm")
library("rstan")
library("loo")
library(ggplot2)
library(dplyr)
library(gridExtra)
library(stargazer)
library(car)
library(caret)
library(boot)

set.seed(1)

#Data

data <- read.csv(file= "bank.csv", header=TRUE)
summary(data)

#Unknowns into NAs

sapply(data, function(y) sum(length(which(is.na(y))))) # check for missing values
sapply(data, function(y) sum(length(which(y =='unknown')))) # check for 'unknown' values
sum(sapply(data, function(y) sum(length(which(y =='unknown')))))
summary(filter(data, loan=='unknown')) # All observations with missing housing values were also missing loan values. 
default <- data$default
data[data == "unknown"] <- NA # transform 'unknown' into NA
data$default <- default # I want to keep the 'unknown' value for the default variable, as there are too many rows with missing value and too little variation in values in the data awe do have

## Early visualisations (not described in the report)

hist_age <- ggplot(data, aes(x = age)) + geom_histogram()
hist_campaign <- ggplot(data, aes(x = campaign)) + geom_histogram()
hist_previous <- ggplot(data, aes(x = previous)) + geom_histogram()

hist_emp.var.rate <- ggplot(data, aes(x = emp.var.rate)) + geom_histogram()
hist_cons.price.idx <- ggplot(data, aes(x = cons.price.idx)) + geom_histogram()
hist_cons.conf.idx <- ggplot(data, aes(x = cons.conf.idx)) + geom_histogram()
hist_euribor3m <- ggplot(data, aes(x = euribor3m)) + geom_histogram()
hist_nr.employed <- ggplot(data, aes(x = nr.employed)) + geom_histogram()

grid.arrange(hist_age, hist_campaign, hist_previous, ncol = 1, nrow = 3)

grid.arrange(hist_emp.var.rate, hist_cons.price.idx, hist_cons.conf.idx, hist_euribor3m, hist_nr.employed, ncol = 2, nrow = 3)

scat_age_y <- ggplot(data, aes(x = age, y = y)) + geom_point(alpha = 0.3)
scat_campaign_y <- ggplot(data, aes(x = campaign, y = y)) + geom_point(alpha = 0.5)
scat_previous_y <- ggplot(data, aes(x = previous, y = y)) + geom_point(alpha = 0.5)

scat_emp.var.rate_y <- ggplot(data, aes(x = emp.var.rate, y = y)) + geom_point(alpha = 0.5)
scat_cons.price.idx_y <- ggplot(data, aes(x = cons.price.idx, y = y)) + geom_point(alpha = 0.5)
scat_cons.conf.idx_y <- ggplot(data, aes(x = cons.conf.idx, y = y)) + geom_point(alpha = 0.5)
scat_euribor3m_y <- ggplot(data, aes(x = euribor3m, y = y)) + geom_point(alpha = 0.5)
scat_nr.employed_y <- ggplot(data, aes(x = nr.employed, y = y)) + geom_point(alpha = 0.5)

bar_y_month <- ggplot(data, aes(x = month, ..count..)) + geom_bar(aes(fill = y), position = "dodge") + 
  theme(axis.text.x=element_text(color = "black", size=10, angle=20)) 
bar_y_month

bar_y_day_of_week <- ggplot(data, aes(x = day_of_week, ..count..)) + geom_bar(aes(fill = y), position = "dodge") + 
  theme(axis.text.x=element_text(color = "black", size=10, angle=20)) 
bar_y_day_of_week


bar_y_poutcome <- ggplot(data, aes(x = poutcome, ..count..)) + geom_bar(aes(fill = y), position = "dodge") + 
  theme(axis.text.x=element_text(color = "black", size=10, angle=20)) 
bar_y_poutcome

# Remove rows with NAs

data_custom <- na.omit(data)

# Remove the NA level

data_custom$job <- factor(data_custom$job) 
data_custom$marital <- factor(data_custom$marital) 
data_custom$education <- factor(data_custom$education) 
data_custom$default <- factor(data_custom$default)
data_custom$housing <- factor(data_custom$housing)
data_custom$loan <- factor(data_custom$loan) 

# Merging vriables

## Outcome ##

data_custom$y <- ifelse(data_custom$y == "yes", 1, 0)
table(data_custom$y)/sum(table(data_custom$y))

## Job ##

group_by(data_custom, job) %>% summarize("mean of y" = mean(y))

data_custom$job_merged <- as.character(data_custom$job) 
data_custom$job_merged[data_custom$job_merged %in% c("self-employed","housemaid", "management", "admin.", "entrepreneur", "housemaid", "management", "technician", "unemployed")]<-"other" 
data_custom$job_merged[data_custom$job_merged %in% c("blue-collar","services")]<-"low-wage"
data_custom$job_merged <- factor(data_custom$job_merged, order = FALSE, levels =c('other', 'low-wage', 'student', 'retired'))

levels(data_custom$job_merged)
table(data_custom$job_merged)
table(data_custom$job_merged)/sum(table(data_custom$job_merged))
group_by(data_custom, job_merged) %>% summarize("mean of y" = mean(y))

## Marital ##

group_by(data_custom, marital) %>% summarize("mean of y" = mean(y))
data_custom <- data_custom %>% mutate("evermarried" = ifelse(marital == "single", 0, 1))

group_by(data_custom, evermarried) %>% summarize("mean of y" = mean(y))
levels(data_custom$evermarried)
table(data_custom$evermarried)
table(data_custom$evermarried)/sum(table(data_custom$evermarried))

## Education ##

group_by(data_custom, education) %>% summarize('mean of y' = mean(y)) # Illiterate and university stand out
data_custom$has.university <- ifelse(data_custom$education=="university.degree", 1, 0)
group_by(data_custom, has.university) %>% summarize('mean of y' = mean(y))

levels(data_custom$has.university)
table(data_custom$has.university)
table(data_custom$has.university)/sum(table(data_custom$has.university))

## Housing, Default, Loans, and Contact have two levels and don't require changes ##

group_by(data_custom, default) %>% summarize('mean of y' = mean(y)) # likely very significant, but few observations of defaults
data_custom$confirmed.no.default <- ifelse(data_custom$default == 'no', 1, 0)
table(data_custom$confirmed.no.default)
table(data_custom$confirmed.no.default)/sum(table(data_custom$confirmed.no.default))

group_by(data_custom, housing) %>% summarize('mean of y' = mean(y)) # likely not significant

group_by(data_custom, loan) %>% summarize('mean of y' = mean(y)) # likely not significant

group_by(data_custom, contact) %>% summarize('mean of y' = mean(y)) # likely significant
table(data_custom$contact)
table(data_custom$contact)/sum(table(data_custom$contact))


table(data_custom$contact)
table(data_custom$contact)/sum(table(data_custom$contact))

## Pdays ##
group_by(data_custom, pdays) %>% summarize('mean of y' = mean(y))
data_custom$was.contacted <-ifelse(data_custom$pdays == 999, 0, 1)
group_by(data_custom, was.contacted) %>% summarize('mean of y' = mean(y))

levels(data_custom$was.contacted)
table(data_custom$was.contacted)
table(data_custom$was.contacted)/sum(table(data_custom$was.contacted))

## Poutcome ##
group_by(data_custom, poutcome) %>% summarize('mean of y' = mean(y))

data_custom$past_success <- ifelse(data_custom$poutcome == "success", 1, 0)

group_by(data_custom, past_success) %>% summarize('mean of y' = mean(y))

table(data_custom$past_success)

## Days ##

group_by(data_custom, day_of_week) %>% summarize('mean of y' = mean(y))

## Month ##

group_by(data_custom, month) %>% summarize('mean of y' = mean(y))
table(data_custom$month)
xtabs(~ y + month, data_custom)

## Contact ##

data_custom$contact_by_phone <- ifelse(data_custom$contact == "telephone", 1, 0)

# create a final dataset

df <- data_custom %>% select(y, job_merged, month, housing, loan, has.university, confirmed.no.default, campaign, evermarried, was.contacted, past_success, contact_by_phone, previous, age, duration, emp.var.rate, cons.price.idx, cons.conf.idx, euribor3m, nr.employed) #
df2 <- data_custom %>% select(y, job_merged, month, housing, loan, has.university, confirmed.no.default, campaign, evermarried, was.contacted, past_success, contact_by_phone, previous, age, duration, emp.var.rate, cons.price.idx, cons.conf.idx, euribor3m, nr.employed) # This will be used for cross validaation


# xtabs

xtabs(~ y + job_merged, df)

xtabs(~ y + job_merged, df)/sum(table(df$job_merged))

xtabs(~ y + evermarried, df)

xtabs(~ y + has.university, df)

xtabs(~ y + housing, df)

xtabs(~ y + loan, df)

xtabs(~ y + contact_by_phone, df)

xtabs(~ y + was.contacted, df)

xtabs(~ y + past_success, df)

# Regressions and plots for independent continuous variables against outcome

par(mfrow=c(4,4))

cdplot(factor(y)~age, df)
boxplot(age~y, df)
dir()

cdplot(factor(y)~emp.var.rate, df)
boxplot(emp.var.rate~y, df)

cdplot(factor(y)~cons.price.idx, df)
boxplot(cons.price.idx~y, df)

cdplot(factor(y)~cons.conf.idx, df)
boxplot(cons.conf.idx~y, df)

cdplot(factor(y)~euribor3m, df)
boxplot(euribor3m~y, df)

cdplot(factor(y)~nr.employed, df)
boxplot(nr.employed~y, df)

boxplot(duration~y, df)

lreg_age <- glm(y ~ age, df, family = "binomial")
summary(lreg_age)
stargazer(lreg_age, type = "text")

lreg_duration <- glm(y ~ duration, df, family = "binomial")
summary(lreg_duration)
stargazer(lreg_duration, type = "text")

lreg_cons.price.idx <- glm(y ~ cons.price.idx, df, family = "binomial")
summary(lreg_cons.price.idx)
stargazer(lreg_cons.price.idx, type = "text")

lreg_emp.var.rate <- glm(y ~ emp.var.rate, df, family = "binomial")
summary(lreg_emp.var.rate)
stargazer(lreg_emp.var.rate, type = "text")

lreg_cons.conf.idx <- glm(y ~ cons.conf.idx, df, family = "binomial")
summary(lreg_cons.conf.idx)
stargazer(lreg_cons.conf.idx, type = "text")

lreg_euribor3m <- glm(y ~ euribor3m, df, family = "binomial")
summary(lreg_euribor3m)
stargazer(lreg_euribor3m, type = "text")

lreg_nr.employed <- glm(y ~ nr.employed, df, family = "binomial")
summary(lreg_nr.employed)
stargazer(lreg_nr.employed, type = "text")

## Regression Models ##

## Model 1: Null ##

# Finding the deviance in the null model
glm.0<- glm(y~1, binomial,data=df)
glm.0

## Model 2: Full ##

# All predictors
glm.1 <- glm(y~., data=df, family="binomial")
glm.1
summary(glm.1)
stargazer(glm.1, type="text")

anova(glm.0,glm.1,test="Chisq") #The full model is a big improvement over the null model

## Collinearity ##

vif(glm.1)

# correlation matrix of all numeric variables leads to same results as VIF and devance analysis

cor(df[,6:20]) # cons.price.idx, euribor3m, nr.employed, emp.var.rate are highly correlated; past_success and was.contacted highly correlated

summary(filter(data_custom, past_success==1)) # All past_success were also was.contacted 

df <- select(df, -euribor3m, -was.contacted, -emp.var.rate)

# vif smaller dataset
glm.2 <- glm(y~., data=df, family="binomial")
vif(glm.2)

# correlation matrix new dataset
cor(df[,6:17]) 

# I then look at multiple models sequentially and at deviance

df <- df[,c(1, 2, 4:11, 13,17,3,12,14,15,16)]

glm.3 <- glm(y~., data=df, family="binomial")
summary(glm.3)

anova(glm.1, glm.3, test="Chisq")

anova(glm.3, test="Chisq") # ony housing and loan do not reduce deviance

df <- select(df, -housing, -loan)
glm.4 <- glm(y~., data=df, family="binomial")
summary(glm.4)
anova(glm.4, test="Chisq")
vif(glm.4)

glm.5 <- glm(y~.,data=select(df, -campaign, -duration,-cons.price.idx, -cons.conf.idx, -age,-evermarried, -previous), family="binomial")
summary(glm.5)
stargazer(glm.5, type="text")
anova(glm.4,glm.5,test="Chisq") # significant but slight improvement; remove additional variables to improve interpretability
anova(glm.5, test="Chisq")

## Cross-Validation ##
# 10-fold cross validation

# note! I run the following code three times, eaach time using a different cost function: default for MSE, from line 328 for prediction accuraacy, from line 329 for predction accuracy of outcomes with y=1. 

# Incumbent model (glm.5)

set.seed (17)
cv.error10.glm.5= rep(0 ,10)
cost <- function(r, pi) mean(ifelse(r==round(pi), 1, 0)) # mean(abs(r-pi) > 0.5) # change the cost function to prediction error rate
cost <- function(r, pi) mean(na.omit(ifelse(r==1,(ifelse(r==round(pi), 1, 0)), NA))) 
for (i in 1:10) {
  glm.5.temp <- glm(y~.,data=select(df, -has.university, -evermarried, -cons.price.idx, -cons.conf.idx, -age, -previous), family="binomial")
  cv.error10.glm.5[i]= cv.glm(select(df, -has.university, -evermarried, -cons.price.idx, -cons.conf.idx, -age, -previous), glm.5.temp, cost,K=10)$delta[1]
  }
cv.error10.glm.5
mean(cv.error10.glm.5)


# Glm.5 w/o month

set.seed (17)
cv.error10.glm.6= rep(0 ,10)
for (i in 1:10) {
  glm.6.temp <- glm(y~.,data=select(df, -has.university, -month, -evermarried, -cons.price.idx, -cons.conf.idx, -age, -previous), family="binomial")
  cv.error10.glm.6[i]= cv.glm(select(df, -has.university, -month, -evermarried, -cons.price.idx, -cons.conf.idx, -age, -previous), glm.6.temp ,cost,K=10)$delta[1]
}
cv.error10.glm.6
mean(cv.error10.glm.6)

# Original data; before merging

set.seed (17)
cv.error10.glm.7= rep(0 ,10)
for (i in 1:10) {
  glm.7.temp <- glm(y~.,data=data_custom[,1:22], family="binomial")
  cv.error10.glm.7[i]= cv.glm(data_custom, glm.7.temp, cost ,K=10)$delta[1]
}
cv.error10.glm.7
mean(cv.error10.glm.7)

# FULL dataset without correlated variable removed

set.seed (17)
cv.error10.glm.8= rep(0 ,10)
for (i in 1:10) {
  glm.8.temp <- glm(y~.,data=df2, family="binomial")
  cv.error10.glm.8[i]= cv.glm(df2, glm.8.temp,cost,K=10)$delta[1]
}
cv.error10.glm.8
mean(cv.error10.glm.8)

# null dataset

set.seed (17)
cv.error10.glm.9= rep(0 ,10)
for (i in 1:10) {
  glm.9.temp <- glm(y~1,data=df2, family="binomial")
  cv.error10.glm.9[i]= cv.glm(df2, glm.9.temp,K=10)$delta[1]
}
cv.error10.glm.9
mean(cv.error10.glm.9)

# loocv
glm.5.temp <- glm(y~.,data=select(df, -has.university, -evermarried, -cons.price.idx, -cons.conf.idx, -age, -previous), family="binomial")
loo.cv.error.glm.5 <- cv.glm(select(df, -has.university, -evermarried, -cons.price.idx, -cons.conf.idx, -age, -previous), glm.5.temp)
loo.cv.error.glm.5$delta

glm.6.temp <- glm(y~.,data=select(df, -has.university, -month, -evermarried, -cons.price.idx, -cons.conf.idx, -age, -previous), family="binomial")
loo.cv.error.glm.6 <- cv.glm(select(df, -has.university, -month, -evermarried, -cons.price.idx, -cons.conf.idx, -age, -previous), glm.6.temp)
loo.cv.error.glm.6$delta

# prediction

df$outcome <- predict(glm.5.temp,newdata=df, type='response')
df$outcomelog <- predict(glm.5.temp,newdata=df, type='link')

