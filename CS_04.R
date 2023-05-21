# Case-Study Title: Sports Analytics (Step-wise Regression and Subset Selection Regression)
# Data Analysis methodology: CRISP-DM
# Dataset: Hitters dataset (Major League Baseball Data from the 1986 and 1987 seasons in US)
# Case Goal: Annual Salary prediction of each Player in 1987 base on his performance in 1986


### Required Libraries ----
install.packages('leaps')
install.packages('moments')
install.packages('MASS')
install.packages('corrplot')
library('leaps')
library('moments')
library('MASS')
library('corrplot')


### Read Data from File ----
data <- read.csv('CS_04.csv', header = T)
dim(data)  # 322 records, 20 variables


### Step 1: Business Understanding ----
 # know business process and issues
 # know the context of the problem
 # know the order of numbers in the business


### Step 2: Data Understanding ----
### Step 2.1: Data Inspection (Data Understanding from Free Perspective) ----
## Dataset variables definition
colnames(data)  # KPI (Key Performance Indicator) variables

#KPI variables in 1986
#Hits:       Number of hits in 1986
#HmRun:      Number of home runs in 1986
#Runs:       Number of runs in 1986
#RBI:        Number of runs batted in in 1986
#Walks:      Number of walks in 1986
#PutOuts:    Number of put outs in 1986
#Assists:    Number of assists in 1986
#Errors:     Number of errors in 1986

#KPI variables in whole career life
#Years:      Number of years in the major leagues
#CAtBat:     Number of times at bat during his career
#CHits:      Number of hits during his career
#CHmRun:     Number of home runs during his career
#CRuns:      Number of runs during his career
#CRBI:       Number of runs batted in during his career
#CWalks:     Number of walks during his career

#Categorical variables:
#League:     A factor with levels A and N indicating player's league at the end of 1986 (american league|national league)
#Division:   A factor with levels E and W indicating player's division at the end of 1986 (west|east)
#NewLeague:  A factor with levels A and N indicating player's league at the beginning of 1987

#Salary:     1987 annual salary on opening day in thousands of dollars

#Name:       name of players


### Step 2.2: Data Exploring (Data Understanding from Statistical Perspective) ----
## Overview of Dataframe
class(data)
head(data)
tail(data)
str(data)
summary(data)

## Categorical variables should be stored as factor
cat_var <- c('League', 'Division', 'NewLeague')
data[, cat_var] <- lapply(data[, cat_var], factor)

summary(data)  # 59 NAs in Salary
#we have good distribution in categorical variables


### Step 3: Data PreProcessing ----
## Dealing with MVs
#STEP 1: Determine the type of MVs in our sample (look up for evidence of why we have NA in our dataset)
View(data)

#STEP 2: Determine the Extent of MVs in our data (if less than 5%, we can ignore them)
summary(data)

#Varaibles aspect: Summary of Variables (NA count by columns)
mv_summary_1 <- data.frame('variable_name' = colnames(data))
mv_summary_1$mvs_freq <- apply(data, 2, function(x) sum(is.na(x)))  # frequency of NAs in each variable
mv_summary_1$mvs_percent <- round(mv_summary_1$mvs_freq / nrow(data), 3) * 100
View(mv_summary_1)  # NA count and percentage in each variable

#Case aspect: Summary of Cases (NA count by rows)
mv_summary_2 <- as.data.frame(table(apply(data, 1, function(x) sum(is.na(x)))))  # frequency of NAs in each row
colnames(mv_summary_2) <- c('mvs_per_case', 'mvs_freq')
mv_summary_2$mvs_percent <- round(mv_summary_2$mvs_freq/nrow(data),3) * 100
mv_summary_2$mvs_per_case <- as.numeric(levels(mv_summary_2$mvs_per_case))
mv_summary_2$mvs_per_case_percent <- round(mv_summary_2$mvs_per_case/(ncol(data) - 1), 3) * 100
View(mv_summary_2[c(1,4,2,3)])  # 18.3% of observations has 1 NAs (5.3% of columns)

#STEP 3: Diagnose the Randomness degree of the MVs Processes

#STEP 4: Select the Imputation Method (replacement MVs with a valid value)
#Imputation Using Only Valid Data (ignore MVs):
#	Complete Case Approach (just use the records that have no MVs in their columns)
data2 <- data[-which(is.na(data$Salary)),]  # remove records with MVs -> because the outcome variable has MVs and if we want to replace them we have to create a prediction model for Imputation!

data2 <- data2[,-1]  # remove players' name

head(data2)
dim(data2)
sum(is.na(data2))
summary(data2)

## Identify Outliers by Tukey method
tukey_ul <- boxplot(data2$Salary)$stats[5,1]
tukey_ul <- quantile(data2$Salary, probs = 0.75) + 1.5 * IQR(data2$Salary)
tukey_ul  # Tukey upper limit (outlier intersector)
sum(data2$Salary > tukey_ul)  # 11 outlier observation
sum(data2$Salary > tukey_ul)/nrow(data2)*100  # 4% of total data are outliers


## Univariate Profiling (check each variable individually)
# Categorical variables
#check to sure that have good distribution in each category
table(data2$League)
table(data2$Division)
table(data2$NewLeague)

# Continuous variables
par(mar = c(2,2,2,2), mfrow = c(3, 6))
for(i in c(1:12, 15:18)) {
	hist(data2[,i], freq = F, xlab = '', main = paste('Histogram of', colnames(data2)[i]))
	lines(density(data2[,i]), col = 'blue')
	x_fit <- seq(min(data2[,i]), max(data2[,i]), length.out = 50)
	y_fit <- rnorm(x_fit, mean = mean(data2[,i]), sd = sd(data2[,i]))
	lines(x_fit, y_fit, col = 'red')
}
par(mfrow = c(1,1))

boxplot(data2$Salary, main = 'Salary Distribution')
points(1, mean(data2$Salary), col = 'red', pch = 20, cex = 2)

## Bivariate Profiling (measure 2-2 relationships between variables)
# Two Continuous variables (Correlation Analysis)
cor_table <- round(cor(data2[, c(18, 1:12, 15:17)]), 2)
View(cor_table)
corrplot(cor_table)
#Salary has big correlation with career variables + Hits + Runs + RBI + Walks + Years
#predictor variables have high correlations with each other: multi-collinearity serious danger in our Analysis
##Hits with Runs, RBI
##CAtBat with Years, CHits, CHmRun, CRuns, CRBI, CWalks

#Scatter Plot (between Salary and other continuous variables 2 by 2)
par(mar = c(2,2,2,2), mfrow = c(4,4))
for(i in c(1:12, 15:17)){
	plot(x = data2[,i], y = data2$Salary, xlab = '', main = paste('Salary vs.', colnames(data2)[i]))
}
par(mar = c(4.5, 4.5, 4.5, 4.5), mfrow = c(1,1))

## Divide Dataset into Train and Test randomly
#learn model in Train dataset
#evaluate model performance in Test dataset
set.seed(1234)
train_cases <- sample(1:nrow(data2), nrow(data2) * 0.8)  # 80% data to train because dataset is small
train <- data2[train_cases,]
test <- data2[-train_cases,]

#train data distribution must be similar to test data distribution
dim(train)
summary(train)
dim(test)
summary(test)


### Step 4: Modeling ----
# Model 1: Traditional Linear Regression
lm_1 <- lm(Salary ~ ., data = train)  # fit Salary on all of other variables

summary(lm_1)
#F-test p-value < 0.05 -> there is at least one linear relationship between predictor variables and Salary
#more of variables are not significant based-on t-test results
#t-test results are reliable just if Assumptions of Linear Regression model be confirmed

#Check Assumptions of Regression
#1: Normality of residuals
hist(lm_1$residuals, freq = F, breaks = 25)
lines(density(lm_1$residuals), col = 'red')  # skewed to right

qqnorm(lm_1$residuals, main = 'QQ Plot of residuals', pch = 20)
qqline(lm_1$residuals, col = 'red')

jarque.test(lm_1$residuals)
#p-value < 0.05 reject normality assumption

anscombe.test(lm_1$residuals)
#p-value < 0.05 reject normality assumption

#result: Residuals are not Normally Distributed -> reject first Assumption of Regression

#2. Residuals independency
plot(lm_1)  # Diagnostic Plots
#variance of errors along to y_hat is not constant (errors increase within salary increase) -> Heteroscedasticity problem

#Check multicollinearity
car::vif(lm_1)  # so serious multicollinearity (VIF >> 10) -> so, the t-test results is absolutely unreliable for feature selection!

#Conclusion: severe violation of regression assumption (Errors are not Normal, High Multicollinearity)
#so, this modeling approach consider to our dataset nature, is not appropriate!

# Model 2: Box-Cox Transformation (for y variable)
#one cause of Heteroscedasticity problem can be skewness of Salary
#Salary is skewed -> changing variable for decrease skewness (bring Salary to Normal distribution)
box_results <- boxcox(Salary ~ ., data = train, lambda = seq(-5, 5, 0.1))
box_results <- as.data.frame(box_results)
lambda <- box_results[which(box_results$y == max(box_results$y)), 1]
lambda  # the lambda that maximizes log(Likelihood) -> we can replace lambda by 0 consider to confidence level 95% (with 95% probability, lambda lies on this range)
#lambda has not statistically significant difference from 0, so we consider lambda equal to 0

#log transformation
train$Log_Salary <- log(train$Salary)  # consider to lambda = 0
head(train)

lm_2 <- lm(Log_Salary ~ . -Salary, data = train)
summary(lm_2)

#Check Assumptions of Regression
#1: Normality of residuals
hist(lm_2$residuals, freq = F, breaks = 25)
lines(density(lm_2$residuals), col = 'red')  # skewed to right

qqnorm(lm_2$residuals, main = 'QQ Plot of residuals', pch = 20)
qqline(lm_2$residuals, col = 'red')

jarque.test(lm_2$residuals)
#p-value < 0.05 reject normality assumption

anscombe.test(lm_2$residuals)
#p-value < 0.05 reject normality assumption

#result: Residuals are not Normally Distributed -> reject first Assumption of Regression

#2. Residuals independency
plot(lm_2)  # Diagnostic Plots
#variance of errors along to y_hat is not constant (errors increase within salary increase) -> Heteroscedasticity problem
#residuals are better than past (decrease Heteroscedasticity)
#qqplot is closer to 45' line
#consider to Cook's distance zone 0.5 is good

#Check multicollinearity
car::vif(lm_2)  # so serious multicollinearity (VIF >> 10) -> so, the t-test results is absolutely unreliable for feature selection!

#Conclusion: changing variable helped us to close Errors to normal (was good practice)

# Model 3: Best Subset Selection (Stepwise Regression)
bestsub_1 <- leaps::regsubsets(Log_Salary ~ . - Salary, nvmax = 18, data = train, method = 'exhaustive')  # regress Log_Salary, because predict Log_Salary helped us to close model Errors to normal
summary(bestsub_1)  # we have 18 created models here

#Best Model Selection (comparing all 18 model with each other using statistical indexes and pick the best model)
#R-squared (max is better)
summary(bestsub_1)$rsq  # report R-squared from 1-variable to 18-variables

#Adjusted R-squared (max is better)
plot(summary(bestsub_1)$adjr2, type = 'b', xlab = '# of Variables', ylab = 'AdjR2', xaxt = 'n', xlim = c(1, 18)); grid()
axis(1, at = 1:18, labels = 1:18)
points(which.max(summary(bestsub_1)$adjr2), summary(bestsub_1)$adjr2[which.max(summary(bestsub_1)$adjr2)], col = 'red', cex = 2, pch = 20)
#max AdjR2 is for 10-variables Regression

#Cp (min is better)
plot(summary(bestsub_1)$cp, type = 'b', xlab = '# of Variables', ylab = 'Cp', xaxt = 'n', xlim = c(1,18)); grid()
axis(1, at = 1:18, labels = 1:18)
points(which.min(summary(bestsub_1)$cp), summary(bestsub_1)$cp[which.min(summary(bestsub_1)$cp)], col = 'red', cex = 2, pch = 20)
#min Cp is for 6-variables Regression

#BIC (min is better)
plot(summary(bestsub_1)$bic, type = 'b', xlab = '# of Variables', ylab = 'BIC', xaxt = 'n', xlim = c(1,18)); grid()
axis(1, at = 1:18, labels = 1:18)
points(which.min(summary(bestsub_1)$bic), summary(bestsub_1)$bic[which.min(summary(bestsub_1)$bic)], col = 'red', cex = 2, pch = 20)
#min BIC is for 3-variables Regression

#Coefficients of the best Regression model (based-on Adjusted R-squared index)
coef(bestsub_1, 10)  # model with 10 variables

lm_bestsub <- lm(Log_Salary ~ Hits + HmRun + RBI + Walks + Years + CAtBat + CWalks + League + Division + Errors, data = train)
summary(lm_bestsub)

# Model 4: Forward Stepwise Selection (Stepwise Regression)
fwd_1 <- regsubsets(Log_Salary ~ . - Salary, nvmax = 18, data = train, method = 'forward')  # do Forward Stepwise Regression method
summary(fwd_1)

which.max(summary(fwd_1)$adjr2)  # 8-variables model
which.min(summary(fwd_1)$cp)  # 6-variables model
which.min(summary(fwd_1)$bic)  # 3-variables model

# Model 5: Backward Stepwise Selection (Stepwise Regression)
bwd_1 <- regsubsets(Log_Salary ~ . - Salary, nvmax = 18, data = train, method = 'backward')  # do Backward Stepwise Regression method
summary(bwd_1)

which.max(summary(bwd_1)$adjr2)  # 9-variables model
which.min(summary(bwd_1)$cp)  # 6-variables model
which.min(summary(bwd_1)$bic)  # 4-variables model

#compare best models base on Cp
coef(bestsub_1, 6)
coef(fwd_1, 6)
coef(bwd_1, 6)

# Model 6: Using K-fold Cross-Validation Approach (instead of using statistical indexes)
k <- 10
set.seed(123)
folds <- sample(1:k, nrow(train), rep = T)  # create 10-folds
cv_errors <- matrix(data = NA, nrow = k, ncol = 18, dimnames = list(NULL, 1:18))
cv_errors  # matrix of errors of 18 models per k-fold on validation set

#Create prediction function for regsubsets() -> "leaps" library, does not have predict() function!
predict_regsubsets <- function(object,  #regression syntax object (regression formula)
                               newdata, #test dataset
                               id       #which Mk
                               ) {
                              reg_formula <- as.formula(object$call[[2]]) #cast character to regression formula
                              mat    <- model.matrix(reg_formula, newdata) #Design Matrix
                              coef_i <- coef(object, id = id) #extract which Mk
                              mat[, names(coef_i)] %*% coef_i} #DM * RegCoef = Predictions

#K-fold Cross Validation
set.seed(1234)
for(i in 1:k){
	best_fit <- leaps::regsubsets(Log_Salary ~ . - Salary, data = train[folds != i,], nvmax = 18, method = 'exhaustive')
	for(j in 1:18){  # per models M1 to M18
		pred <- predict_regsubsets(best_fit, newdata = train[folds == i,], id = j)  # use fold i for validation set, use j for Mj
		cv_errors[i, j] <- mean((train$Log_Salary[folds == i] - pred) ^ 2)  # use MSE (an index to compare models)
	}
}

View(cv_errors)  # performance of models in 10-fold
mean_cv_errors <- apply(cv_errors, 2, mean)  # mean of columns
plot(mean_cv_errors, type = 'b')
which.min(mean_cv_errors)  # 6-variables Regression model is the best

#Coefficients of the best model base on k-fold cross validation
coef(bestsub_1, 6)  # model with 6 variables

bestsub_cv_1 <- lm(Log_Salary ~ Hits + Walks + Years + CRuns + League + Division, data = train)
summary(bestsub_cv_1)
car::vif(bestsub_cv_1)


### Step 5: Model Evaluation ----
# Test the Model 1 performance
#Prediction
pred_lm <- predict(lm_1, test)  # prediction on test dataset

#Evaluate model performance in Test dataset:
#Actual vs. Prediction
plot(test$Salary, pred_lm, xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)  # compare with 45' line

#Absolute Error mean, median, sd, max, min
lm_abs_err <- abs(pred_lm - test$Salary) #absolute value (AEV)

hist(lm_abs_err, breaks = 25)  # residuals distribution
mean(lm_abs_err)
median(lm_abs_err)
sd(lm_abs_err)
max(lm_abs_err)
min(lm_abs_err)

#boxplot (which observations are outliers?)
boxplot(lm_abs_err, main = 'Error distribution')

# Test the Model 3 performance
#Prediction
pred_bestsub <- predict(lm_bestsub, test)  # prediction on test dataset
pred_bestsub  # prediction of log(Salary)
pred_bestsub <- exp(pred_bestsub)
pred_bestsub  # prediction of Salary

#Evaluate model performance in Test dataset:
#Actual vs. Prediction
plot(test$Salary, pred_bestsub, xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)  # compare with 45' line

#Absolute Error mean, median, sd, max, min
bestsub_abs_err <- abs(pred_bestsub - test$Salary) #absolute value (AEV)

hist(bestsub_abs_err, breaks = 25)  # residuals distribution
mean(bestsub_abs_err)
median(bestsub_abs_err)
sd(bestsub_abs_err)
max(bestsub_abs_err)
min(bestsub_abs_err)

#boxplot (which observations are outliers?)
boxplot(bestsub_abs_err, main = 'Error distribution')

# Test the Model 6 performance
#Prediction
pred_bestsub_cv <- predict(bestsub_cv_1, test)  # prediction on test dataset
pred_bestsub_cv  # prediction of log(Salary)
pred_bestsub_cv <- exp(pred_bestsub_cv)
pred_bestsub_cv  # prediction of Salary

#Evaluate model performance in Test dataset:
#Actual vs. Prediction
plot(test$Salary, pred_bestsub_cv, xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)  # compare with 45' line

#Absolute Error mean, median, sd, max, min
bestsub_cv_abs_err <- abs(pred_bestsub_cv - test$Salary) #absolute value (AEV)

hist(bestsub_cv_abs_err, breaks = 25)  # residuals distribution
mean(bestsub_cv_abs_err)
median(bestsub_cv_abs_err)
sd(bestsub_cv_abs_err)
max(bestsub_cv_abs_err)
min(bestsub_cv_abs_err)

#boxplot (which observations are outliers?)
boxplot(bestsub_cv_abs_err, main = 'Error distribution')

# Comparisons of Models
df <- data.frame('Model_1' = lm_abs_err,  # Simple Linear Regression model with all 18 variables
		 'Model_3' = bestsub_abs_err,  # Best Subset Selection model (based on statistical indexes)
		 'Model_6' = bestsub_cv_abs_err)  # Best Subset Selection model (based on k-fold cross validation)

models_comp <- data.frame('Mean of AbsErrors'   = apply(df, 2, mean),
			  'Median of AbsErrors' = apply(df, 2, median),
			  'Sd of AbsErrors'     = apply(df, 2, sd),
			  'IQR of AbsErrors'    = apply(df, 2, IQR),
			  'Min of AbsErrors'    = apply(df, 2, min),
			  'Max of AbsErrors'    = apply(df, 2, max))

rownames(models_comp) <- c('LM', 'BestSub', 'BestSubCV')
View(models_comp)  

#Best Subset Selection model (based on k-fold cross validation) is the best model for prediction on test data in General

#Boxplot of absolute errors
boxplot(df, main = "Abs. Errors Dist. of Models")
