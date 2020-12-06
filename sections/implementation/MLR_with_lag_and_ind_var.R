coredata <- read.csv("edited_csv_table_dataaTimeMonth_v3.csv")
#observation 64 is outlier
#create new index called coredata2
coredata2 <- coredata[-64, ]
coredata2$ln_cost_of_goods_sold <- log(coredata2$cost_of_goods_sold)
#create indicator variables for month name ----
Jan <- as.numeric(coredata2$month_name == "Jan")
Feb <- as.numeric(coredata2$month_name == "Feb")
Mar <- as.numeric(coredata2$month_name == "Mar")
Apr <- as.numeric(coredata2$month_name == "Apr")
May <- as.numeric(coredata2$month_name == "May")
Jun <- as.numeric(coredata2$month_name == "Jun")
Jul <- as.numeric(coredata2$month_name == "Jul")
Aug <- as.numeric(coredata2$month_name == "Aug")
Sep <- as.numeric(coredata2$month_name == "Sep")
Oct <- as.numeric(coredata2$month_name == "Oct")
Nov <- as.numeric(coredata2$month_name == "Nov")
Dec <- as.numeric(coredata2$month_name == "Dec")
#combine with dataframe
coredata2 <- cbind(coredata2, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)
#create lagging variables ----
library(Hmisc)
income_1 <- Lag(coredata2$income, 1)
hours_worked_1 <- Lag(coredata2$hours_worked, 1)
billable_hours_1 <- Lag(coredata2$billable_hours, 1)
billable_amount_1 <- Lag(coredata2$billable_amount, 1)
new_biz_hours_1 <- Lag(coredata2$new_biz_hours, 1)
new_biz_amount_1 <- Lag(coredata2$new_biz_amount, 1)
num_of_time_entries_1 <- Lag(coredata2$num_of_time_entries, 1)
number_of_time_entries_billable_1 <- Lag(coredata2$number_of_time_entries_billable, 1)
number_of_new_biz_time_entries_1 <- Lag(coredata2$number_of_new_biz_time_entries, 1)
ln_cost_of_goods_sold_1 <- Lag(coredata2$ln_cost_of_goods_sold, 1)
cost_of_goods_sold_1 <- Lag(coredata2$cost_of_goods_sold, 1)
expense_1 <- Lag(coredata2$expense, 1)
rm(cost_of_goods_sold)
coredata2 <-  cbind(coredata2, income_1, hours_worked_1, billable_hours_1, billable_amount_1,
                   new_biz_hours_1, new_biz_amount_1, num_of_time_entries_1,
                   number_of_time_entries_billable_1, number_of_new_biz_time_entries_1,
                   ln_cost_of_goods_sold_1, cost_of_goods_sold_1, expense_1)


#remove rows with NA values
coredata2.omit <- na.omit(coredata2)
#using BIC Backward selection to find best variables
full <- lm(income ~ income_1 + billable_hours_1 + num_of_time_entries_1 + number_of_new_biz_time_entries_1 + number_of_time_entries_billable_1 + billable_amount_1 +
             new_biz_hours_1 + new_biz_amount_1 +
             hours_worked_1 + cost_of_goods_sold_1 + expense_1 + month + 
             Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + 
             Sep + Oct + Nov, data = coredata2.omit)

n <- nrow(coredata2.omit)
step(full, direction = "backward", k = log(n), trace = F)
bic <- lm(income ~ income_1 + billable_amount_1 + number_of_new_biz_time_entries_1  +
           month + cost_of_goods_sold_1 + Oct, data = coredata2.omit)
summary(bic)

#k-fold model selection ----
library(leaps)
set.seed(29)
predict.regsubsets <- function (object, newdata, id){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form,newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

k <- 5
folds <- sample(1:k, nrow(coredata2.omit), replace=TRUE)
cv.errors <- matrix(NA, k, 23, dimnames = list(NULL, paste(1:23)))
cv.errors
set.seed(29)
for(j in 1:k){
  best.fit <- regsubsets(formula(full), data = coredata2.omit[folds != j,], nvmax = 23)
  for (i in 1:23){
    pred <- predict.regsubsets(best.fit, coredata2.omit[folds == j, ], id = i)
    cv.errors[j, i] <- mean((coredata2.omit$income[folds == j] - pred)^2)
  }
}
mean.cv.errors <- apply(cv.errors, 2, mean)
rmse <- sqrt(mean.cv.errors)
rmse

plot(rmse, pch = 19, type = "b")

reg.best <- regsubsets(formula(full), data = coredata2.omit, nvmax = 23)
coef(reg.best, 7)

kfold_model <- lm(income ~ number_of_new_biz_time_entries_1 +
                   billable_amount_1 + cost_of_goods_sold_1 + 
                   month + Oct + Nov + Feb, data = coredata2.omit)
summary(kfold_model)
#model diagnostics
ts.plot(residuals(kfold_model), ylab = "Residuals", main = "Residuals Plot")
abline(h = 0, col = "blue")
plot(fitted(kfold_model) ~ residuals(kfold_model), xlab = "Residuals", ylab = "Fitted")
acf(residuals(kfold_model), main = "Residuals, Autocorrelation")
pacf(residuals(kfold_model), main = "Residuals, Partial Autocorrelation")
hist(residuals(kfold_model), xlab = "residuals(model)", main = "Residuals")
qqnorm(residuals(kfold_model), main = "Normal Q-Q Plot Residuals", pch = 19)

#plot fitted v original
fitted(kfold_model, data = coredata2.omit)
coredata2.omit$fitted <-fitted(kfold_model)

fitted <- ts(coredata2.omit$fitted)
income <- ts(coredata2.omit$income)
#plot fitted v original
ts.plot(income, fitted, lty = c(2,1), col = c("black","blue"))
legend("topleft", c("Original", "Fitted"), lty = c(2,1),
       col = c("black", "blue"), cex = 0.75)

