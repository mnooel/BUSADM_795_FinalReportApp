# Title     : im_script2.R
# Objective : render plots related to the implentation section
# Created by: michaelnoel
# Created on: 12/5/20

library(Hmisc)
library(leaps)

### dataframe functions ###

# dataframe to rmse
im_rmse <- function(dataframe) {
  dataframe2 <- dataframe[-64,]
  dataframe2$ln_cost_of_goods_sold <- log(abs(dataframe2$cost_of_goods_sold)) # todo confirm abs use
  #create indicator variables for month name ----
  Jan <- as.numeric(dataframe2$month_name == "Jan")
  Feb <- as.numeric(dataframe2$month_name == "Feb")
  Mar <- as.numeric(dataframe2$month_name == "Mar")
  Apr <- as.numeric(dataframe2$month_name == "Apr")
  May <- as.numeric(dataframe2$month_name == "May")
  Jun <- as.numeric(dataframe2$month_name == "Jun")
  Jul <- as.numeric(dataframe2$month_name == "Jul")
  Aug <- as.numeric(dataframe2$month_name == "Aug")
  Sep <- as.numeric(dataframe2$month_name == "Sep")
  Oct <- as.numeric(dataframe2$month_name == "Oct")
  Nov <- as.numeric(dataframe2$month_name == "Nov")
  Dec <- as.numeric(dataframe2$month_name == "Dec")
  #combine with dataframe
  dataframe2 <- cbind(dataframe2, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)
  #create lagging variables ----
  library(Hmisc)
  income_1 <- Lag(dataframe2$income, 1)
  hours_worked_1 <- Lag(dataframe2$hours_worked, 1)
  billable_hours_1 <- Lag(dataframe2$billable_hours, 1)
  billable_amount_1 <- Lag(dataframe2$billable_amount, 1)
  new_biz_hours_1 <- Lag(dataframe2$new_biz_hours, 1)
  new_biz_amount_1 <- Lag(dataframe2$new_biz_amount, 1)
  num_of_time_entries_1 <- Lag(dataframe2$num_of_time_entries, 1)
  number_of_time_entries_billable_1 <- Lag(dataframe2$number_of_time_entries_billable, 1)
  number_of_new_biz_time_entries_1 <- Lag(dataframe2$number_of_new_biz_time_entries, 1)
  ln_cost_of_goods_sold_1 <- Lag(dataframe2$ln_cost_of_goods_sold, 1)
  cost_of_goods_sold_1 <- Lag(dataframe2$cost_of_goods_sold, 1)
  expense_1 <- Lag(dataframe2$expense, 1)
  # rm(cost_of_goods_sold)
  dataframe2 <- cbind(dataframe2, income_1, hours_worked_1, billable_hours_1, billable_amount_1,
                      new_biz_hours_1, new_biz_amount_1, num_of_time_entries_1,
                      number_of_time_entries_billable_1, number_of_new_biz_time_entries_1,
                      ln_cost_of_goods_sold_1, cost_of_goods_sold_1, expense_1)


  #remove rows with NA values
  dataframe2.omit <- na.omit(dataframe2)
  #using BIC Backward selection to find best variables
  full <- lm(income ~ income_1 +
    billable_hours_1 +
    num_of_time_entries_1 +
    number_of_new_biz_time_entries_1 +
    number_of_time_entries_billable_1 +
    billable_amount_1 +
    new_biz_hours_1 +
    new_biz_amount_1 +
    hours_worked_1 +
    cost_of_goods_sold_1 +
    expense_1 +
    month +
    Jan +
    Feb +
    Mar +
    Apr +
    May +
    Jun +
    Jul +
    Aug +
    Sep +
    Oct +
    Nov, data = dataframe2.omit)

  n <- nrow(dataframe2.omit)
  step(full, direction = "backward", k = log(n), trace = F)
  bic <- lm(income ~ income_1 +
    billable_amount_1 +
    number_of_new_biz_time_entries_1 +
    month +
    cost_of_goods_sold_1 +
    Oct, data = dataframe2.omit)
  summary(bic)

  #k-fold model selection ----
  library(leaps)
  set.seed(420)

  predict.regsubsets <- function(object, newdata, id) {
    form <- as.formula(object$call[[2]])
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id = id)
    xvars <- names(coefi)
    mat[, xvars] %*% coefi
  }

  k <- 5
  folds <- sample(1:k, nrow(dataframe2.omit), replace = TRUE)
  cv.errors <- matrix(NA, k, 23, dimnames = list(NULL, paste(1:23)))
  cv.errors
  set.seed(420)
  for (j in 1:k) {
    best.fit <- regsubsets(formula(full), data = dataframe2.omit[folds != j,], nvmax = 23)
    for (i in 1:23) {
      pred <- predict.regsubsets(best.fit, dataframe2.omit[folds == j,], id = i)
      cv.errors[j, i] <- mean((dataframe2.omit$income[folds == j] - pred)^2)
    }
  }
  mean.cv.errors <- apply(cv.errors, 2, mean)
  rmse <- sqrt(mean.cv.errors)
  rmse
}

# dataframe to kfolds_model
im_kfold_model <- function(dataframe) {
  dataframe2 <- dataframe[-64,]
  dataframe2$ln_cost_of_goods_sold <- log(abs(dataframe2$cost_of_goods_sold)) # todo confirm abs use
  #create indicator variables for month name ----
  Jan <- as.numeric(dataframe2$month_name == "Jan")
  Feb <- as.numeric(dataframe2$month_name == "Feb")
  Mar <- as.numeric(dataframe2$month_name == "Mar")
  Apr <- as.numeric(dataframe2$month_name == "Apr")
  May <- as.numeric(dataframe2$month_name == "May")
  Jun <- as.numeric(dataframe2$month_name == "Jun")
  Jul <- as.numeric(dataframe2$month_name == "Jul")
  Aug <- as.numeric(dataframe2$month_name == "Aug")
  Sep <- as.numeric(dataframe2$month_name == "Sep")
  Oct <- as.numeric(dataframe2$month_name == "Oct")
  Nov <- as.numeric(dataframe2$month_name == "Nov")
  Dec <- as.numeric(dataframe2$month_name == "Dec")
  #combine with dataframe
  dataframe2 <- cbind(dataframe2, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)
  #create lagging variables ----
  income_1 <- Lag(dataframe2$income, 1)
  hours_worked_1 <- Lag(dataframe2$hours_worked, 1)
  billable_hours_1 <- Lag(dataframe2$billable_hours, 1)
  billable_amount_1 <- Lag(dataframe2$billable_amount, 1)
  new_biz_hours_1 <- Lag(dataframe2$new_biz_hours, 1)
  new_biz_amount_1 <- Lag(dataframe2$new_biz_amount, 1)
  num_of_time_entries_1 <- Lag(dataframe2$num_of_time_entries, 1)
  number_of_time_entries_billable_1 <- Lag(dataframe2$number_of_time_entries_billable, 1)
  number_of_new_biz_time_entries_1 <- Lag(dataframe2$number_of_new_biz_time_entries, 1)
  ln_cost_of_goods_sold_1 <- Lag(dataframe2$ln_cost_of_goods_sold, 1)
  cost_of_goods_sold_1 <- Lag(dataframe2$cost_of_goods_sold, 1)
  expense_1 <- Lag(dataframe2$expense, 1)
  # rm(cost_of_goods_sold)
  dataframe2 <- cbind(dataframe2, income_1, hours_worked_1, billable_hours_1, billable_amount_1,
                      new_biz_hours_1, new_biz_amount_1, num_of_time_entries_1,
                      number_of_time_entries_billable_1, number_of_new_biz_time_entries_1,
                      ln_cost_of_goods_sold_1, cost_of_goods_sold_1, expense_1)


  #remove rows with NA values
  dataframe2.omit <- na.omit(dataframe2)
  #using BIC Backward selection to find best variables
  full <- lm(income ~ income_1 +
    billable_hours_1 +
    num_of_time_entries_1 +
    number_of_new_biz_time_entries_1 +
    number_of_time_entries_billable_1 +
    billable_amount_1 +
    new_biz_hours_1 +
    new_biz_amount_1 +
    hours_worked_1 +
    cost_of_goods_sold_1 +
    expense_1 +
    month +
    Jan +
    Feb +
    Mar +
    Apr +
    May +
    Jun +
    Jul +
    Aug +
    Sep +
    Oct +
    Nov, data = dataframe2.omit)

  n <- nrow(dataframe2.omit)
  step(full, direction = "backward", k = log(n), trace = F)
  bic <- lm(income ~ income_1 +
    billable_amount_1 +
    number_of_new_biz_time_entries_1 +
    month +
    cost_of_goods_sold_1 +
    Oct, data = dataframe2.omit)
  summary(bic)

  #k-fold model selection ----
  set.seed(420)

  predict.regsubsets <- function(object, newdata, id) {
    form <- as.formula(object$call[[2]])
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id = id)
    xvars <- names(coefi)
    mat[, xvars] %*% coefi
  }

  k <- 5
  folds <- sample(1:k, nrow(dataframe2.omit), replace = TRUE)
  cv.errors <- matrix(NA, k, 23, dimnames = list(NULL, paste(1:23)))
  cv.errors
  set.seed(420)
  for (j in 1:k) {
    best.fit <- regsubsets(formula(full), data = dataframe2.omit[folds != j,], nvmax = 23)
    for (i in 1:23) {
      pred <- predict.regsubsets(best.fit, dataframe2.omit[folds == j,], id = i)
      cv.errors[j, i] <- mean((dataframe2.omit$income[folds == j] - pred)^2)
    }
  }
  mean.cv.errors <- apply(cv.errors, 2, mean)
  rmse <- sqrt(mean.cv.errors) # rmse

  plot(rmse, pch = 19, type = "b")

  reg.best <- regsubsets(formula(full), data = dataframe2.omit, nvmax = 23)
  coef(reg.best, 7)

  kfold_model <- lm(income ~ number_of_new_biz_time_entries_1 +
    billable_amount_1 +
    cost_of_goods_sold_1 +
    month +
    Oct +
    Nov +
    Feb, data = dataframe2.omit)
}

### im_plot5 ###

# im_render_plot5
im_render_plot5 <- function(dataframe) {
  rmse <- im_rmse(dataframe)
  plot <- plot(rmse, pch = 19, type = "b")
  plot
}

### im_plot6 ###


### im_plot2 ###
