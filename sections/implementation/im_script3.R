# Title     : im_script3.R
# Objective : Neural Network
# Created by: michaelnoel
# Created on: 12/6/20

# libraries
library(neuralnet)
library(Hmisc)

nn_df <- read.csv('./data_dir/edited_NN_csv_table.csv')


# remove outliers
dataframe <- nn_df[-64,]

# numeric index cols for months
Jan <- as.numeric(dataframe$month_name == "Jan")
Feb <- as.numeric(dataframe$month_name == "Feb")
Mar <- as.numeric(dataframe$month_name == "Mar")
Apr <- as.numeric(dataframe$month_name == "Apr")
May <- as.numeric(dataframe$month_name == "May")
Jun <- as.numeric(dataframe$month_name == "Jun")
Jul <- as.numeric(dataframe$month_name == "Jul")
Aug <- as.numeric(dataframe$month_name == "Aug")
Sep <- as.numeric(dataframe$month_name == "Sep")
Oct <- as.numeric(dataframe$month_name == "Oct")
Nov <- as.numeric(dataframe$month_name == "Nov")
Dec <- as.numeric(dataframe$month_name == "Dec")
# bind to the other_data_data
dataframe <- cbind(dataframe, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)

# other_data_data columns needed

# lag each column
income_1 <- Lag(dataframe$income, 1)
hours_worked_1 <- Lag(dataframe$hours_worked, 1)
billable_hours_1 <- Lag(dataframe$billable_hours, 1)
billable_amount_1 <- Lag(dataframe$billable_amount, 1)
new_biz_hours_1 <- Lag(dataframe$new_biz_hours, 1)
new_biz_amount_1 <- Lag(dataframe$new_biz_amount, 1)
num_of_time_entries_1 <- Lag(dataframe$num_of_time_entries, 1)
number_of_time_entries_billable_1 <- Lag(dataframe$number_of_time_entries_billable, 1)
number_of_new_biz_time_entries_1 <- Lag(dataframe$number_of_new_biz_time_entries, 1)
cost_of_goods_sold_1 <- Lag(dataframe$cost_of_goods_sold, 1)
expense_1 <- Lag(dataframe$expense, 1)

# new other_data_data.frame of neural network other_data_data
NN.data <- data.frame(dataframe$income, dataframe$month, income_1, hours_worked_1, billable_hours_1, billable_amount_1,
                      new_biz_hours_1, new_biz_amount_1, num_of_time_entries_1,
                      number_of_time_entries_billable_1, number_of_new_biz_time_entries_1,
                      cost_of_goods_sold_1, expense_1, Jan, Feb, Mar, Apr,
                      May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)


# omit nan
NN.data <- na.omit(NN.data)

samplesize <- 0.80 * nrow(NN.data)
set.seed(80)

index <- sample(seq_len(nrow(NN.data)), size = samplesize)
datatrain <- NN.data[index,]
datatest <- NN.data[-index,]
max <- apply(NN.data, 2, max)
min <- apply(NN.data, 2, min)
scaled <- as.data.frame(scale(NN.data, center = min, scale = max - min))
trainNN <- scaled[index,]
trainNN
testNN <- scaled[-index,]
set.seed(80)

NN <- neuralnet(dataframe.income ~ number_of_new_biz_time_entries_1 +
  billable_amount_1 +
  cost_of_goods_sold_1 +
  dataframe.month +
  Oct +
  Nov +
  Feb, trainNN, hidden = c(9,9,3), linear.output = T, stepmax = 1e7)

plot(NN)

#predicting test dataset
pr.nn <- compute(NN,testNN)
pr.nn
# Results from NN are normalized (scaled)
# Descaling for comparison
pr.nn_ <- pr.nn$net.result*(max(dataframe$income)-min(dataframe$income))+min(dataframe$income)
test.r <- (testNN$income)*(max(dataframe$income)-min(dataframe$income))+min(dataframe$income)
pr.nn_
#compute mse
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(testNN)
RMSE <- sqrt(MSE.nn)
RMSE






