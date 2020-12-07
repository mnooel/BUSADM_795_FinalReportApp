# Title     : TODO
# Objective : TODO
# Created by: michaelnoel
# Created on: 12/6/20

# libraries
library(neuralnet)
library(Hmisc)


df <- read.csv('NN_csv_table.csv')

# define dataframe3 object
setClass("im_NeuralNetwork",
         slots = list(

         # serries inputs into the neural net
         Jan = "numeric",
         Feb = "numeric",
         Mar = "numeric",
         May = "numeric",
         Apr = "numeric",
         Jun = "numeric",
         Jul = "numeric",
         Aug = "numeric",
         Sep = "numeric",
         Oct = "numeric",
         Nov = "numeric",
         Dec = "numeric",
         income_1 = "numeric",
         hours_worked_1 = "numeric",
         billable_hours_1 = "numeric",
         billable_amount_1 = "numeric",
         new_biz_hours_1 = "numeric",
         new_biz_amount_1 = "numeric",
         num_of_time_entries_1 = "numeric",
         number_of_time_entries_billable_1 = "numeric",
         number_of_new_biz_time_entries_1 = "numeric",
         cost_of_goods_sold_1 = 'numeric',
         expense_1 = 'numeric',

         # other attribures
         samplesize = 'numeric',
         seed = 'numeric',
         hidden = 'numeric',
         data = 'data.frame',
         data_nn = 'data.frame'

         # todo add plot or something here
         )
)

init_im_NeuralNetwork_from_df <- function(dataframe,
                                          sample_size_percent) {

  # remove outliers
  dataframe2 <- dataframe[-64,]

  # numeric index cols for months
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
  # bind to the data
  data <- cbind(dataframe2, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)

  # lag each column
  income_1 <- Lag(data$income, 1)
  hours_worked_1 <- Lag(data$hours_worked, 1)
  billable_hours_1 <- Lag(data$billable_hours, 1)
  billable_amount_1 <- Lag(data$billable_amount, 1)
  new_biz_hours_1 <- Lag(data$new_biz_hours, 1)
  new_biz_amount_1 <- Lag(data$new_biz_amount, 1)
  num_of_time_entries_1 <- Lag(data$num_of_time_entries, 1)
  number_of_time_entries_billable_1 <- Lag(data$number_of_time_entries_billable, 1)
  number_of_new_biz_time_entries_1 <- Lag(data$number_of_new_biz_time_entries, 1)
  cost_of_goods_sold_1 <- Lag(data$cost_of_goods_sold, 1)
  expense_1 <- Lag(data$expense, 1)

  # new data.frame of neural network data
  data_nn <- data.frame(data$income, data$month, income_1, hours_worked_1, billable_hours_1, billable_amount_1,
                        new_biz_hours_1, new_biz_amount_1, num_of_time_entries_1,
                        number_of_time_entries_billable_1, number_of_new_biz_time_entries_1,
                        cost_of_goods_sold_1, expense_1, Jan, Feb, Mar, Apr,
                        May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)


  # omit nan
  data_nn <- na.omit(data_nn)


  # sample and split data
  samplesize <- 0.80 * nrow(data_nn)
  set.seed(80)

  index <- sample(seq_len(nrow(data_nn)), size = samplesize)
  datatrain <- data_nn[index,]
  datatest <- data_nn[-index,]
  max <- apply(data_nn, 2, max)
  min <- apply(data_nn, 2, min)
  scaled <- as.data.frame(scale(data_nn, center = min, scale = max - min))
  trainNN <- scaled[index,]
  trainNN
  testNN <- scaled[-index,]
  set.seed(80)
  nn <- neuralnet(data.income ~ number_of_new_biz_time_entries_1 +
    billable_amount_1 +
    cost_of_goods_sold_1 +
    data.month +
    Oct +
    Nov +
    Feb, trainNN, hidden = 4, linear.output = T, stepmax = 1e7)
  plot <- plot(nn)
  show(plot)

  # set attributes
  neural_network <- new("im_NeuralNetwork",
                        Jan = Jan,
                        Feb = Feb,
                        Mar = Mar,
                        Apr = Apr,
                        May = May,
                        Jun = Jun,
                        Jul = Jul,
                        Aug = Aug,
                        Sep = Sep,
                        Oct = Oct,
                        Nov = Nov,
                        Dec = Dec,
                        income_1 = income_1,
                        hours_worked_1 = hours_worked_1,
                        billable_hours_1 = billable_hours_1,
                        billable_amount_1 = billable_amount_1,
                        new_biz_hours_1 = new_biz_hours_1,
                        new_biz_amount_1 = new_biz_amount_1,
                        num_of_time_entries_1 = num_of_time_entries_1,
                        number_of_time_entries_billable_1 = number_of_time_entries_billable_1,
                        number_of_new_biz_time_entries_1 = number_of_new_biz_time_entries_1,
                        cost_of_goods_sold_1 = cost_of_goods_sold_1,
                        expense_1 = expense_1,
                        samplesize = 0.80,
                        seed = 80,
                        hidden = 4,
                        data = data,
                        data_nn = data_nn
  )

  # return object
  return(neural_network)
}

df <- read.csv('NN_csv_table.csv')

neural_network <- init_im_NeuralNetwork_from_df(dataframe = df)


#create train and test dataset
NN.data <- neural_network@data_nn

samplesize <- 0.80 * nrow(neural_network@data_nn)
set.seed(80)
index <- sample(seq_len(nrow(neural_network@data_nn)), size = samplesize)
datatrain <- neural_network@data_nn[index,]
datatest <- neural_network@data_nn[-index,]
max <- apply(neural_network@data_nn, 2, max)
min <- apply(neural_network@data_nn, 2, min)
scaled <- as.data.frame(scale(neural_network@data_nn, center = min, scale = max - min))
trainNN <- scaled[index,]
trainNN
testNN <- scaled[-index,]
set.seed(80)
NN <- neuralnet(neural_network@data$income ~
                  neural_network@number_of_new_biz_time_entries +
                    neural_network@billable_amount_1 +
                    neural_network@cost_of_goods_sold_1 +
                    neural_network@data$month +
                    neural_network@Oct +
                    neural_network@Dec +
                    neural_network@Feb, trainNN, hidden = neural_network@hidden, linear.output = T, stepmax = 1e7)
plot(NN)
#predicting test dataset
pr.nn <- compute(NN, testNN)
pr.nn
# Results from NN are normalized (scaled)
# Descaling for comparison
pr.nn_ <- pr.nn$net.result * (max(coredata2$income) - min(coredata2$income)) + min(coredata2$income)
test.r <- (testNN$income) * (max(coredata2$income) - min(coredata2$income)) + min(coredata2$income)
pr.nn_
#compute mse
MSE.nn <- sum((test.r - pr.nn_)^2) / nrow(testNN)
RMSE = sqrt(MSE.nn)
RMSE