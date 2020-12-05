#ts plot check for randomness----
dataframe <- read.csv("data/edited_csv_table_dataaTimeMonth_v3.csv")
head(dataframe)
t <- seq(1, 94)
plot <- ts.plot(dataframe$income, ylab = "Monthly Income in Dollars", xlab="Time")
plot
lines(predict(lm(dataframe$income~t)), col='green')

#appears non-random, looks like positive trend and possible seasonality
#test to confirm non-randomness
#test autocorrelation
acf(dataframe$income, main = "ACF of Income")
#Ljung boxtest for autocorrelation
Box.test(dataframe$income, type="Ljung", lag=10)
#p-value is significant, reject null hypothesis 
#that first 10 autocorrelations equal 0

#nonrandom time series using simple regression----
#create time variable

plot(dataframe$income ~ t, ylab = "Monthly Income in $")
slrout <- lm(income ~ t, data = dataframe)
summary(slrout)
#model diagnostics
ts.plot(residuals(slrout))
abline(h = 0, col = "blue")
#residuals appear random over time
acf(residuals(slrout), main = "Residuals")
hist(residuals(slrout), main = "")
qqnorm(residuals(slrout), pch = 19)
#outlier detected
#remove outlier by creating new index
max(dataframe$income)
print(dataframe$income)
coredata2 <- dataframe[-c(64), ]
#redo slr without outlier----
t2 <- seq(1, 93)
ts.plot(coredata2$income, ylab = "Monthly Income in Dollars")
lines(predict(lm(coredata2$income~t)),col='green')
plot(coredata2$income ~ t2, ylab = "Income", xlab = "Time")
slrout2 <- lm(income ~ t2, data = coredata2)
summary(slrout2)
#model diagnostics
ts.plot(residuals(slrout2))
abline(h = 0, col = "blue")
#residuals appear random over time
acf(residuals(slrout2), main = "Residuals")
hist(residuals(slrout2), main = "")
qqnorm(residuals(slrout2), pch = 19)
#plot income time series with fitted regression line
ts.plot(coredata2$income, ylab = "Monthly Income $")
abline(slrout2, col = "red")
#Lagged based regression----
#find any autocorrelations
acf(coredata2$income, main = "ACF")
#acf shows significant autocorrelation at t-1:t-5
pacf(coredata2$income, main = "PACF")
#PACF suggests using t-l as predictor variable
#use Hmisc library to add lag 1 predictor variable
library(Hmisc)
coredata2$income_1 <- Lag(coredata2$income, shift = 1)
lagout <- lm(income ~ income_1, data = coredata2)
summary(lagout)
#model diagnostics
ts.plot(residuals(lagout))
abline(h = 0, col = "blue")
hist(residuals(lagout), main = "")
qqnorm(residuals(lagout), pch = 19)
#looks like positive skew, there is uptrend still
ts.plot(coredata2$income, ylab = "Monthly Income $")
lines(predict(lm(income ~ income_1, col = 'red', data = coredata2)))
#Accounting for seasonality----
#graph to see
library(ggplot2)
library(ggrepel)
ggplot(data = coredata2, aes(x = t2, y = income))+
  geom_line() +
  geom_point() +
  geom_text_repel(aes(label = month), size = 2.5)
#seems like there is a spike somewhere in the 3rd quarter each year
#for now lets try creating indicator variables for months, 
#may try quarters later
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
str(coredata2)
outind <- lm(income ~ t2 + Jan + Feb + Mar + Apr + May + Jun + Jul + 
               Aug + Sep + Oct + Nov, data = coredata2)
summary(outind)
#model diagnostics
ts.plot(residuals(outind))
abline(h = 0, col = "blue")
acf(residuals(outind), main = "Residuals")
pacf(residuals(outind), main = "Residuals")
#looks like a significant autocorrelation at t-1
library(Hmisc)
coredata2$income_1 <- Lag(coredata2$income, shift = 1)
lagout2 <- lm(income ~ income_1 + t2 + Jan + Feb + Mar +
                Apr + May + Jun + Jul + Aug + Sep + Oct +
                Nov, data = coredata2)
summary(lagout2)
#model diagnostics
ts.plot(residuals(lagout2), ylab = "Residuals", main = "Residuals Plot")
abline(h = 0, col = "blue")
acf(residuals(lagout2), main = "Residuals, Autocorrelation")
pacf(residuals(lagout2), main = "Residuals, Partial Autocorrelation")
hist(residuals(lagout2), xlab = "residuals(model)", main = "Residuals")
qqnorm(residuals(lagout2), main = "Normal Q-Q Plot Residuals", pch = 19)
plot(fitted(lagout2) ~ residuals(lagout2), xlab = "Residuals", ylab = "Fitted")
#looks approximately normal
#next need to compare fitted to actual,

coredata2$fitted <- append(NA, fitted(lagout2))
fitted_ <- ts(coredata2$fitted)
income_ <- ts(coredata2$income)
fitted_[2:93]
income_
#plot fitted v original
ts.plot(income_, fitted_, lty = c(2,1), col = c("black","blue"))
legend("topleft", c("Original", "Fitted"), lty = c(2,1),
       col = c("black", "blue"), cex = 0.75)
#RMSE values
MSE <- sum((fitted_[2:93] - income_[2:93])^2)/92
RMSE <- sqrt(MSE)
RMSE

