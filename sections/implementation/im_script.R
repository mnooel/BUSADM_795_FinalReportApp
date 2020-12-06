# Title     : im_script.R
# Objective : render plots related to the implentation section
# Created by: michaelnoel
# Created on: 12/5/20

library(Hmisc)

### dataframe functions ###

# dataframe to dataframe2
im_dataframe2 <- function(dataframe) {
  dataframe2 <- dataframe[-64,]
  dataframe2
}

# dataframe to dataframe3
im_dataframe3 <- function(dataframe) {
  dataframe3 <- im_dataframe2(dataframe)
  dataframe3$income_1 <- Lag(dataframe3$income, shift = 1)
  #seems like there is a spike somewhere in the 3rd quarter each year
  #for now lets try creating indicator variables for months,
  #may try quarters later
  Jan <- as.numeric(dataframe3$month_name == "Jan")
  Feb <- as.numeric(dataframe3$month_name == "Feb")
  Mar <- as.numeric(dataframe3$month_name == "Mar")
  Apr <- as.numeric(dataframe3$month_name == "Apr")
  May <- as.numeric(dataframe3$month_name == "May")
  Jun <- as.numeric(dataframe3$month_name == "Jun")
  Jul <- as.numeric(dataframe3$month_name == "Jul")
  Aug <- as.numeric(dataframe3$month_name == "Aug")
  Sep <- as.numeric(dataframe3$month_name == "Sep")
  Oct <- as.numeric(dataframe3$month_name == "Oct")
  Nov <- as.numeric(dataframe3$month_name == "Nov")
  Dec <- as.numeric(dataframe3$month_name == "Dec")
  #combine with dataframe
  dataframe3 <- cbind(dataframe3, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)
  dataframe3
}

# lagout1
im_lagout1 <- function(dataframe2) {
  lagout <- lm(income ~ income_1, data = dataframe2)
  lagout
}

# lagout2
im_lagout2 <- function(dataframe3) {
  Jan <- as.numeric(dataframe3$month_name == "Jan")
  Feb <- as.numeric(dataframe3$month_name == "Feb")
  Mar <- as.numeric(dataframe3$month_name == "Mar")
  Apr <- as.numeric(dataframe3$month_name == "Apr")
  May <- as.numeric(dataframe3$month_name == "May")
  Jun <- as.numeric(dataframe3$month_name == "Jun")
  Jul <- as.numeric(dataframe3$month_name == "Jul")
  Aug <- as.numeric(dataframe3$month_name == "Aug")
  Sep <- as.numeric(dataframe3$month_name == "Sep")
  Oct <- as.numeric(dataframe3$month_name == "Oct")
  Nov <- as.numeric(dataframe3$month_name == "Nov")
  Dec <- as.numeric(dataframe3$month_name == "Dec")
  t2 <- seq(1, 93)
  lagout2 <- lm(income ~ income_1 +
    t2 +
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
    Nov, data = dataframe3)  # todo does DEC need to be included
  lagout2
}


### im_plot1 ###

# im_plot1_choices
im_plot1_choice1 <- '1. Visualization of Core Creative Monthly Income from Jan 2013 to October 2020'
im_plot1_choice2 <- '2. Income v Time - Idenficication of Outlier'
im_plot1_choice3 <- '3. Income v Time - w/o Outlier'
im_plot1_choice4 <- '4. Visualization of Core Creative Monthly Income from Jan 2012 to October 2020 w/o Outlier'

im_plot1_choices <- list(
  im_plot1_choice1, im_plot1_choice2, im_plot1_choice3, im_plot1_choice4
)

# todo maybe fill in dataframe 2 function

# im_render_plot1_choice1 Time_Series_Analysis.R Line 2-6
im_render_plot1_choice1 <- function(dataframe) {
  t <- seq(1, 94)
  plot <- ts.plot(dataframe$income, ylab = "Monthly Income in Dollars", xlab = "Time")
  lines(predict(lm(dataframe$income ~ t)), col = 'red')
  plot
}

# im_render_plot1_choice2 Time_Series_Analysis.R Line 20
im_render_plot1_choice2 <- function(dataframe) {
  t <- seq(1, 94)
  plot <- plot(dataframe$income ~ t, ylab = "Monthly Income in Dollars", xlab = "Time")
  plot
}

# im_render_plot1_choice3 Time_Series_Analysis.R Line 53
im_render_plot1_choice3 <- function(dataframe) {
  dataframe2 <- im_dataframe2(dataframe)
  t2 <- seq(1, 93)
  plot <- plot(dataframe2$income ~ t2, ylab = "Monthly Income in Dollars", xlab = "Time")
  plot
}

# im_render_plot1_choice4
im_render_plot1_choice4 <- function(dataframe) {
  dataframe2 <- dataframe[-64,]
  t2 <- seq(1, 93)
  plot <- ts.plot(dataframe2$income, ylab = "Monthly Income in Dollars", xlab = "Time")
  lines(predict(lm(dataframe2$income ~ t2)), col = 'green')
  plot
}

# im_render_plot1
im_render_plot1 <- function(dataframe, choice) {
  if (choice == im_plot1_choice1) {
    plot <- im_render_plot1_choice1(dataframe)
    plot
  }
  if (choice == im_plot1_choice2) {
    plot <- im_render_plot1_choice2(dataframe)
    plot
  }
  if (choice == im_plot1_choice3) {
    plot <- im_render_plot1_choice3(dataframe)
    plot
  }
  if (choice == im_plot1_choice4) {
    plot <- im_render_plot1_choice4(dataframe)
    plot
  }
}


### im_plot2 ###

# im_plot2_a Time_Series_Analysis.R Lines 2-6
im_plot2_choice1 <- 'ACF of Income'
im_plot2_choice2 <- 'PACF'

im_plot2_choices <- list(
  im_plot2_choice1, im_plot2_choice2
)

# im_render_plot2_choice1 Time_Series_Analysis.R Line 53
im_render_plot2_choice1 <- function(dataframe) {
  dataframe2 <- dataframe[-64,]
  acf_plot <- acf(dataframe2$income, main = "ACF") # afc_4 golden
  acf_plot
}

# im_render_plot2_choice2 Time_Series_Analysis.R Line 55
im_render_plot2_choice2 <- function(dataframe) {
  dataframe2 <- dataframe[-64,]
  pacf_plot <- pacf(dataframe2$income, main = "PACF") # pafc_1 golden
  pacf_plot
}

# im_render_plot2
im_render_plot2 <- function(dataframe, choice) {
  if (choice == im_plot2_choice1) {
    plot <- im_render_plot2_choice1(dataframe)
    plot
  }
  if (choice == im_plot2_choice2) {
    plot <- im_render_plot2_choice2(dataframe)
    plot
  }
}

### im_plot3 ###

# im_plot3_a Time_Series_Analysis.R Lines 2-6
im_plot3_choice1 <- 'im_plot3_choice1'
#im_plot3_choice2 <- 'PACF'
#im_plot3_choice3 <- 'ACF of Income'
#im_plot3_choice4 <- 'PACF'
#im_plot3_choice5 <- 'ACF of Income'
#im_plot3_choice6 <- 'PACF'

im_plot3_choices <- list(
  im_plot3_choice1 # im_plot3_choice2, im_plot3_choice3, im_plot3_choice4, im_plot3_choice5, im_plot3_choice6
)


# im_render_plot3_choice1 Time_Series_Analysis.R Line 112-113
im_render_plot3_choice1 <- function(dataframe) {
  dataframe3 <- im_dataframe3(dataframe)
  plot <- ts.plot(residuals(im_lagout2(dataframe3)),
                  ylab = "Residuals",
                  main = "Residuals Plot"
  )
  abline(h = 0, col = "blue")
  plot
}

## im_render_plot3_choice2 Time_Series_Analysis.R Line 118
#im_render_plot3_choice2 <- function(dataframe) {
#  dataframe2 <- dataframe[-64,]
#  acf_plot <- acf(dataframe2$income, main = "ACF")
#  acf_plot
#}
#
## im_render_plot3_choice3 Time_Series_Analysis.R Line 114
#im_render_plot3_choice3 <- function(dataframe) {
#  dataframe2 <- dataframe[-64,]
#  acf_plot <- acf(dataframe2$income, main = "ACF")
#  acf_plot
#}
#
## im_render_plot3_choice4 Time_Series_Analysis.R Line 115
#im_render_plot3_choice4 <- function(dataframe) {
#  dataframe2 <- dataframe[-64,]
#  acf_plot <- acf(dataframe2$income, main = "ACF")
#  acf_plot
#}
#
## im_render_plot3_choice5 Time_Series_Analysis.R Line 116
#im_render_plot3_choice5 <- function(dataframe) {
#  dataframe2 <- dataframe[-64,]
#  acf_plot <- acf(dataframe2$income, main = "ACF")
#  acf_plot
#}
#
## im_render_plot3_choice6 Time_Series_Analysis.R Line 117
#im_render_plot3_choice6 <- function(dataframe) {
#  dataframe2 <- dataframe[-64,]
#  acf_plot <- acf(dataframe2$income, main = "ACF")
#  acf_plot
#}

# im_render_plot3
im_render_plot3 <- function(dataframe, choice) {
  if (choice == im_plot3_choice1) {
    plot <- im_render_plot3_choice1(dataframe)
    plot
  }
}
