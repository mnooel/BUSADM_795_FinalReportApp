# Title     : im_script.R
# Objective : render plots related to the implentation section
# Created by: michaelnoel
# Created on: 12/5/20

### im_plot1 ###

# im_plot1_a Time_Series_Analysis.R Lines 2-6
im_plot1_choice1 <- '1. Visualization of Core Creative Monthly Income from Jan 2013 to October 2020'
im_plot1_choice2 <- '2. Income v Time - Idenficication of Outlier'
im_plot1_choice3 <- '3. Income v Time - w/o Outlier'
im_plot1_choice4 <- '4. Visualization of Core Creative Monthly Income from Jan 2012 to October 2020 w/o Outlier'

im_plot1_choices <- list(
  im_plot1_choice1, im_plot1_choice2, im_plot1_choice3, im_plot1_choice4
)

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
  dataframe2 <- dataframe[-64,]
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
