# Title     : Forcasting Revenue at an Advertising Agency.
# Objective : Explore the revenue forcasting capavilities of datasciece at an advertising agency utilizing their ERP database.
# Created by: Aaron Koss, Michael Noel
# Course : 216-795-004 - BUSADM 795 - Seminar-in-Management: Ideas and Applications of Data Science in Difference Fields
# Semseter : Fall 2020
# Authors : Aaron Koss, Michael Noel
# Created on: 12/3/20


# load required packages
if (!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if (!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if (!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if (!require(Hmisc)) install.packages("Himsc", repos = "http://cran.us.r-project.org")
if (!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if (!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if (!require(leaps)) install.packages("leaps", repos = "http://cran.us.r-project.org")
if (!require(neuralnet)) install.packages("neuralnet", repos = "http://cran.us.r-project.org")


# import libraries
library(shinydashboard)
library(Hmisc)
library(neuralnet)


# import other_data_data
aTimeMonth_v3 <- read.csv("./data_dir/edited_csv_table_dataaTimeMonth_v3.csv")
Formal_aTimeMonth_v3 <- read.csv('./data_dir/ap_plot1_data.csv')
nn_data_df <- read.csv(file = './data_dir/NN_csv_table.csv')


### ANALYSIS PLAN FUNCTIONS ###
# ap_render_plot1 # todo move to analysis plan
ap_render_plot1 <- function(dataframe, column_name) {
  plot <- plot(dataframe$Income ~ eval(as.name(column_name)),
               dataframe,
               main = paste('Income by', column_name),
               ylab = 'Income',
               xlab = column_name
  )
  plot
}

### SCRIPTS ###
source(file = './sections/implementation/im_script1.R')
source(file = './sections/implementation/im_script2.R')
#source(file = './sections/implementation/im_script3.R')


### SHINY UI ###


# Introduction # todo done
introduction <- tabPanel(
  title = 'Intoduction',
  fluidPage(
    div(class = 'section_head'),
    div(class = 'section',
        h1('Introduction'),
    ),
    div(class = 'section',
        includeHTML(path = './sections/introduction/int_body1.html')
    ),
  ),
)

# Executive Summary
executive_summary <- tabPanel(
  title = 'Executive Summary',
  fluidPage(
    div(class = 'section_head'),
    div(class = 'section',
        h1('Executive Summary'),
    ),
    div(class = 'section', # todo remove needed items div from executive_summary
        includeHTML(path = './sections/executive_summary/es_requirements.html')
    ),
    div(class = 'section',
        includeHTML(path = './sections/executive_summary/es_body1.html')
    ),
  ),
)

# Desctiption of Data Avaiable
# todo table output
desctiption_of_data <- tabPanel(
  title = 'Desc. of Data',
  fluidPage(
    div(class = 'section_head'),
    div(class = 'section',
        h1('Description of Data that is Redily Avaiable About the Issue'),
    ),
    div(class = 'section',
        h4('Needed Items:'),
        includeHTML(path = './sections/description_of_data/dd_requirements.html')
    ),
    div(class = 'section',
        h4('Database Connection Engines'),
        includeHTML(path = './sections/description_of_data/dd_body1.html')
    ),
    div(class = 'section',
        includeHTML(path = './sections/description_of_data/database_engines.html')
    ),
    div(class = 'section',
        h4('Database Connection Engines'),
        includeHTML(path = './sections/description_of_data/dd_body2.html')
    ),

  ),
)

# Analysis Plan
# todo reference proper table
# todo add hyperlink
# todo btm padding
analysis_plan <- tabPanel(
  title = 'Analysis Plan',
  fluidPage(
    div(class = 'section_head'),
    div(class = 'section',
        h1('Analysis Plan'),
    ),
    div(class = 'section', # todo remove needed items div from analysis_plan
        h4('Needed Items:'),
        includeHTML(path = './sections/analysis_plan/ap_requirement.html')
    ),
    div(class = 'section',
        includeHTML(path = './sections/analysis_plan/ap_body1.html')
    ),
    div(class = 'section',
        selectInput(inputId = 'ap_plot1_select',
                    label = "Column to plot against income.",
                    selected = NULL,
                    choices = colnames(Formal_aTimeMonth_v3),
                    width = "100%"
        ),
        plotOutput(outputId = 'ap_plot1', height = 650)

    ),
    div(class = 'section',
        includeHTML(path = './sections/analysis_plan/ap_body_2.html')
    ),
  ),
)

# Expectations
expectations <- tabPanel(
  title = 'Expectations',
  fluidPage(
    div(class = 'section_head'),
    div(class = 'section',
        h1('Expectations'),
    ),
    div(class = 'section', # todo remove needed items div from expectations
        h4('Needed Items'),
        includeHTML(path = './sections/expectations/ex_requirements.html')
    ),
    # ex_body1.html
    div(class = 'section',
        includeHTML(path = './sections/expectations/ex_body1.html')
    ),
  ),
)

# Recomendations
recomendations <- tabPanel(
  title = 'Recomendations',
  fluidPage(
    div(class = 'section_head'),
    div(class = 'section',
        h1('Recomendations'),
    ),
    div(class = 'section', # todo remove needed items div from recomendations
        h4('Needed Items:'),
        includeHTML(path = './sections/recomendations/re_requirements.html')
    ),
    # re_body1.html
    div(class = 'section',
        includeHTML(path = './sections/recomendations/re_body1.html')
    ),
  ),
)

# Implementation
implementation <- tabPanel(
  title = 'Implementation',
  fluidPage(
    div(class = 'section_head'),
    div(class = 'section',
        h1('Implementation'),
    ),
    # requirements section
    div(class = 'section', # todo remove needed items div from implementation
        h4('Needed Items:'),
        includeHTML(path = './sections/implementation/im_requirements.html')
    ),
    # im_body1.html
    div(class = 'section',
        includeHTML(path = './sections/implementation/im_body1.html')
    ),
    # im_plot1
    div(class = 'section',
        selectInput(inputId = 'im_plot1_select',
                    label = "Outlier Removal Discovery",
                    selected = NULL,
                    choices = im_plot1_choices,
                    width = '100%'
        ),
        plotOutput(outputId = 'im_plot1', height = 650),
    ),
    # im_body2.html
    div(class = 'section',
        includeHTML(path = './sections/implementation/im_body2.html')
    ),
    # im_plot2
    div(class = 'section',
        selectInput(inputId = 'im_plot2_select',
                    label = "New Title here",  # todo new title here
                    selected = NULL,
                    choices = im_plot2_choices,
                    width = '100%'
        ),
        plotOutput(outputId = 'im_plot2', height = 650),
    ),
    # im_body3.html
    div(class = 'section',
        includeHTML(path = './sections/implementation/im_body3.html')
    ),
    # im_body_console1.html
    div(class = 'section',
        includeHTML(path = './sections/implementation/im_body_console1.html')
    ),
    # im_body4.html: Model Diagnostics
    div(class = 'section',
        includeHTML(path = './sections/implementation/im_body4.html')
    ),
    # im_plot3
    div(class = 'section',
        selectInput(inputId = 'im_plot3_select',
                    label = "Modal Diagnostic Plots",
                    selected = NULL,
                    choices = im_plot3_choices,
                    width = '100%'
        ),
        plotOutput(outputId = 'im_plot3', height = 650),
        htmlOutput(outputId = 'im_plot3_desc', inline = TRUE),
    ),
    # im_body5.html: Plot Original vs Fitted Values
    div(class = 'section',
        includeHTML(path = './sections/implementation/im_body5.html')
    ),
    # im_plot4
    div(class = 'section',
        plotOutput(outputId = 'im_plot4', height = 650),
    ),
    # im_body6.html
    div(class = 'section',
        includeHTML(path = './sections/implementation/im_body6.html')
    ),
    # im_body7.html
    div(class = 'section',
        includeHTML(path = './sections/implementation/im_body7.html')
    ),
    # im_plot5
    div(class = 'section',
        imageOutput(outputId = 'im_plot5', inline = TRUE),
    ),
    # im_body8.html
    div(class = 'section',
        includeHTML(path = './sections/implementation/im_body8.html')
    ),
    # im_body_console2.html
    div(class = 'section',
        includeHTML(path = './sections/implementation/im_body_console2.html')
    ),
    # im_body6.html
    div(class = 'section',
        includeHTML(path = './sections/implementation/im_body9.html')
    ),
    # im_plot6
    div(class = 'section',
        selectInput(inputId = 'im_plot6_select',
                    label = "Modal Diagnostic Plots",
                    selected = NULL,
                    choices = im_plot3_choices,
                    width = '100%'
        ),
        imageOutput(outputId = 'im_plot6', inline = TRUE),
    ),
    # im_body10.html
    div(class = 'section',
        includeHTML(path = './sections/implementation/im_body10.html')
    ),
    # im_plot7 todo plot original vs fitted values
    div(class = 'section',
        imageOutput(outputId = 'im_plot7', inline = TRUE),
    ),
    # im_body11.html
    div(class = 'section',
        includeHTML(path = './sections/implementation/im_body11.html'),
    ),
    ## im_plot8 first neural network
    #div(class = 'section',
    #    imageOutput(outputId = 'im_plot8', inline = TRUE)
    #),
    # im_plot9
    div(class = 'section',
        sliderInput("im_plot9_slider1", "layer 1 neurons", 1, 10, 4, width = '100%'),
        sliderInput("im_plot9_slider2", "layer 2 neurons", 0, 10, 0, width = '100%'),
        sliderInput("im_plot9_slider3", "layer 3 neurons", 0, 10, 0, width = '100%'),
        sliderInput('im_plot9_slider4', 'test population %', 10, 90, 80, width = '100%'),


        plotOutput(outputId = 'im_plot9', height = 1000),
    ),
    div(class = 'section',
        includeHTML(path = './sections/implementation/im_body12.html')
    ),
  ),
)

# Next Steps
next_steps <- tabPanel(
  title = 'Next Steps',
  fluidPage(
    div(class = 'section_head'),
    div(class = 'section',
        h1('Next Steps'),
    ),
    div(class = 'section', # todo remove needed items div from next_steps
        h4('Needed Items:'),
        includeHTML(path = './sections/next_steps/ns_requirements.html')
    ),
    div(class = 'section',
        includeHTML(path = './sections/next_steps/ns_body1.html')
    ),
  ),
)

# References
references <- tabPanel(
  title = 'References',
  fluidPage(
    div(class = 'section_head'),
    div(class = 'section',
        h1('References'),
    ),
    div(class = 'section',  # todo remove needed items div from references
        h4('Needed Items:'),
        includeHTML(path = './sections/references/rf_references.html')
    ),
    div(class = 'section',
        includeHTML(path = './sections/references/rf_body1.html')
    ),
  ),
)

## Base Demo Page
#base_page_to_copy <- tabPanel(
#  title = 'REPLACE ME',  # todo remove base_page_to_copy
#  fluidPage(
#    div(class = 'section_head'),
#    div(class = 'section',
#        span(),
#        h1('BASE PAGE'),
#    ),
#    div(class = 'section',
#        includeHTML(path = './sections/_REPLACE_ME/example_paragraph.html')
#    ),
#    div(class = 'section',
#        sliderInput(inputId = 'ex_slider',
#                    label = 'Number of observations',
#                    1,
#                    100,
#                    50,
#                    width = '100%'),
#        plotOutput('ex_plot', height = 650),
#    )
#    ,
#    div(class = 'section',
#        h4('HEADING 2'),
#        includeHTML(path = './sections/_REPLACE_ME/example_paragraph.html')
#    )
#  ),
#)


ui <- bootstrapPage(
  tags$head(includeCSS('./styles.css')),
  navbarPage(theme = shinytheme('flatly'),
             header = tags$head(includeCSS(path = './styles.css')),
             position = 'fixed-top',
             collapsable = FALSE,
             title = "Revenue Forcasting",
             id = 'nav',
             #base_page_to_copy, # todo remove base_page_to_copy
             introduction,
             executive_summary,
             desctiption_of_data,
             analysis_plan,
             expectations,
             recomendations,
             implementation,
             next_steps,
             references
             #base_page_to_copy
             # no trailing comma
  )
)

### SHINY SERVER ###

server <- function(input, output, session) {


  ### analysis_plan ###
  #ap_plot1
  output$ap_plot1 <- renderPlot({
    plot <- ap_render_plot1(Formal_aTimeMonth_v3, input$ap_plot1_select)
    show(plot)
  })

  ### implementation ###
  #im_plot1
  output$im_plot1 <- renderPlot({
    plot <- im_render_plot1(aTimeMonth_v3, input$im_plot1_select)
    show(plot)
  })
  #im_plot2
  output$im_plot2 <- renderPlot({
    plot <- im_render_plot2(aTimeMonth_v3, input$im_plot2_select)
    show(plot)
  })

  #im_plot3
  output$im_plot3 <- renderPlot({
    plot <- im_render_plot3(aTimeMonth_v3, input$im_plot3_select)
    show(plot)
  })

  output$im_plo3_desc <- renderText({
    desc <- im_render_plot3_desc(input$im_plot3_select)
    desc
  })

  #im_plot4
  output$im_plot4 <- renderPlot({
    plot <- im_render_plot4(aTimeMonth_v3)
    show(plot)
  })

  #im_plot5
  output$im_plot5 <- renderImage({
    filename <- "./images/im_plot5.png"
    list(src = filename)
  }, deleteFile = FALSE)

  #im_plot6
  output$im_plot6 <- renderImage({


    print(input$im_plot6_select)

    filename <- im_render_plot6(im_plot3_choices, input$im_plot6_select)
    print(filename)
    list(
      src = filename
    )
  }, deleteFile = FALSE)

  #im_plot7
  output$im_plot7 <- renderImage({

    filename <- "./images/im_plot7.png"

    # Return a list containing information about the image
    list(
      src = filename
    )


  }, deleteFile = FALSE)

  # neural network visualization
  output$im_plot9 <- renderImage({

    # plot the NN
    plot_neural_network <- function(nn_dataframe, layer1, layer2, layer3, test_per) {

      ### layers ###
      layers <- list(layer1, layer2, layer3)
      neuron_vec <- c()

      for (layer in layers) {
        if (layer == 0) { }
        else(neuron_vec <- append(neuron_vec, layer))
      }
      print(neuron_vec)

      # test percentage
      test_percentage <- (test_per / 100)

      ### neural network ###
      # remove outliers
      dataframe <- nn_dataframe[-64,]

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

      samplesize <- test_percentage * nrow(NN.data)
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
        Feb, trainNN, hidden = neuron_vec, linear.output = T, stepmax = 1e7)


      # adjust to height of session
      width <- session$clientData$output_im_plot9_width
      height <- session$clientData$output_im_plot9_height

      # A temp file to save the output. It will be deleted after renderImage
      # sends it, because deleteFile=TRUE.
      outfile <- tempfile(fileext = '.png')

      png(outfile, width = width, height = height)
      dev.off()
      print(plot(NN))
      #dev.print(outfile)
      dev.print(png, outfile, width = width, height = height)


      list(src = outfile,
           width = width,
           height = height,
           alt = "Rendered Neural Network")


    }

    plot_neural_network(nn_data_df,
                        input$im_plot9_slider1,
                        input$im_plot9_slider2,
                        input$im_plot9_slider3,
                        input$im_plot9_slider4
    )
  }, deleteFile = TRUE)

}

shinyApp(ui, server)
