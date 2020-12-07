# Title     : Forcasting Revenue at an Advertising Agency.
# Objective : Explore the revenue forcasting capavilities of datasciece at an advertising agency utilizing their ERP database.
# Created by: Aaron Koss, Michael Noel
# Course : 216-795-004 - BUSADM 795 - Seminar-in-Management: Ideas and Applications of Data Science in Difference Fields
# Semseter : Fall 2020
# Authors : Aaron Koss, Michael Noel  todo: add email addresses?
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
aTimeMonth_v3 <- read.csv("data_dir/edited_csv_table_dataaTimeMonth_v3.csv")
Formal_aTimeMonth_v3 <- read.csv('data_dir/formal_csv_table_dataaTimeMonth_v3.csv')
nn_data_df <- read.csv(file = 'data_dir/NN_csv_table.csv')


### ANALYSIS PLAN FUNCTIONS ###
# ap_render_plot1 # todo move to analysis plan
ap_render_plot1 <- function(dataframe, column_name) {
  plot <- plot(dataframe$income ~ eval(as.name(column_name)),
               dataframe,
               main = paste('Income by', column_name),
               ylab = 'Income',
               xlab = column_name
  )
  plot
}

### SCRIPTS ###
source(file = 'sections/implementation/im_script1.R')
source(file = 'sections/implementation/im_script2.R')
#source(file = 'sections/implementation/im_script3.R')


### SHINY UI ###


# Introduction
introduction <- tabPanel(
  title = 'Intoduction',
  fluidPage(
    div(class = 'section_head'),
    div(class = 'section',
        h1('Introduction'),
    ),
    div(class = 'section',
        includeHTML(path = 'sections/introduction/int_body1.html')
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
        includeHTML(path = 'sections/executive_summary/es_requirements.html')
    ),
    div(class = 'section',
        includeHTML(path = 'sections/executive_summary/es_body1.html')
    ),
  ),
)

# Desctiption of Data Avaiable
desctiption_of_data <- tabPanel(
  title = 'Desc. of Data',
  fluidPage(
    div(class = 'section_head'),
    div(class = 'section',
        h1('Description of Data that is Redily Avaiable About the Issue'),
    ),
    div(class = 'section',
        h4('Needed Items:'),
        includeHTML(path = 'sections/description_of_data/dd_requirements.html')
    ),
    div(class = 'section',
        h4('Database Connection Engines'),
        includeHTML(path = 'sections/description_of_data/dd_body1.html')
    ),
    div(class = 'section',
        h4('Database Connection Engines'),
        includeHTML(path = 'sections/description_of_data/database_engines.html')
    ),
    div(class = 'section',
        h4('Database Connection Engines'),
        includeHTML(path = 'sections/description_of_data/dd_body2.html')
    ),

  ),
)

# Analysis Plan
analysis_plan <- tabPanel(
  title = 'Analysis Plan',
  fluidPage(
    div(class = 'section_head'),
    div(class = 'section',
        h1('Analysis Plan'),
    ),
    div(class = 'section', # todo remove needed items div from analysis_plan
        h4('Needed Items:'),
        includeHTML(path = 'sections/analysis_plan/ap_requirement.html')
    ),
    div(class = 'section',
        includeHTML(path = 'sections/analysis_plan/ap_body1.html')
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
        includeHTML(path = 'sections/analysis_plan/ap_body_2.html')
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
        includeHTML(path = 'sections/expectations/ex_requirements.html')
    ),
    # ex_body1.html
    div(class = 'section',
        includeHTML(path = 'sections/expectations/ex_body1.html')
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
        includeHTML(path = 'sections/recomendations/re_requirements.html')
    ),
    # re_body1.html
    div(class = 'section',
        includeHTML(path = 'sections/recomendations/re_body1.html')
    ),
  ),
)

im_plot6_choice1 <- 'Residuals Plot'
im_plot6_choice2 <- 'Residuals show constant variance'
im_plot6_choice3 <- 'There are no significant autocorrelations in the residuals'
im_plot6_choice4 <- 'There are no significant partial autocorrelations in the residuals'
im_plot6_choice5 <- 'The distribution of the residuals looks approximately normal'
im_plot6_choice6 <- 'The homoscedasticity, normality, and autocorrelation assumtions of linear regression appear to be satisfited byh the model we have choose.'

im_plot6_choices <- list(
  im_plot6_choice1, im_plot6_choice2, im_plot6_choice3, im_plot6_choice4, im_plot6_choice5, im_plot6_choice6
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
        includeHTML(path = 'sections/implementation/im_requirements.html')
    ),
    # im_body1.html
    div(class = 'section',
        includeHTML(path = 'sections/implementation/im_body1.html')
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
        includeHTML(path = 'sections/implementation/im_body2.html')
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
        includeHTML(path = 'sections/implementation/im_body3.html')
    ),
    # im_body_console1.html
    div(class = 'section',
        includeHTML(path = 'sections/implementation/im_body_console1.html')
    ),
    # im_body4.html: Model Diagnostics
    div(class = 'section',
        includeHTML(path = 'sections/implementation/im_body4.html')
    ),
    # im_plot3
    # todo add render text descriptions of the graphs
    div(class = 'section',
        selectInput(inputId = 'im_plot3_select',
                    label = "Modal Diagnostic Plots",
                    selected = NULL,
                    choices = im_plot3_choices,
                    width = '100%'
        ),
        plotOutput(outputId = 'im_plot3', height = 650),
    ),
    # im_body5.html: Plot Original vs Fitted Values
    div(class = 'section',
        includeHTML(path = 'sections/implementation/im_body5.html')
    ),
    # im_plot4
    div(class = 'section',
        plotOutput(outputId = 'im_plot4', height = 650),
    ),
    # im_body6.html
    div(class = 'section',
        includeHTML(path = 'sections/implementation/im_body6.html')
    ),
    # im_body7.html
    div(class = 'section',
        includeHTML(path = 'sections/implementation/im_body7.html')
    ),
    # im_plot5 todo plot rmse
    div(class = 'section',
        imageOutput(outputId = 'im_plot5', inline = TRUE),
    ),
    # im_body8.html
    div(class = 'section',
        includeHTML(path = 'sections/implementation/im_body8.html')
    ),
    # im_body_console2.html
    div(class = 'section',
        includeHTML(path = 'sections/implementation/im_body_console2.html')
    ),
    # im_body6.html
    div(class = 'section',
        includeHTML(path = 'sections/implementation/im_body9.html')
    ),
    # im_plot6 todo plot modal diagnostics for kfolds shit
    div(class = 'section',
        selectInput(inputId = 'im_plot6_select',
                    label = "Modal Diagnostic Plots",
                    selected = NULL,
                    choices = im_plot6_choices,
                    width = '100%'
        ),
        imageOutput(outputId = 'im_plot6', inline = TRUE),
    ),
    # im_body10.html
    div(class = 'section',
        includeHTML(path = 'sections/implementation/im_body10.html')
    ),
    # im_plot7 todo plot original vs fitted values
    div(class = 'section',
        plotOutput(outputId = 'im_plotnn', height = 650),
    ),
    # im_body11.html
    div(class = 'section',
        includeHTML(path = 'sections/implementation/im_body11.html'),
    ),
    # im_plot8 first neural network
    div(class = 'section',
        imageOutput(outputId = 'im_plot8', inline = TRUE)
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
        includeHTML(path = 'sections/next_steps/ns_requirements.html')
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
        includeHTML(path = 'sections/references/rf_references.html')
    ),
    div(class = 'section',
        includeHTML(path = 'sections/references/rf_body1.html')
    ),
  ),
)

# Base Demo Page
base_page_to_copy <- tabPanel(
  title = 'REPLACE ME',  # todo remove base_page_to_copy
  fluidPage(
    div(class = 'section_head'),
    div(class = 'section',
        span(),
        h1('BASE PAGE'),
    ),
    div(class = 'section',
        includeHTML(path = 'sections/_REPLACE_ME/example_paragraph.html')
    ),
    div(class = 'section',
        sliderInput(inputId = 'ex_slider',
                    label = 'Number of observations',
                    1,
                    100,
                    50,
                    width = '100%'),
        plotOutput('ex_plot', height = 650),
    )
    ,
    div(class = 'section',
        h4('HEADING 2'),
        includeHTML(path = 'sections/_REPLACE_ME/example_paragraph.html')
    )
  ),
)


ui <- bootstrapPage(
  tags$head(includeCSS('styles.css')),
  navbarPage(theme = shinytheme('flatly'),
             header = tags$head(includeCSS(path = 'styles.css')),
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
             references,
             base_page_to_copy
             # no trailing comma
  )
)

### SHINY SERVER ###

server <- function(input, output, session) {

  #set.seed(122)
  #histdata <- rnorm(100)
  #
  #output$ex_plot <- renderPlot({
  #  other_data_data <- histdata[seq_len(input$ex_slider)]
  #  plot <- hist(other_data_data)
  #  show(plot)
  #})

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

  #im_plot4
  output$im_plot4 <- renderPlot({
    plot <- im_render_plot4(aTimeMonth_v3)
    show(plot)
  })

  #im_plot5
  output$im_plot5 <- renderImage({
    filename <- "images/im_plot5.png"
    list(src = filename)
  }, deleteFile = FALSE)

  #im_plot6
  output$im_plot6 <- renderImage({

    if (input$im_plot6_select == im_plot6_choice1)


    filename <- im_render_plot6(input$im_plot6_select)
    list(
      src = filename
    )
  }, deleteFile = FALSE)

  #im_plot8 todo change label
  output$im_plot8 <- renderImage({

    filename <- "images/Rplot04.png"

    # Return a list containing information about the image
    list(
      src = filename
    )


  }, deleteFile = FALSE)

}

shinyApp(ui, server)

