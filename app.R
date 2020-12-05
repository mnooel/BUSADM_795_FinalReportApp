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

# import libraries
library(shinydashboard)


# import data
data_ap_plt1 <- read.csv('data/edited_csv_table_dataaTimeMonth_v3.csv')
cols_ap_plt1 <- colnames(data_ap_plt1)


### FUNCTIONS ###
# functions to plot data

# general_functions



# analysis_plan
ap_render_plot1 <- function(dataframe, column_name) {
  plot <- plot(income ~ eval(as.name(column_name)), dataframe)

  plot
}


### SHINY UI ###

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
                    width = 2000),
        plotOutput('ex_plot', height = 650),
    )
    ,
    div(class = 'section',
        h4('HEADING 2'),
        includeHTML(path = 'sections/_REPLACE_ME/example_paragraph.html')
    )
  ),
)

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
        h4('Needed Items:'),
        includeHTML(path = 'sections/executive_summary/es_requirements.html')
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
    div(class = 'section', # todo remove needed items div from description_of_data
        h4('Needed Items:'),
        includeHTML(path = 'sections/description_of_data/dd_requirements.html')
    ),
    div(class = 'section',
        h4('Database Connection Engine'),
        includeHTML(path = 'sections/description_of_data/database_engines.html')
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
        includeHTML(path = 'sections/analysis_plan/ap_body_data_exploration1.html')
    ),
    div(class = 'section',
        selectInput(inputId = 'ap_plot1_select',
                    label = "Column to plot against income.",
                    selected = NULL,
                    choices = cols_ap_plt1
        ),

    ),
    div(class = 'section',
        plotOutput(outputId = 'ap_plot1', height = 650)
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
      div(class = 'section', # todo remove needed items div from implementation
          h4('Needed Items:'),
          includeHTML(path = 'sections/implementation/im_requirements.html')
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


  ui <- bootstrapPage(
    tags$head(includeCSS('styles.css')),
    navbarPage(theme = shinytheme('flatly'),
               header = tags$head(includeCSS(path = 'styles.css')),
               position = 'fixed-top',
               collapsable = FALSE,
               title = "Revenue Forcasting",
               id = 'nav',
               base_page_to_copy, # todo remove base_page_to_copy
               introduction,
               executive_summary,
               desctiption_of_data,
               analysis_plan,
               expectations,
               recomendations,
               implementation,
               next_steps,
               references
               # no trailing comma
    )
  )

  ### SHINY SERVER ###

  server <- function(input, output) {

    set.seed(122)
    histdata <- rnorm(100)

    output$ex_plot <- renderPlot({
      data <- histdata[seq_len(input$ex_slider)]
      plot <- hist(data)
      show(plot)
    })

    ### analysis_plan ###
    #ap_plot1
    output$ap_plot1 <- renderPlot({
      data <- read.csv(file = 'data/edited_csv_table_dataaTimeMonth_v3.csv')
      plot <- ap_render_plot1(data, input$ap_plot1_select)
      show(plot)
    })

  }

  shinyApp(ui, server)
