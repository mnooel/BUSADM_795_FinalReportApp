# Title     : Revenue Forcasting todo expand title
# Objective : todo expand objective
# Created by: Michael Noel, University of Wisconsin - Milwaukee
# Created on: 12/3/20

# load required packages
if (!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if (!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if (!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")

# import data
library(shinydashboard)


### SHINY UI ###


base_page_to_copy <- tabPanel(
  title = 'REPLACE ME',
  fluidPage(
    div(class = 'section',
        h1('BASE PAGE'),
    ),
    div(class = 'section',
        h4('HEADING 1:'),
        includeHTML(path = 'sections/_REPLACE_ME/example_paragraph.html')
    ),
    div(class = 'section',
        sliderInput(inputId = 'ex_slider',
                    label = 'Number of observations',
                    1,
                    100,
                    50,
                    width = 2000),
        plotOutput('ex_plot', height = 800),
    )
    ,
    div(class = 'section',
        h4('HEADING 2'),
        includeHTML(path = 'sections/_REPLACE_ME/example_paragraph.html')
    )
  ),
)

# Executive Summary
executive_summary <- tabPanel(
  title = 'Executive Summary',
  fluidPage(
    div(class = 'section',
        h1('Executive Summary'),
    ),
    div(class = 'section',
        h4('Needed Items:'),
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
    div(class = 'section',
        h1('Description of Data that is Redily Avaiable About the Issue'),
    ),
    div(class = 'section',
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
    div(class = 'section',
        h1('Analysis Plan'),
    ),
    div(class = 'section',
        h4('Needed Items:'),
        includeHTML(path = 'sections/analysis_plan/ap_requirement.html')
    ),
  ),
)

# Expectations
expectations <- tabPanel(
  title = 'Expectations',
  fluidPage(
    div(class = 'section',
        h1('Expectations'),
    ),
    div(class = 'section',
        h4('Needed Items'),
        includeHTML(path = 'sections/expectations/ex_requirements.html')
    ),
  ),
)

# Recomendations
recomendations <- tabPanel(
  title = 'Recomendations',
  fluidPage(
    div(class = 'section',
        h1('Recomendations'),
    ),
    div(class = 'section',
        h4('Needed Items:'),
        includeHTML(path = 'sections/recomendations/re_requirements.html')
    ),
  ),
)

# Implementation
implementation <- tabPanel(
  title = 'Implementation',
  fluidPage(
    div(class = 'section',
        h1('Implementation'),
    ),
    div(class = 'section',
        h4('Needed Items:'),
        includeHTML(path = 'sections/implementation/im_requirements.html')
    ),
  ),
)

# Next Steps
next_steps <- tabPanel(
  title = 'Next Steps',
  fluidPage(
    div(class = 'section',
        h1('Next Steps'),
    ),
    div(class = 'section',
        h4('Needed Items:'),
        includeHTML(path = 'sections/next_steps/ns_requirements.html')
    ),
  ),
)

# References
references <- tabPanel(
  title = 'References',
  fluidPage(
    div(class = 'section',
        h1('References'),
    ),
    div(class = 'section',
        h4('Needed Items:'),
        includeHTML(path = 'sections/references/rf_references.html')
    ),
  ),
)


ui <- bootstrapPage(
  tags$head(includeCSS('styles.css')),
  navbarPage(theme = shinytheme('flatly'),
             header = tags$head(includeCSS(path = 'styles.css')),
             collapsable = FALSE,
             title = "Revenue Forcasting",
             id = 'nav',
             base_page_to_copy, # todo remove base_page_to_copy
             executive_summary,
             desctiption_of_data,
             analysis_plan,
             expectations,
             recomendations,
             implementation,
             next_steps,
             references
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


}

shinyApp(ui, server)
