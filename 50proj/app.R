library(shiny)
library(tidyverse)
library(ggplot2)
library(atus)
library(dplyr, warn.conflicts = FALSE)
library(ggforce)

#Load ATUS data
dtest <- read.csv("2016.csv")
model <- read.csv("2016.csv")

# Define UI for application that draws a histogram
ui <- navbarPage(
    "How Americans Spend Time During the Day",
    tabPanel("Home",
             titlePanel("Home"),
             p("I modelled the differences in time spent in different states.")),
    tabPanel("Model",
             h3("Working on data wrangling to fit the state level data to represent the averages, 
                some states may appear higher right now since they are overrepresented in the sample."),
             fluidPage(
                 #selectInput("x", "X variable", choices = names(dtest)),
                 selectInput("y", "Y variable", choices = names(dtest)),
                 selectInput("geom", "geom", c("histogram")),
                 #selectInput("geom", "geom", c("point", "column", "histogram", "jitter", "smooth")),
                 plotOutput("plot")
             )),
 #   tabPanel("State Comparison",
  #           h3("Time Usage Based on State Level data"),
   #          sidebarLayout(
    #             sidebarPanel(
     #                selectInput("select_state", "Select State",
      #                           choices = state)
       #          ),
#    mainPanel(
 #       plotOutput("Plot1")
  #      )
   # )
#    ),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("I chose to focus on how the average American spends their time. By looking at a wide range of
               activities such as how much someone sleeps in a day, the amount of housework they do, the time
               they spendon sporting events, and so forth. The time data can be organized by state, family income,
               education, age, and type of job.")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("This project utilizes data from the American Time Use Survey (ATUS) that collects 
               information on how respondents spend their time along with socioeconomic demographics.
               Through data visualizations and analysis, we are able to take a deeper dive into how
               Americans spend their time and how different variables such as income, location, or sex,
               among others can influence time usage."),
             h3("About Me"),
             p("My name is Hope Kudo and I study Government. 
             You can reach me at hopekudo@college.harvard.edu.")))


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    plot_geom <- reactive({
        switch(input$geom,
               point = geom_point(),
               histogram = geom_histogram(),
               smooth = geom_smooth(se = TRUE, na.rm = TRUE),
               jitter = geom_jitter()
        )
    })
    
    #new model
    output$plot <- renderPlot({
        ggplot(dtest, aes(x = state, .data[[input$y]])) +
            geom_col(stat = "identity", state = "fill") +
            theme_bw() +
            #plot_geom() +
            labs(title = "Demographic Distributions by State",
                 x = "States")
    }, res = 96)
    
#    output$Plot1 <- renderPlot({
#        data %>%
#            filter() %>%
#            ungroup(year) %>%
#            group_by(state) %>%
#            filter(state == input$select_state) %>%
#            ggplot(aes(x = year)) +
#            labs(y = "Percent") + 
#            ggtitle(input$select_state)
#    })
}

# Run the application 
shinyApp(ui = ui, server = server)
