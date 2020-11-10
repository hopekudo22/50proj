library(shiny)
library(tidyverse)
library(ggplot2)
library(atus)
library(dplyr, warn.conflicts = FALSE)
library(ggforce)

#Load ATUS data
dtest <- read.csv("HI.csv")

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Final Project Title",
    tabPanel("Home",
             titlePanel("Home"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("Model",
             fluidPage(
                 selectInput("x", "X variable", choices = names(dtest)),
                 selectInput("y", "Y variable", choices = names(dtest)),
                 selectInput("geom", "geom", c("point", "column", "jitter")),
                 plotOutput("plot")
             )),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello, this is where I talk about my project."),
             h3("About Me"),
             p("My name is Hope Kudo and I study Government. 
             You can reach me at ______@college.harvard.edu.")))


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    plot_geom <- reactive({
        switch(input$geom,
               point = geom_point(),
               #smooth = geom_smooth(se = TRUE, na.rm = TRUE),
               jitter = geom_jitter()
        )
    })
    
    output$plot <- renderPlot({
        ggplot(dtest, aes(.data[[input$x]], .data[[input$y]])) +
            plot_geom()
    }, res = 96)
}

# Run the application 
shinyApp(ui = ui, server = server)
