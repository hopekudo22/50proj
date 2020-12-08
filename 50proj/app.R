library(shiny)
library(tidyverse)
library(ggplot2)
library(atus)
library(shinythemes)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(ggforce)

#Load ATUS data
fulldata <- read.csv("fullset.csv")

# Define UI for application that draws a histogram
ui <- navbarPage(
 
  theme = shinytheme("yeti"),
    "How Americans Spend Time During the Day",
    
  tabPanel("Sleep & Income Model",
             fluidPage(
               titlePanel("How Does Family Income Influence Sleep?"),
             fluidRow(column(12, 
                  h3("Distribution of Hours Slept Based on Income"),
                  h4("Determining the distribution of hours slept based on family 
                     income from survey responses 2012-2016"),
                  plotOutput("Plot1"))),
             fluidRow(column(12,
                             gt_output("regressiontable"))),
             fluidRow(column(12,
                             p("The regression table was created used a Bayesian generalized linear model, 
                               using stan_glm, to model the relation between the average amount of
                               hours slept during the night and family income. Based on the results,
                               I'm 95% confident that the true value of the")))
  
             )),
 
   tabPanel("State Comparison",
            fluidPage(
              fluidRow(column(12,
                       h3("Time Usage Distriubtions Based on State"),
                       p("To further analyze the data, I filtered the information for specific states."))),
              fluidRow(column(12,
                       h4("Activities by State"),
                       selectizeInput(inputId = "stateInput",
                                      label = "State",
                                      choices = unique(fulldata$state), #add data set
                                      selected = "Hawaii"),
                       plotOutput("Plot2")
                       )))),
    
    tabPanel("About",
             fluidPage(
               fluidRow(column(12,
                               h3("Project Background"),
                               p("This project utilizes data from the American Time Use Survey (ATUS) that collects 
                                 information on how respondents spend their time along with socioeconomic demographics.
                                 Through data visualizations and analysis, we are able to take a deeper dive into how
                                 Americans spend their time and how different variables such as income, location, or sex,
                                 among others can influence time usage. I modelled the differences in time spent in different states. I chose to focus on how the average American spends their time. 
                                 By looking at a wide range of activities such as how much someone sleeps in a day, the amount of housework they do, the time
                                 they spendon sporting events, and so forth. The time data can be organized by state, family income,
                                 education, age, and type of job."),
                               h3("Source Information"),
                               p("I used data from the American Time Usage Survey over the years of 2003 - 2017. I filtered
                                 the data in order to make it easier to work with, thus, the data is from the years 2012-2017."),
                               h3("About Me"),
                               p("My name is Hope Kudo and I'm a junior at Harvard studying Government and Psychology. As a proud
                               member of the Class of 2022 and Quincy House, living at home this semester has been quite a shake up,
                               but this project has been a great to delve into interesting data and a great way to experiment with R!
                               I'm excited to put the skills I've learned this semester into use. I can be reached at hopekudo@college.harvard.edu. 
                               The code for this project can be found at my", a("GitHub page here.", href = "https://github.com/hopekud22/50proj"))))
             )
    )
)
        


# Define server logic

server <- function(input, output, session) {
    
  output$Plot1 <- renderPlot({
    fulldata %>%
    ggplot(aes(x = sleep, fill = famincome)) +
      geom_histogram() +
      facet_wrap(~ famincome) +
      theme(axis.text = element_text(size = 5), strip.text = element_text(size = 7),
            panel.grid = element_blank(), panel.spacing.x = unit(3, "mm"),
            axis.ticks = element_blank(), axis.ticks.y = element_blank()) +
      labs(title = "Distribution of Hours Slept Based on Income",
           subtitle = "Determining the distribution of hours slept based on family income from survey responses 2012-2016",
           x = "Hours Slept",
           y = "",
           caption = "Source: ATUS data") +
      theme_linedraw()
  })
  
  
  output$regressiontable <- render_gt({
    set.seed(1000)
    fit_obj <- stan_glm(data = data,
                        formula = sleep ~ famincome,
                        refresh = 0)
    fit_obj %>%
      tbl_regression() %>%
      as_gt() %>%
      tab_header(title = "Regression of Family Income's Impact on Sleep") %>% 
      tab_source_note("Source: ATUS data") 
    
  })
  
    #new model
    output$plot2 <- renderPlot({
        ggplot(fulldata, aes(x = sleep, .data[[input$y]])) +
            geom_histogram(stat = "identity", state = "fill") +
            theme_bw() +
            labs(title = "Demographic Distributions by State",
                 x = "States")
    }, res = 96)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
