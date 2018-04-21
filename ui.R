
library(shiny)
library(plotly)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Data Science Survey"),
   tabsetPanel(
    tabPanel("Comparison Based on Title",
      sidebarPanel(
        radioButtons(inputId="rbutt","Type of the plot:",
                     choices = c("Title vs Education"="tedu",
                                 "Title vs Learning Time"="tlt",
                                 "Title vs Age"="tage",
                                 "Title vs Salary"="tsal",
                                 "First training based on Title"="tft",
                                 "Title vs Parent Education"="tpe",
                                 "Title vs Job Satisfaction"="tjs",
                                 "Title vs Major" = "tma"))),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotlyOutput("plot1")
        
      )
    ) ,
    tabPanel("Frequency and Popularity" ,sidebarPanel(
        radioButtons(inputId="rbutt2","Type of the plot:",
                     choices = c("Learning Platform"="lp",
                                 "Job Factor"="jf",
                                 "Job Skill Importance"="jsi",
                                 "Work Chalenge"="wc",
                                 "Work Methods"="wm",
                                 "Work tools"="wt"
                                 ))),
        mainPanel(
          plotlyOutput("plot2")
        ) ),
    
    tabPanel("Descriptive Statistics", sidebarPanel(
      radioButtons(inputId="rbutt3","Type of the plot:",
                   choices = c("Method Next Year"="mln",
                               "Distribution of Salary"="sa",
                               "Distribution of the Gender"="dg",
                               "Title Vs Salary"="tsal",
                               "First training based on Title"="tft",
                               "Title VS Parent Education"="tpe",
                               "Title Vs Job Satisfaction"="tjs"))),
      mainPanel(
        plotlyOutput("plot3")))
 
  )))
  
  # Sidebar with a slider input for number of bins 

library(rsconnect)