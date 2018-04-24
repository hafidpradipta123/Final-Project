
library(shiny)
library(plotly)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Data Science Survey"),
   tabsetPanel(
     tabPanel("Description of the App", 
              h3("Exploratory Data Analysis of Kaggle ML and Data Science Survey, 2017 "),
              h5("The code is taken from  https://www.kaggle.com/jackcook/how-to-become-a-data-scientist . 
I tried to play with it because the code is very efficient and tidy. I am curious to figure out each layer of the code. "),
              h5("Kaggle conducted industry-wide sruvey that receives 16,716 usable respondents from 171 countries and territories. The survey was live from August 7th to August 25th."),
              h5("
                 
                 The dataset contain 228 variables. However some of the variables are the extension of one information. 
                 For example all variables started with WorkToolsFrequency refers to what kind of the work tools that the employee used often for their job."),
              h5("
                 
                 In general, this dataset contains several group of variables: "),
              h5("Demographic: Gender, Age, Country, Status as student, Title, Salary, Type of employment, and Education. "),
              h5("
                 
                 Learning platform used: what kind of platform that they use to learn data science. "),
              h5("
                 Language recommendation : Which programming language that they recommend."),
              h5(" 
                 Job Skill importance : which skils are important for their career. "),
              h5("
                 Learning Category: How do they learn to be data scientist"),
              h5("
                 Work Tools Frequency : What kind of tools that they use the most. "),
              h5("
                 Work Method Frequency : What method that they use the most. "),
              h5("
                 Time : How do they spind most of their time during their job"),
              h5("
                 Work Challenge Frequency : what are the challenges to be data scientist."),
              h5(" 
                 Job Factor : what are the factors that make them a data scientist."),
              h5(" 
                 
                 This analysis is the extension of the Jack Cook's code that has created an efficient code to extract the information in this dataset. It begins by setting th theme of the visualization and try to wrangle the data by rename several outcome to make it simpler. "),
              h5(" Please click to the tab in this app to see the visualization based on their characteristic. I will include my own explanation below the plot. 
")
              ),
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
        plotlyOutput("plot1"),
        textOutput("text1")
        
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
          plotlyOutput("plot2"),
         textOutput("text2")
        ) ),
    
    tabPanel("Descriptive Statistics", sidebarPanel(
      radioButtons(inputId="rbutt3","Type of the plot:",
                   choices = c("Method Next Year"="mln",
                               "Distribution of Salary"="sa"
                               ,"Distribution of the Gender"="dg"
                               ))),
      mainPanel(
        plotlyOutput("plot3")))
 
  )))
  
  # Sidebar with a slider input for number of bins 

library(rsconnect)