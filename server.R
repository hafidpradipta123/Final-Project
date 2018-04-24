#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
#install.packages(c("plotly","shiny","ggplot2","tibble","readr","forcats","tidyverse"))


library(plotly)
library(shiny)
library(ggplot2)
library(nycflights13)
library(data.table)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(tibble)
library(readr)
library(forcats)

theme1 <- theme(
  plot.background = element_rect(fill = "#eeeeee"),
  panel.background = element_rect(fill = "#eeeeee"),
  legend.background = element_rect(fill = "#eeeeee"),
  legend.title = element_text(size = 12, family  = "Helvetica", face = "bold"),
  legend.text = element_text(size = 9, family = "Helvetica"),
  plot.title = element_text (size = 20, family = "Helvetica", face = "bold", hjust = 0.5),
  panel.grid.major = element_line(size = 0.4, linetype = "solid", color = "#cccccc"),
  panel.grid.minor = element_line(size = 0),
  axis.title = element_text(size = 14, family= "Helvetica", face = "bold"),
  axis.title.x = element_text(margin = margin(t = 20)),
  axis.title.y  = element_text(margin = margin(r = 20)),
  axis.ticks = element_blank()
  
)

## Import the data
results <- read_csv("multipleChoiceResponses.csv")

results$CurrentJobTitleSelect[results$CurrentJobTitleSelect=="Software Developer / Software Engineer"] <- "Sofware Engineer"
results$MajorSelect[results$MajorSelect == "Engineering (non-computer focused)"] <- "Engineering"

results$MajorSelect[results$MajorSelect == "Information technology, networking, or system administration"] <- "Information technology"

results$MajorSelect[results$MajorSelect == "Management information systems"] <- "Information systems"

results$MajorSelect[results$MajorSelect == "A health science"] <- "Health Science"

results$MajorSelect[results$MajorSelect == "A social science"] <- "Social Science"

results$MajorSelect[results$MajorSelect == "A humanities discipline"] <- "Humanities Discipline"



results_names <- names(results)
results_names
results_names[results_names == "WorkMethodsFrequencyA/B"] <- "WorkMethodsFrequencyABTesting"
results_names[results_names == "WorkMethodsFrequencyCross-Validation"] <- "WorkMethodsFrequencyCrossValidation"

names(results) <- results_names
results
names(results)#theme
clean <- results[,c(1:15,35,36,50:59,66:68,74:75,132,167:173,197:228)]
results$FormalEducation
results$FormalEducation[results$FormalEducation == "Master's degree"] <- "Master"
results$FormalEducation[results$FormalEducation == "MBachelor's degree"] <- "Bachelor"
results$FormalEducation[results$FormalEducation == "Doctoral degree"] <- "Doctoral"
results$FormalEducation[results$FormalEducation == "Some college/university study without earning a bachelor's degree"] <- "Non degree"
results$FormalEducation[results$FormalEducation == "Professional degree"] <- "Prof Degree"
results$FormalEducation[results$FormalEducation == "I prefer not to answer"] <- "Not Asnwer"
results$FormalEducation[results$FormalEducation == "I did not complete any formal education past high school"] <- "High School"



# Define server logic required to draw a histogram
shinyServer(function(input, output) {



output$text1 <- renderPrint({
  if (input$rbutt=="tedu"){
   paste(" Based on the graph, most of the title in the data scientist fields are dominated by Master's degree with the proportion around 45%. Software developer, Data Analyst, and Database Engineer are the title that employs most of the Bachelor's degree. However, around 57% of the scientist and researchers are Doctoral Degree. It seems that this position requires a high level of education. ") 
  }
  if (input$rbutt=="tedu"){
    print("The graph of job title and learning time seems not very informative for me. Most of the job title requires less than one year to be this position. However, most of the data scientists have Master's Degree a who needs at least one year of education. There are a little portion of the employees in scientist/researcher that require 10-15 years of learning time and it kind of make sense to me. I would not take a look more in-depth at this graph and move on to other variables. ")}
  if (input$rbutt=="tlt"){
    print("Most of the people in data scientist field are between 26-35 years old. This group age is a productive age, and the starting point of 26 is exciting because most of the Bachelor's Degree student will graduate around 22-23 years old. If they continue their Master's Degree for 1 or 2 years, they will be employed at 24-25 years old. I am wondering whether it is their first job or they do something else first and become data scientist later. ")}
  if (input$rbutt=="tage"){
    print("The visualization of Job Title and Salary shows that on average,  data scientists get high compensation. On average 40% of the salary for all title are six digits salary. For a programmer, half of the wage is below median household salary in the US which is 55k. I am curious what type of programmers they are.")}
  if (input$rbutt=="tft"){
    print("This visualization shows what kind of style of learning that they adopt when they first learn to be a data scientist. Interesting information in here is that on average less than 10% of all title learn the materials from work. It means that most of the data scientist already know what they are doing their job. In other words, what they learn to do is their job. Furthermore, most of them learn from Online courses with a small difference from different categories which are self-taught and university courses. The good news in here is that at least the materials to be a data scientist are on the internet")}
  if (input$rbutt=="tpe"){
    print("I am curious to see whether there is an inclination of parent's education that makes them a data scientist. I can say that most of their parents are entirely educated that has bachelor and master's degree. The graph is too broad to conclude any information.")}
  if (input$rbutt=="tjs"){
    print("Based on the graph of Job Satisfaction, most of the data scientists are satisfied with the score around 6.5 out of 9.9.")}
  
  if (input$rbutt=="tma"){
    print("Most of the people in data scientist field come from computer science or information technology major except for those who work as a statistician. They are from math or statistics major which are quite make sense. Even though most of them are form computer science, there are around 10% people from psychology or social science that become a data scientist.")}
})

  output$plot1 <- renderPlotly({

    
    if (input$rbutt=="tedu"){
      a <- results %>%
        rename(title = CurrentJobTitleSelect, Education =FormalEducation ) %>% 
        filter(title !="", Education != "") %>% 
        group_by(title, Education) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = title, y = freq, fill = Education, label = ifelse(freq>8, round(freq),"")))+
        ggtitle("Education vs Job Title")+
        labs(y= "Frequency (%)")+
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme1+
        theme(legend.title=element_blank())+
        theme(legend.position="right")+
        theme(axis.title.x = element_blank(),
              axis.text.x = element_text(angle = 325, hjust = 0))
      
      result <- ggplotly(a)
      }
    
    if(input$rbutt=="tlt"){
     a<- results %>%
        rename(title = CurrentJobTitleSelect, time =LearningDataScienceTime ) %>% 
        filter(title !="", time != "") %>% 
        group_by(title, time) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = title, y = freq, fill = time, label = ifelse(freq>8, round(freq),"")))+
        ggtitle("Learning Time VS Job Title")+
        labs(x = "Job Title", y= "Frequency (%)")+
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+  
        theme1+
        theme(legend.title=element_blank())+
        theme(axis.title.x = element_text(margin = margin(t=8)),
              axis.text.x = element_text(angle = 325, hjust = 0))
     result <- ggplotly(a)
      
    }
    if(input$rbutt=="tage"){
      a<- results %>%
        mutate(catage = cut(Age, c(0,15,20,25,30,35,40,45,50,55,100), right = FALSE,
                            labels = c("0-15","16-20","21-25","26-30","31-35","36-40","41-45","46-50","51-55","56++"))) %>% 
        rename(title = CurrentJobTitleSelect ) %>% 
        filter(title !="", catage != "") %>% 
        group_by(title, catage) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = title, y = freq, fill = catage, label = ifelse(freq>8, round(freq),"")))+
        ggtitle("Job Title vs Age")+
        labs(x = "Job Title", y= "Frequency (%)")+
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme1+
        theme(legend.title=element_blank())+
        theme(axis.title.x = element_text(margin = margin(t=8)),
              axis.text.x = element_text(angle = 325, hjust = 0))
    result <- ggplotly(a)
      }
    if(input$rbutt=="tsal"){
      results$CompensationAmount <- as.numeric(results$CompensationAmount)
      a <- results %>% 
        mutate(catcomp = cut(CompensationAmount,c(0,40000,50000,60000,70000,80000,90000,100000,110000,120000,130000,140000,Inf), right = FALSE, labels = c("0-40k","41-50k","51-60k","61-70k","71-80k","81-90k","91-100k","101-110k","111-120k","121-130k","131-140k","140k above"))) %>% 
        filter(CompensationCurrency=="USD") %>%
        rename(title = CurrentJobTitleSelect, salary = catcomp ) %>% 
        filter(title !="", salary != "") %>% 
        group_by(title, salary) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = title, y = freq, fill = salary, label = ifelse(freq>8, round(freq),"")))+
        ggtitle("Salary VS Job Title")+
        labs(x = "Job Title", y= "Frequency (%)")+
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme1+
        theme(legend.title=element_blank())+
        theme(axis.title.x = element_text(margin = margin(t=8)),
              axis.text.x = element_text(angle = 325, hjust = 0))
    result <- ggplotly(a)
      }
    if(input$rbutt=="tft"){
      a <- results %>%
        rename(title = CurrentJobTitleSelect, firsttrain =FirstTrainingSelect ) %>% 
        filter(title !="", firsttrain != "") %>% 
        group_by(title, firsttrain) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = title, y = freq, fill = firsttrain, label = ifelse(freq>8, round(freq),"")))+
        ggtitle("First training VS Job Title")+
        labs(x = "Job Title", y= "Frequency (%)")+
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme1+
        theme(legend.title=element_blank())+
        theme(axis.title.x = element_text(margin = margin(t=8)),
              axis.text.x = element_text(angle = 325, hjust = 0))
    result <- ggplotly(a)
      }
    if(input$rbutt=="tpe"){
      results$ParentsEducation <- as.factor(results$ParentsEducation)
      a <- results %>%
        rename(title = CurrentJobTitleSelect, pedu =ParentsEducation ) %>% 
        filter(title !="", pedu != "") %>% 
        group_by(title, pedu) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>%
        ggplot(aes(x = title, y = freq, fill = pedu, label = ifelse(freq>8, round(freq),"")))+
        ggtitle("Parents Education VS Job Title")+
        labs(x = "Job Title", y= "Frequency (%)")+
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme1+
        theme(legend.title=element_blank())+
        theme(axis.title.x = element_text(margin = margin(t=8)),
              axis.text.x = element_text(angle = 325, hjust = 0))
      
      result <- ggplotly(a)
    }
    if(input$rbutt=="tjs"){
      
     rename1 <- which(results$JobSatisfaction=="10 - Highly Satisfied")
     results$JobSatisfaction[rename1] <-  "9.9 - Highly Satisfied" 
     results$JobSatisfaction <- as.factor(results$JobSatisfaction)
     a <- results %>%
        rename(JobTitle = CurrentJobTitleSelect ) %>% 
        filter(JobTitle !="", JobSatisfaction != "") %>% 
        group_by(JobTitle, JobSatisfaction) %>% 
        summarise(n = n()) %>% 
        mutate(Frequency = n/sum(n)*100) %>% 
        ggplot(aes(x = JobTitle, y = Frequency, fill = JobSatisfaction, label = ifelse(Frequency>8, round(Frequency),"")))+
        ggtitle("Job Satisfaction VS Job Title")+
        labs(x = "Job Title", y= "Frequency (%)")+
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme1+
        theme(axis.title.x = element_text(margin = margin(t=8)),
              axis.text.x = element_text(angle = 325, hjust = 0))
    result <- ggplotly(a)
    }
    
    if (input$rbutt == "tma"){
      a <- results %>% 
        rename(Major = MajorSelect, title = CurrentJobTitleSelect) %>% 
        filter(title !="", Major !="") %>% 
        group_by(title,Major) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = title, y = freq, fill = Major, label = ifelse(freq>8, round(freq),"")))+
        ggtitle("Major vs Job Title")+
        labs(x = "Job Title", y= "Frequency (%)")+
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(axis.title.x = element_text(margin = margin(t=8)),
              axis.text.x = element_text(angle = 325, hjust = 0))
      result <- ggplotly(a)
      
    }
    
    
    
    
    print(result)
  })
   output$text2 <- renderPrint({
     if( input$rbutt2 == "lp"){
       print("Based on the visualization above, Projects are very useful and very popular to learn about data scientist. Personally, I agree with this poll because I learn a lot when I do something. Furthermore, it's followed by Courses, Kaggle and SO. It seems that online community and projects are the best combinations to learn to be a data scientist.")}
     if( input$rbutt2 == "jf"){
       print("Surprisingly, learning becomes a significant factor for data scientists to remain on their job. It follows by Salary, languages, and Office. I don't get about the language, but from what I extract, it seems that data scientist loves to learn and get paid well.")
     }
     if( input$rbutt2 == "jsi"){
       print("According to the survey, it is important to have skill in python as a data scientist followed by statistical skill. Then it is good to know about R, SQL, BigData, and Visualization. Technically a data scientist has to know everything. However, from this graph we can choose which one to start and how are we going to proceed to the next skill. In contrast, the degree is not important for this field with the score 0.58.")
     }
     if( input$rbutt2 == "wc"){
       print("When it comes to the challenge as a data scientist, dirty data is the most frequent and most challenging in this position. Talent is less challenging but quite frequent and similar to the Hiring funds. I am curious for those who answer Other Select. This category is quite challenging even though it is not often.")
     }
     if( input$rbutt2 == "wm"){
       print("Based on the work method importance and frequency. Data visualization is the most important and most frequently used by data scientist followed by Cross-Validation. It kind of make sense because data visualization and cross-validation are easy to understand. A graph is easy to read, and the cross-validation method compares between prediction rate or means squared error. Interesting information in this graph is most of the method used by data scientist are classification methods such as logistic regression, decision tree, and random forest. I don't know whether Kaggle didn't ask them about the quantitative method or they don't use a quantitative method such as regression on their job.")
     }
     if( input$rbutt2 == "wt"){
       print("This graph goes hand in hand with the previous graph about Job Skill Importance. It is obvious of python is an important skill, they will use python to work.")
     }
     
   })
  output$plot2 <- renderPlotly({
    
    
    if (input$rbutt2=="lp"){
      platforms <- grep("LearningPlatformUsefulness", names(results), value = T)
      
      names <- c()
      popularities <- c()
      scores <- c()
      
      for(k in platforms){
        usefulness <- results %>% 
          group_by_(k) %>% 
          count()
        
        popularity <- usefulness[[2]][1]+usefulness[[2]][2]+usefulness[[2]][3]
        
        score <- (usefulness[[2]][1]*0+usefulness[[2]][2]*0.5+usefulness[[2]][3]*1)/popularity
        
        names <- c(names, gsub("LearningPlatformUsefulness","",k))
        popularities <- c(popularities, popularity)
        scores <- c(scores, score)
      }
      scores_df <- data.frame(
        Popularity = popularities,
        Usefulness = scores,
        Name = names
      )
      scores_df
      
     result2a <-  ggplot(scores_df, aes(x = Usefulness, y = Popularity)) +
        ggtitle("Effectiveness of Learning Methods") +
        geom_point() +
        geom_text(aes(label = Name, family = "Helvetica"), nudge_y = 10)+
        theme1
    result2 <- ggplotly(result2a)
    result2
     }
    
    if (input$rbutt2=="jf"){
      JobFactor <- grep("JobFactor", names(results), value = T)
      JobSkillImportance <- grep("JobSkillImportance", names(results), value = T)
      WorkChallengeFrequency<- grep("WorkChallengeFrequency", names(results), value = T)
      JobFactor
      results[,167:172]
      JobFactor
      names.j <- c()
      importances <- c()
      scores.j <- c()
      
      for (k in JobFactor){
        weighted <- results %>% 
          group_by_(k) %>% 
          count()
        importance <- weighted[[2]][1]+weighted[[2]][2]+weighted[[2]][3]
        score <- (weighted[[2]][1]*0+weighted[[2]][2]*0.5+weighted[[2]][3]*1)/importance
        
        names.j <- c(names.j, gsub("JobFactor","",k))
        importances <- c(importances, importance)
        scores.j <- c(scores.j,score)
      }
      
      scores.df.j <- data.frame(
        Importance = importances,
        Scores = scores.j,
        Name = names.j
      )
      
      scores.df.j
     result2a <-  scores.df.j %>% 
        ggplot(aes(x= Scores, y= Importance ))+
        ggtitle("Job Factor Importance") +
        geom_point()+
        geom_text(aes(label = Name), nudge_y  = 5)+
        theme1
     result2 <- ggplotly(result2a)
     }
    
    
    if (input$rbutt2=="jsi"){
      JobSkillImportance <- grep("JobSkillImportance", names(results), value = T)
      names.js <- c()
      importances.js <- c()
      scores.js <- c()
      
      for (k in JobSkillImportance){
        weightedjs <- results %>%
          group_by_(k) %>% 
          count()
        importance.js <- weightedjs[[2]][1]+weightedjs[[2]][2]+weightedjs[[2]][3]
        score.js <- (weightedjs[[2]][1]*1+weightedjs[[2]][2]*0.5+weightedjs[[2]][3]*0)/importance.js
        names.js <- c(names.js, gsub("JobSkillImportance","",k))
        importances.js <- c(importances.js, importance.js)
        scores.js <- c(scores.js, score.js)
      }
      
      weightedjs[[1]]
      scores.df.js <- tibble(
        Importance = importances.js,
        Scores = scores.js,
        Name = names.js
      )
      scores.df.js <- scores.df.js[-c(11:13),]
      scores.df.js %>% 
        arrange(Scores)
      
      result2a <- scores.df.js %>% 
        ggplot(aes(x= Scores, y= Importance ))+
        ggtitle("Job Skill Importance") +
        geom_point()+
        geom_text(aes(label = Name), nudge_y  = 5)+
        theme1
      
      result2 <- ggplotly(result2a)
    }
    
    if (input$rbutt2=="wc"){
      WorkChallengeFrequency<- grep("WorkChallengeFrequency", names(results), value = T)
      names.wc <- c()
      importances.wc <- c()
      scores.wc <- c()
      
      for (k in WorkChallengeFrequency){
        weighted <- results %>%
          group_by_(k) %>% 
          count()
        importance.wc <- weighted[[2]][1]+weighted[[2]][2]+weighted[[2]][3]+weighted[[2]][4]
        score.wc <- (weighted[[2]][1]*1+weighted[[2]][2]*0.66+weighted[[2]][3]*0.33+weighted[[2]][4]*0)/importance.wc
        names.wc <- c(names.wc, gsub("WorkChallengeFrequency","",k))
        importances.wc <- c(importances.wc, importance.wc)
        scores.wc <- c(scores.wc, score.wc)
      }
      
      weighted[[1]]
      scores.df.wc <- tibble(
        Frequency = importances.wc,
        Scores = scores.wc,
        Name = names.wc
      )
      
      result2a <- scores.df.wc %>% 
        ggplot(aes(x= Scores, y= Frequency ))+
        ggtitle("Work Challenge") +
        geom_point()+
        geom_text(aes(label = Name), nudge_y  = 100)+
        #geom_point(aes(x= Scores, y= Frequency , color="blue"), data= scores.df.wm)+
        #geom_text(aes(label = Name), data= scores.df.wm, nudge_y  = 100, color = "blue")+
        theme1
      result2 <- ggplotly(result2a)}
    
    if (input$rbutt2=="wm"){
      WorkMethodsFrequency<- grep("WorkMethodsFrequency", names(results), value = T)
      
      names.wm <- c()
      importances.wm <- c()
      scores.wm <- c()
      
      for (k in WorkMethodsFrequency){
        weighted <- results %>%
          group_by_(k) %>% 
          count()
        importance.wm <- weighted[[2]][1]+weighted[[2]][2]+weighted[[2]][3]+weighted[[2]][4]
        score.wm <- (weighted[[2]][1]*1+weighted[[2]][2]*0.66+weighted[[2]][3]*0.33+weighted[[2]][4]*0)/importance.wm
        names.wm <- c(names.wm, gsub("WorkMethodsFrequency","",k))
        importances.wm <- c(importances.wm, importance.wm)
        scores.wm <- c(scores.wm, score.wm)
      }
      weighted[[1]]
      scores.df.wm <- tibble(
        Frequency = importances.wm,
        Importance = scores.wm,
        Name = names.wm
      )
      scores.df.wm <- scores.df.wm[-c(31:33),]
      
      result2a <- scores.df.wm %>% 
        ggplot(aes(x= Importance, y= Frequency ))+
        ggtitle("Work Method") +
        geom_point()+
        geom_text(aes(label = Name), nudge_y  = 100)+
        theme1
      
      result2 <- ggplotly(result2a)
    }
    
    if (input$rbutt2=="wt"){
      WorkToolsFrequency<- grep("WorkToolsFrequency", names(results), value = T)
      
      names.wt <- c()
      importances.wt <- c()
      scores.wt <- c()
      
      for (k in WorkToolsFrequency){
        weighted <- results %>%
          group_by_(k) %>% 
          count()
        importance.wt <- weighted[[2]][1]+weighted[[2]][2]+weighted[[2]][3]+weighted[[2]][4]
        score.wt <- (weighted[[2]][1]*1+weighted[[2]][2]*0.66+weighted[[2]][3]*0.33+weighted[[2]][4]*0)/importance.wt
        names.wt <- c(names.wt, gsub("WorkToolsFrequency","",k))
        importances.wt <- c(importances.wt, importance.wt)
        scores.wt <- c(scores.wt, score.wt)
      }
      
      weighted[[1]]
      scores.df.wt <- tibble(
        Frequency = importances.wt,
        Scores = scores.wt,
        Name = names.wt
      )
      
     result2a <-  scores.df.wt %>% 
        ggplot(aes(x= Scores, y= Frequency ))+
        ggtitle("Work Tools") +
        geom_point()+
        geom_text(aes(label = Name), nudge_y  = 100)+
        theme1
     
     result2 <- ggplotly(result2a)}
    
    print(result2)
    
    
    })
  
  output$plot3 <- renderPlotly({
    if (input$rbutt3=="mln"){
      
      a <- clean %>% group_by(MLMethodNextYearSelect) %>% 
        summarise(count = n()) %>%
        mutate(themethod = fct_reorder(as.factor(MLMethodNextYearSelect),-count)) %>% 
        slice(1:10) %>% 
        na.omit() %>% 
        ggplot(aes(x = themethod, y= count))+
        geom_bar(stat= "identity", fill = "midnightblue")+
        geom_text(aes(label = count), nudge_y = 200)+
        theme(axis.title.x = element_text(margin = margin(t=8)),
              axis.text.x = element_text(angle = 325, hjust = 0))
      result3 <- ggplotly(a)
      
    }
    if (input$rbutt3=="sa"){
      clean$CompensationAmount <- as.numeric(clean$CompensationAmount)
      a <- clean %>% 
        filter(Country == "United States") %>% 
        select(CompensationAmount) %>% 
        na.omit() %>% 
        ggplot(aes(x = CompensationAmount))+
        geom_histogram(fill = "blue")+
        ggtitle("Compensation in USD")+
      xlim(NA, 600000)+
        geom_density()+
        geom_vline(aes(xintercept=median(CompensationAmount)),
                   color="black", linetype="dashed", size=1)
      result3 <- ggplotly(a)
      result3
    }
    if (input$rbutt3=="dg"){
     a <-  clean %>% group_by(GenderSelect) %>% 
        summarise(count = n())%>% 
        na.omit() %>% 
        ggplot(aes(x= GenderSelect , y = count,fill = "midnightblue"))+
        ggtitle("Distribution of the Gender")+
        geom_bar(stat="identity")+
        coord_flip()+
        geom_text(aes(label = count), hjust = 0, color = "black", size = 4)+
        theme( plot.title = element_text (size = 20, family = "Helvetica", face = "bold", hjust = 0.5))+
        theme(legend.position="none")
     result3 <- ggplotly(a)
    
    }
    
    print(result3)
  })
})

rsconnect::setAccountInfo(name='hafidpradipta',
                          token='BBE53952BC403B7F474AC06B0C47FB89',
                          secret='sPB7yGK8xJmh2Xytpa1+9Nisfsj2hvwcrTTx2gbM')


