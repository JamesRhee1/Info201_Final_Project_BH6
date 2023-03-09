# Loading required libraries
library(shiny)
library(tidyverse)
library(readr)
library(dplyr)
library(plotly)
library(DT)

# data set for plot 1 and 2 (James)
sat <- read_delim("data/SAT Report 2015-2016.csv")
naRemoved <- sat %>% 
  filter(!is.na(sat$cname),!is.na(as.numeric(sat$AvgScrRead)),
         !is.na(as.numeric(sat$AvgScrMath)), !is.na(as.numeric(sat$AvgScrWrit)))
county <- unique(naRemoved$cname)

# data set for plot 3 (Regina)
data = read.csv("data/SAT Report 2015-2016.csv",sep=",")
data = data[data$index!=0,]
data[,7:14] <- lapply(data[,7:14],as.numeric)
data <- data%>%na.omit()

ui <- fluidPage(
    titlePanel("California SAT Scores Evaluation"),
    tabsetPanel(
      # First tab of the app
      tabPanel("Project Overview",
               p(strong("Project Purpose:"), style = "font-size:30px;"),
               p("The SAT is a standardized test 
                 commonly used in the United States for college admissions."), 
               p("The test is designed to assess a student's knowledge and skills
                 in reading, writing, and mathematics, and is often a requirement 
                 for admission to many colleges and universities."),  
               p("Analyzing the SAT scores of California students by region and 
                 school can help identify many disparities in education quality and access."), 
               p("By analyzing SAT 
                 scores and number of test takers, people can better understand the 
                 challenges and needs of different regions and schools and how to 
                 best help them."),
               p(strong("Data Source:"), style = "font-size:30px;"),
               p("This data set was taken from the kaggle SAT Results 
                 dataset which contains the number of SAT test takers in California
                 and average scores by county, district, and school."),
               uiOutput("tab"),
               mainPanel(
                 img(src="image.jpg"),
               )
               ),
      # Second tab: Plot made by James
      tabPanel("Average SAT Scores over the counties",
        sidebarLayout(
        sidebarPanel(
          selectInput(
            "county", "select a desired county",
            choices = county,
            selected = 1
          ),
          radioButtons(
            "subject", "select a desired subject",
            choices = list("Reading", "Math", "Writing"),
            selected = "Reading"
          )
        ),
        mainPanel(
          plotOutput("plot1"),
          textOutput("text1"),
            tags$head(tags$style("#text1{font-size: 30px;}"))  
        )
        )
      ),
      # Third tab: Divya's plot
      tabPanel("Number of SAT takers",
        sidebarLayout(
          sidebarPanel(
            selectInput(inputId = "level", label = "Select level:", 
                        choices = c("County", "District"))
                 ),
                 mainPanel(
                   plotlyOutput(outputId = "plot2"),
                   textOutput("text2"),
                    tags$head(tags$style("#text2{font-size: 30px;}"))
                 )
               )      
      ),
      # Fourth tab: Regina's plot
      tabPanel("High Achieving District/County",
               sidebarLayout(
                 sidebarPanel(h4("Distribute of PctGE1500 in different variable"),
                    radioButtons("type1","choose plot type?",c("Bubble","bar")),
                    radioButtons("var2","choose variable(plot):",
                                c("dname" ,"cname"))),
                mainPanel(
                #show the result in each panel
                  plotlyOutput("plot3"),
                  textOutput("text3"),
                    tags$head(tags$style("#text3{font-size: 30px;}"))
                  )
                 )
               ),
      # Fifth tab: Conclusion
      tabPanel("Conclusion",
               p(strong("Conclusion:"), style = "font-size:30px;"),
               p("A pattern that was discovered is that the districts and 
                 counties with a higher number of test takers also have a 
                 positive correlation in test scores overall and higher scores 
                 across each subject."),
               p("The pattern was most noticeable in the graph that showed 
                 the district with the highest number of students who scored 
                 above 1500, with those same districts also having 
                 the highest number of test takers, and highest scores in specific subjects."),
               p("This illustrates that there is a strong link between school 
                 district and SAT scores and it highlights the importance of 
                 educational equity."),
               p(strong("Quality of Data:"), style = "font-size:30px;"),
               p("The dataset seems to be clear and reasonable."),
               p("The data includes information on average scores 
                  and number of test takers by school, district, 
                  and county which are important variables for 
                  understanding academic achievement."),
               p("However, it is important to note that this dataset only 
                 includes data from California, so the findings may not be 
                 generalizable to other states or regions."),
               p(strong("Future Ideas:"), style = "font-size:30px;"),
               p("Future research could include datasets that explore potential 
                 solutions to address the disparities in SAT scores based on 
                 geographic or demographic factors."),
               p("This could include targeted interventions such as test preparation 
                 programs, outreach to underserved communities, or 
                 changes to the test itself to see how it makes it more inclusive 
                 and reflective of diverse student populations.")
               )
    )
)

server <- function(input, output) {
  # First plot codes by James
  output$plot1 <- renderPlot({
    if (input$subject == "Reading") {
      naRemoved %>% 
        filter(cname == input$county) %>% 
        ggplot(aes(AvgScrRead, NumTstTakr)) +
        geom_col(aes(fill=factor(AvgScrRead))) +
        ggtitle("Average Reading Score and Number of Test takers in county: ", input$county)
    } else if (input$subject == "Math") {
      naRemoved %>% 
        filter(cname == input$county) %>% 
        ggplot(aes(AvgScrMath, NumTstTakr)) +
        geom_col(aes(fill=factor(AvgScrMath))) +
        ggtitle("Average Math Score and Number of Test takers in county: ", input$county)
    } else {
      naRemoved %>% 
        filter(cname == input$county) %>% 
        ggplot(aes(AvgScrWrit, NumTstTakr)) +
        geom_col(aes(fill=factor(AvgScrWrit)))+
        ggtitle("Average Writing Score and Number of Test takers in county: ", input$county)
    }
  })
  output$text1 <- renderPrint({
    cat("The graph shows the average scores of selected subject from
        selected county, the size of each column is decided by 
        the number of test takers")
  })
  
  # Second plot by Divya
  # filter data based on user input
  filtered_data <- reactive({
    if (input$level == "County") {
      naRemoved %>%
        group_by(cname) %>%
        summarize(Total = (NumTstTakr))
    } else {
      naRemoved %>%
        group_by(dname) %>%
        summarize(Total = (NumTstTakr))
    }
  })
  # Create plotly bar chart
  output$plot2 <- renderPlotly({
    plot_ly(data = filtered_data(), x = ~Total, y = ~as.character(filtered_data()[[1]]),
            type = "bar", orientation = "v") %>%
      layout(xaxis = list(title = "Total Test Takers"),
             yaxis = list(title = input$level))
  })
  output$text2 <- renderPrint({
    if (input$level == "County") {
      cat("This graph shows the total number of test takers for each county in California.")
    } else {
      cat("This graph shows the total number of test takers for each district in California.")
    }
  })
  
  # Plot 3 by Regina
  output$plot3 <- renderPlotly({
    if (input$type1=="bar"){
      data1 <- data%>%select(input$var2,PctGE1500)
      colnames(data1)[1] <- "var"
      plot_ly(data1,x = ~var, y = ~PctGE1500, type = "bar")
    } else{
      data1 <- data%>%select(input$var2,PctGE1500,NumGE1500)
      colnames(data1)[1] <- "var"
      ggplotly(ggplot(data=data1,aes(y=NumGE1500, x=PctGE1500, size=PctGE1500, color=var)) +
                 geom_point(alpha=0.5))
    }
  })
  output$text3 <- renderPrint({
    if (input$var2 == "dname") {
      cat("This plot shows the number of people in each district that reached 1500.")
    } else {
      cat("This plot shows the number of people in each counties that reached 1500.")
    }
  })
  
  # Hyperlink for the opening page
  url <- a("SAT Test Results Over the Years from kaggle.com",
           href = "https://www.kaggle.com/datasets/thedevastator/unlocking-achievement-understanding-california-s?select=SAT+Report+2015-2016.csv")
  output$tab <- renderUI({
    tagList("Data set from kaggle.com", url)
  })
}

shinyApp(ui = ui, server = server)
