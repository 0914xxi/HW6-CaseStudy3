## R Shiny Dashboard of “Case Study3_Updated” ##


#Load libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(rpart)
library(rpart.plot)

# data
diabetes <- read.csv("diabetes.csv")
head(diabetes)

diabetes2 <- diabetes %>%
  mutate(
    Age = cut(Age, 
              breaks = c(0, 18, 36, 56, 150), 
              labels = c("Youth","Young","MidAge","Old"),
              include.lowest = T),
    BMI = cut(BMI, 
              breaks = c(0, 18.5, 25, 30, 100), 
              labels = c("Low","Normal","Overweight","Obesity"),
              include.lowest = T)
  ) %>%
  mutate(Age = factor(Age),
         BMI = factor(BMI),
         Outcome = factor(Outcome))

yvar <- "Outcome"
xvars <- setdiff(colnames(diabetes2), yvar)
catvars <- colnames(diabetes2)[sapply(diabetes2, is.factor)]
catxs <- setdiff(catvars, yvar)
numvars <- colnames(diabetes2)[!(sapply(diabetes2, is.factor))]
numxs <- setdiff(numvars, yvar)

newxcolumn <- function(xname, data){
  x <- data[, xname]
  if (is.numeric(x)) {
    column(width = 4,
           numericInput(inputId = xname, 
                        label = xname, 
                        value = median(x)))
  } else {
    column(width = 4,
           selectInput(
             inputId = xname, 
             label = xname, 
             choices = unique(x)))
  }
}

# logistic reg
fit_lr <- glm(
  Outcome ~ .,
  data = diabetes2,
  family = binomial
)


###############################################################


#R Shiny ui
ui <- dashboardPage(
  skin = "red",
  #Dashboard title
  dashboardHeader(title = 'Project Updated by Xi', titleWidth = 290),
  
  #Sidebar layout
  dashboardSidebar(
    width = 300,
    sidebarMenu(menuItem("About", 
                         tabName = "about", 
                         icon = icon('list-alt')),
                menuItem("Tutorial", 
                         tabName = "tutorial", 
                         icon = icon('location-arrow')),
                menuItem("Plot for Univariate", 
                         tabName = "plots", 
                         icon = icon('poll')),
                menuItem("Designing for X and Y", 
                         tabName = "xyplots", 
                         icon = icon('tachometer-alt')),
                menuItem("Prediction", 
                         tabName = 'pred', 
                         icon = icon('search')))
  ),
  
  #Tabs layout
  dashboardBody(
    tags$head(tags$style(HTML('.main-header .logo {font-weight: bold;}'))),
    tabItems(
      #About tab content
      tabItem(
        'about',
        box(
          title = 'Diabetes Diagnosis (HW6-Case Study3) ', 
          status = 'success', 
          solidHeader = TRUE, 
          width = 12, 
          height = 210,
          helpText('This project is about Diabetes diagnosis.'),
          helpText('Diabetes is a chronic (long-lasting) health condition that affects how your body turns food into energy.'),
          helpText(sprintf('This dataset is originally from the National Institute of Diabetes and Digestive and Kidney Diseases. The objective is to predict based on diagnostic measurements whether a patient has diabetes.')),
          tags$a(href = "https://www.kaggle.com/datasets/mathchi/diabetes-data-set", "Data Source")
        )
      ),
      
      #Tutorial tab content
      tabItem(
        'tutorial',
        box(
          title = 'How to use it?', 
          status = 'success', 
          solidHeader = TRUE, 
          width = 12, 
          height = 280,
          helpText('There are three function parts in the application.'),
          helpText('➢ The first part is about two plots for univariate. You can filter the numerical variables (Pregnancies, Glucose, BloodPressure, SkinThickness, Insulin, and DiabetesPedigre to build the density plot. And you also can filter BMI, age, and outcome to build the pie plot.'),
          helpText('➢ The second part is about two charts for x and y. You can filter six numerical variables to create a quadrantal diagram of the data percentiles and averages. Additionally, you can filter three categorical variables to create a bar graph (0=Not Have/1=Have).'),
          helpText(sprintf('➢ The third part is about predicting the outcome of a diabetes diagnosis. Because it is the dependent variable, that means we are in the presence of a regression task. After calculation, the logistic regression model is used to predict. Compared with other algorithms, it has more accurate results. You can input new sample values, and then click the “Predict” button to get the result. The outcome shows the probability of developing diabetes.'))
        )
      ),
      
      
      #Plots tab content
      tabItem(
        'plots', 
        fluidRow(
          # Density plot for numerical variables
          box(status = 'primary', 
              title = 'Density plot for numerical variable',
              solidHeader = TRUE, 
              width = 12,
              fluidRow(
                column(width = 3,
                       selectInput('num', 
                                   "Numerical variable:",
                                   numvars)),
                column(width = 9,
                       plotOutput('numplot'))))
        ),
        
        fluidRow(
          # Pie plot for categorical variables
          box(status = 'primary', 
              title = 'Pie plot for categorical variable',
              solidHeader = TRUE, 
              width = 12,
              fluidRow(
                column(width = 3,
                       selectInput('cat', 
                                   'Categorical variable:',
                                   catvars)),
                column(width = 9,
                       plotOutput('catplot'))))
        ),
      ),
      
      #Dashboard tab content
      tabItem(
        'xyplots',
        
        fluidRow(
          box(status = 'primary', 
              title = 'Numerical x variable and y',
              solidHeader = TRUE, 
              width = 12,
              fluidRow(
                column(width = 3,
                       selectInput('numx', 
                                   "Numerical variable:",
                                   numxs)),
                column(width = 9,
                       plotOutput('numxyplot'))))
        ),
        
        fluidRow(
          box(status = 'primary', 
              title = 'Categorical x variable and y',
              solidHeader = TRUE, 
              width = 12,
              fluidRow(
                column(width = 3,
                       selectInput('catx', 
                                   'Categorical variable:',
                                   catxs)),
                column(width = 9,
                       plotOutput('catxyplot'))))
        )
        
      ),
      
      #Prediction tab content
      tabItem('pred',
              #Box to display information about the model
              box(title = 'Model explanation', 
                  status = 'success', 
                  solidHeader = TRUE, 
                  width = 12, 
                  height = 165,
                  helpText('The prediction is based on a logistic regression supervised machine learning model.'),
                  helpText('It is a statistical analysis method to predict a binary outcome, such as yes or no, based on prior observations of a data set.'),
                  helpText(sprintf('The result will be forecasted by the the probability of developing diabetes.'))),
              
              #Filters for numeric variables
              box(title = 'X of New sample', 
                  status = 'primary', 
                  solidHeader = TRUE, 
                  width = 9,
                  map(xvars, newxcolumn, data = diabetes2)),
              #Box to display the prediction results
              box(title = 'Prediction result', 
                  status = 'success', 
                  solidHeader = TRUE, 
                  width = 3, 
                  height = 293,
                  div(h4('Predicted Class( 0=Not Have, 1=Have):')),
                  textOutput("pclass"),
                  div(h4('Predicted Probability of Developing Diabetes :')),
                  textOutput("pprob"),
                  hr(),
                  actionButton('cal','Predict', icon = icon('calculator'))),
              
      )
    )
  )
  
)

# R Shiny server
server <- shinyServer(function(input, output) {
  
  #Univariate analysis
  output$numplot <- renderPlot({
    diabetes2 %>%
      ggplot(aes_string(x = input$num)) +
      geom_density(size = 2, fill = "skyblue") +
      theme_bw() +
      theme(text = element_text(size = 15))
  })
  
  output$catplot <- renderPlot({
    catxcount <- diabetes2 %>%
      rename("catx" = input$cat) %>%
      count(catx)
    pie(catxcount$n, 
        labels = catxcount$catx, 
        clockwise = T, 
        radius = 1,
        cex = 1.5)
  })
  
  #Dashboard analysis
  output$numxyplot <- renderPlot({
    
    diabetes2 %>%
      ggplot(aes_string(x = yvar, y = input$numx, fill = yvar)) +
      geom_boxplot(show.legend = F) +
      theme_bw() +
      theme(text = element_text(size = 15))
    
  })
  
  output$catxyplot <- renderPlot({
    
    diabetes2 %>%
      ggplot(aes_string(x = input$catx, fill = yvar)) +
      geom_bar(position = "dodge") +
      theme_bw() +
      theme(text = element_text(size = 15))
    
  })
  
  #Prediction model
  newsample <- reactive({
    newsamplei <- 
      data.frame(matrix(NA, 
                        nrow = 1, 
                        ncol = length(xvars)))
    for (i in seq_along(xvars)) {
      valuei <- input[[xvars[i]]]
      newsamplei[1, i] <- valuei
    }
    names(newsamplei) <- xvars
    newsamplei
  })
  
  observeEvent(input$cal, {
    
    alldatax <- rbind(diabetes2[, xvars], newsample())
    
    predprob <- 
      as.numeric(tail(predict(fit_lr, alldatax, type = "response"), 1))
    
    output$pclass <- renderText({
      
      ifelse(predprob > 0.5, 1, 0)
      
    })
    
    output$pprob <- renderText({
      
      round(predprob, 4)
      
    })
    
  })
  
  
  
})

shinyApp(ui, server)  
