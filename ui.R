library(shiny)
library(shinyWidgets)

shinyUI(fluidPage(
  theme = shinytheme(theme = "sandstone"),
  
  tabsetPanel(
    tabPanel("Homepage",
             h1("Homepage"),
             sidebarLayout(
               sidebarPanel(
                p("here is some info about is") 
               ),
               mainPanel(
                 h2("Information"),
                 p("here is some information about our website")
               )
             )
             ),
    tabPanel("Questionaire",
             h1("Questionaire"),
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "goals_in",
                             label = "What are your goals?", 
                             choices = c("Retirement", "Savings","Option 3","Option 4")),
                 sliderTextInput(inputId = "risk_in",
                             label = "What is your risk tolerance?", 
                             choices = c("Low"," " ,"Medium", " ","High"),
                             selected = "Medium"),
                 numericInput(inputId = "investment_in",
                              label = "How much would you like to invest?",
                              value = 10000,
                              min = 0
                              ),
                 submitButton(text = "Submit")
               ),
               mainPanel(
                 plotOutput(outputId = "graph_out")
               )
             )
             )
  )
  )
  )