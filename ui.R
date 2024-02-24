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
                 selectInput(inputId =  "year_in", 
                   label = "In how many years do you need your money?", 
                   choices = seq(5,50, by = 5)
                 ),
                 sliderTextInput(inputId = "risk_in",
                             label = "What is your risk tolerance?", 
                             choices = c("Low","Medium", "High"),
                             selected = "Medium"),
                 numericInputIcon("investment_in", 
                                  "How much would you like to invest?", 
                                  value = 10000, 
                                  step= 1000, 
                                  min= 0,
                                  icon = list(icon("dollar-sign"),NULL)),
                 radioButtons("type_in",
                              label = "What kind of investment?",
                              choices = c("Monthly", "Lump Sum"),
                              selected = "Monthly"),
                 submitButton(text = "Submit"),
                 h1(),
                 h3("Your recommended options:"),
                 uiOutput("second_option_in")
               ),
               mainPanel(
                 plotOutput(outputId = "graph_out")
               )
             )
             )
  )
  )
  )