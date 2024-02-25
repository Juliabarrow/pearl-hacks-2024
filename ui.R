library(shiny)
library(shinyWidgets)

shinyUI(fluidPage(
  theme = shinytheme(theme = "sandstone"),
  
  tabsetPanel(
    tabPanel("Homepage",
             h1("Homepage"),
             sidebarLayout(
               sidebarPanel(
                 h2("About Us"),
                p("We are a team of undergraduate students who aim to help people achieve their financial goals with more ease than traditional tools.") 
               ),
               mainPanel(
                 h2("Information"),
                 h4("Welcome to our interactive webpage dedicated to helping you choose the best investment plan tailored to your financial goals and risk tolerance. Investing is a crucial aspect of financial planning, yet navigating through the myriad of options can be daunting. Whether you're saving for retirement, building wealth, or planning for a specific financial milestone, making informed investment decisions is key to achieving your objectives."),
                 h4("In this user-friendly interface, we'll guide you through the process of understanding different investment options, assessing your risk profile, and selecting the most suitable investment plan. With interactive tools, informative resources, and expert guidance, we aim to empower you to make confident and well-informed investment choices."),
                 h4("Let's embark on this journey together to unlock the potential of your finances and pave the way towards a secure financial future. Whether you're a novice investor or seasoned pro, this platform is designed to provide valuable insights and assistance every step of the way. Let's get started!")
               )
             )
             ),
    tabPanel("Questionaire",
             h1("Questionaire"),
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "goals_in",
                             label = "What are your goals?", 
                             choices = c("Retirement", "Savings")),
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
                 uiOutput("second_option_in"),
                 submitButton(text = "Submit")
               ),
               mainPanel(
                 plotlyOutput(outputId = "graph_out")
               )
             )
             )
  )
  )
  )