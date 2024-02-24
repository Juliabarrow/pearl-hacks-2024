library(shiny)

shinyServer(function(input, output) {
  
  investoptions <- reactive({
    if (input$goals_in == "Retirement" & input$year_in %in% c(5,10,15) & 
        input$risk_in == "Medium" & input$type_in == "Monthly"){
      option1 <- "This is your first option for retirement"
      option2 <- "This is your second option for retirement"
      option3 <- "this is your third option for retirement"
      interest_rate_op1 <- 1.04
      interest_rate_op2 <- 1.06
      interest_rate_op2 <- 1.08
    }
    else if (input$goals_in == "Savings" & input$year_in %in% c(5,10,15) & 
             input$risk_in == "Medium" & input$type_in == "Monthly"){
      option1 <- "This is your first option for savings"
      option2 <- "This is your second option for savings"
      option3 <- "this is your third option for savings"
      interest_rate_op1 <- 1.04
      interest_rate_op2 <- 1.06
      interest_rate_op2 <- 1.08
    }
    return(c(option1,option2,option3))
  })
  
  output$second_option_in <- renderUI({
    radioButtons(inputId = "invest_choice",
                label = "Which investment option would you prefer?",
                choices = investoptions()[1:3]
                )
  })
})