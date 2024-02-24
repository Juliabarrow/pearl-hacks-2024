
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
      interest_rate_op3 <- 1.08
    }
    else if (input$goals_in == "Savings" & input$year_in %in% c(5,10,15) & 
             input$risk_in == "Medium" & input$type_in == "Monthly"){
      option1 <- "This is your first option for savings"
      option2 <- "This is your second option for savings"
      option3 <- "this is your third option for savings"
      interest_rate_op1 <- 1.04
      interest_rate_op2 <- 1.06
      interest_rate_op3 <- 1.08
    }
    return(c(option1,option2,option3,interest_rate_op1,interest_rate_op2,interest_rate_op3))
  })
  
  output$second_option_in <- renderUI({
    radioButtons(inputId = "invest_choice",
                label = "Which investment option would you prefer?",
                choices = investoptions()[1:3]
                )
  })
  
investmentchoice <- reactive({
  if (input$invest_choice == investoptions()[1]){
    interest_choice <- investoptions()[4]
  }
  else if (input$invest_choice == investoptions()[2]){
    interest_choice <- investoptions()[5]
  }
  else if (input$invest_choice == investoptions()[3]){
    interest_choice <- investoptions()[6]
  }
  return(interest_choice)
})

  dataInput <- reactive({
    req(input$year_in, input$type_in, input$investment_in, investmentchoice())
    
    year <- as.numeric(input$year_in)
    amt <- as.numeric(input$investment_in)
    inrate <- as.numeric(investmentchoice())
    amt_type <- input$type_in 
    
    y_values <- numeric(year)
    
    for (i in 1:year) {
      if (amt_type == "Monthly") {
        y_values[i] <- amt * inrate * 12 * i
      } else if (amt_type == "Lump Sum") {
        y_values[i] <- amt * inrate * i
      } else {
        stop("Invalid amount type. Please make a different selection.")
      }
    }
    
    data.frame(
      year = year,
      amt = amt,
      inrate = inrate,
      total = y_values
    )
  })
  
  output$graph_out <- renderPlot({
    ggplot(dataInput(), aes(x = year, y = total)) +
      geom_point() +
      geom_line()
  })
})

