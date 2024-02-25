
library(shiny)
shinyServer(function(input, output) {
  
    investoptions <- reactive({
    if (input$goals_in == "Retirement" & input$year_in %in% c(5,10,15) & 
        input$risk_in == "Medium" & input$type_in == "Monthly"){
      option1 <- "This is your first option for retirement: VSMPX"
      option2 <- "This is your second option for retirement: FXAIX"
      option3 <- "this is your third option for retirement: VFIAX"
      interest_rate_op1 <- data["VSMPX",3]
      interest_rate_op2 <- data["FXAIX",3]
      interest_rate_op3 <- data["VFIAX",3]
    }
    else if (input$goals_in == "Retirement" & input$year_in %in% c(5,10,15) & 
               input$risk_in == "Low" & input$type_in == "Monthly"){
        option1 <- "This is your first option for retirement"
        option2 <- "This is your second option for retirement"
        option3 <- "this is your third option for retirement"
        interest_rate_op1 <- data["VTSAX",3]
        interest_rate_op2 <- data["VMFXX",3]
        interest_rate_op3 <- data["SPAXX",3]
    }
    else if (input$goals_in == "Retirement" & input$year_in %in% c(5,10,15) & 
               input$risk_in == "High" & input$type_in == "Monthly"){
        option1 <- "This is your first option for retirement"
        option2 <- "This is your second option for retirement"
        option3 <- "this is your third option for retirement"
        interest_rate_op1 <- data["VTSAX",3]
        interest_rate_op2 <- data["VMFXX",3]
        interest_rate_op3 <- data["SPAXX",3]
    }
    else if (input$goals_in == "Retirement" & input$year_in %in% c(5,10,15) & 
          input$risk_in == "Medium" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for retirement: VSMPX"
        option2 <- "This is your second option for retirement: FXAIX"
        option3 <- "this is your third option for retirement: VFIAX"
        interest_rate_op1 <- data["VSMPX",3]
        interest_rate_op2 <- data["FXAIX",3]
        interest_rate_op3 <- data["VFIAX",3]
      }
    else if (input$goals_in == "Retirement" & input$year_in %in% c(5,10,15) & 
            input$risk_in == "Low" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for retirement"
        option2 <- "This is your second option for retirement"
        option3 <- "this is your third option for retirement"
        interest_rate_op1 <- data["VTSAX",3]
        interest_rate_op2 <- data["VMFXX",3]
        interest_rate_op3 <- data["SPAXX",3]
      }
    else if (input$goals_in == "Retirement" & input$year_in %in% c(5,10,15) & 
               input$risk_in == "High" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for retirement"
        option2 <- "This is your second option for retirement"
        option3 <- "this is your third option for retirement"
        interest_rate_op1 <- data["VTSAX",3]
        interest_rate_op2 <- data["VMFXX",3]
        interest_rate_op3 <- data["SPAXX",3]
      }
    else if (input$goals_in == "Retirement" & input$year_in > 15 & 
          input$risk_in == "Medium" & input$type_in == "Monthly"){
        option1 <- "This is your first option for retirement: VSMPX"
        option2 <- "This is your second option for retirement: FXAIX"
        option3 <- "this is your third option for retirement: VFIAX"
        interest_rate_op1 <- data["VSMPX",3]
        interest_rate_op2 <- data["FXAIX",3]
        interest_rate_op3 <- data["VFIAX",3]
      }
    else if (input$goals_in == "Retirement" & input$year_in > 15 & 
               input$risk_in == "Low" & input$type_in == "Monthly"){
        option1 <- "This is your first option for retirement"
        option2 <- "This is your second option for retirement"
        option3 <- "this is your third option for retirement"
        interest_rate_op1 <- data["VTSAX",3]
        interest_rate_op2 <- data["VMFXX",3]
        interest_rate_op3 <- data["SPAXX",3]
      }
    else if (input$goals_in == "Retirement" & input$year_in > 15 & 
               input$risk_in == "High" & input$type_in == "Monthly"){
        option1 <- "This is your first option for retirement"
        option2 <- "This is your second option for retirement"
        option3 <- "this is your third option for retirement"
        interest_rate_op1 <- data["VTSAX",3]
        interest_rate_op2 <- data["VMFXX",3]
        interest_rate_op3 <- data["SPAXX",3]
      }
      else if (input$goals_in == "Retirement" & input$year_in > 15 & 
          input$risk_in == "Medium" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for retirement: VSMPX"
        option2 <- "This is your second option for retirement: FXAIX"
        option3 <- "this is your third option for retirement: VFIAX"
        interest_rate_op1 <- data["VSMPX",3]
        interest_rate_op2 <- data["FXAIX",3]
        interest_rate_op3 <- data["VFIAX",3]
      }
      else if (input$goals_in == "Retirement" & input$year_in > 15 & 
               input$risk_in == "Low" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for retirement"
        option2 <- "This is your second option for retirement"
        option3 <- "this is your third option for retirement"
        interest_rate_op1 <- data["VTSAX",3]
        interest_rate_op2 <- data["VMFXX",3]
        interest_rate_op3 <- data["SPAXX",3]
      }
      else if (input$goals_in == "Retirement" & input$year_in > 15 & 
               input$risk_in == "High" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for retirement"
        option2 <- "This is your second option for retirement"
        option3 <- "this is your third option for retirement"
        interest_rate_op1 <- data["VTSAX",3]
        interest_rate_op2 <- data["VMFXX",3]
        interest_rate_op3 <- data["SPAXX",3]
      }
      
      
    else if (input$goals_in == "Savings" & input$year_in %in% c(5,10,15) & 
             input$risk_in == "Medium" & input$type_in == "Monthly"){
      option1 <- "This is your first option for savings"
      option2 <- "This is your second option for savings"
      option3 <- "this is your third option for savings"
      interest_rate_op1 <- data["VTSAX",3]
      interest_rate_op2 <- data["VMFXX",3]
      interest_rate_op3 <- data["SPAXX",3]
    }
    else if (input$goals_in == "Savings" & input$year_in %in% c(5,10,15) & 
               input$risk_in == "Low" & input$type_in == "Monthly"){
        option1 <- "This is your first option for Savings"
        option2 <- "This is your second option for Savings"
        option3 <- "this is your third option for Savings"
        interest_rate_op1 <- data["VTSAX",3]
        interest_rate_op2 <- data["VMFXX",3]
        interest_rate_op3 <- data["SPAXX",3]
      }
      else if (input$goals_in == "Savings" & input$year_in %in% c(5,10,15) & 
               input$risk_in == "High" & input$type_in == "Monthly"){
        option1 <- "This is your first option for Savings"
        option2 <- "This is your second option for Savings"
        option3 <- "this is your third option for Savings"
        interest_rate_op1 <- data["VTSAX",3]
        interest_rate_op2 <- data["VMFXX",3]
        interest_rate_op3 <- data["SPAXX",3]
      }
      else if (input$goals_in == "Savings" & input$year_in %in% c(5,10,15) & 
               input$risk_in == "Medium" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for Savings: VSMPX"
        option2 <- "This is your second option for Savings: FXAIX"
        option3 <- "this is your third option for Savings: VFIAX"
        interest_rate_op1 <- data["VSMPX",3]
        interest_rate_op2 <- data["FXAIX",3]
        interest_rate_op3 <- data["VFIAX",3]
      }
      else if (input$goals_in == "Savings" & input$year_in %in% c(5,10,15) & 
               input$risk_in == "Low" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for Savings"
        option2 <- "This is your second option for Savings"
        option3 <- "this is your third option for Savings"
        interest_rate_op1 <- data["VTSAX",3]
        interest_rate_op2 <- data["VMFXX",3]
        interest_rate_op3 <- data["SPAXX",3]
      }
      else if (input$goals_in == "Savings" & input$year_in %in% c(5,10,15) & 
               input$risk_in == "High" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for Savings"
        option2 <- "This is your second option for Savings"
        option3 <- "this is your third option for Savings"
        interest_rate_op1 <- data["VTSAX",3]
        interest_rate_op2 <- data["VMFXX",3]
        interest_rate_op3 <- data["SPAXX",3]
      }
      else if (input$goals_in == "Savings" & input$year_in > 15 & 
               input$risk_in == "Medium" & input$type_in == "Monthly"){
        option1 <- "This is your first option for Savings: VSMPX"
        option2 <- "This is your second option for Savings: FXAIX"
        option3 <- "this is your third option for Savings: VFIAX"
        interest_rate_op1 <- data["VSMPX",3]
        interest_rate_op2 <- data["FXAIX",3]
        interest_rate_op3 <- data["VFIAX",3]
      }
      else if (input$goals_in == "Savings" & input$year_in > 15 & 
               input$risk_in == "Low" & input$type_in == "Monthly"){
        option1 <- "This is your first option for Savings"
        option2 <- "This is your second option for Savings"
        option3 <- "this is your third option for Savings"
        interest_rate_op1 <- data["VTSAX",3]
        interest_rate_op2 <- data["VMFXX",3]
        interest_rate_op3 <- data["SPAXX",3]
      }
      else if (input$goals_in == "Savings" & input$year_in > 15 & 
               input$risk_in == "High" & input$type_in == "Monthly"){
        option1 <- "This is your first option for Savings"
        option2 <- "This is your second option for Savings"
        option3 <- "this is your third option for Savings"
        interest_rate_op1 <- data["VTSAX",3]
        interest_rate_op2 <- data["VMFXX",3]
        interest_rate_op3 <- data["SPAXX",3]
      }
      else if (input$goals_in == "Savings" & input$year_in > 15 & 
               input$risk_in == "Medium" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for Savings: VSMPX"
        option2 <- "This is your second option for Savings: FXAIX"
        option3 <- "this is your third option for Savings: VFIAX"
        interest_rate_op1 <- data["VSMPX",3]
        interest_rate_op2 <- data["FXAIX",3]
        interest_rate_op3 <- data["VFIAX",3]
      }
      else if (input$goals_in == "Savings" & input$year_in > 15 & 
               input$risk_in == "Low" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for Savings"
        option2 <- "This is your second option for Savings"
        option3 <- "this is your third option for Savings"
        interest_rate_op1 <- data["VTSAX",3]
        interest_rate_op2 <- data["VMFXX",3]
        interest_rate_op3 <- data["SPAXX",3]
      }
      else if (input$goals_in == "Savings" & input$year_in > 15 & 
               input$risk_in == "High" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for Savings"
        option2 <- "This is your second option for Savings"
        option3 <- "this is your third option for Savings"
        interest_rate_op1 <- data["VTSAX",3]
        interest_rate_op2 <- data["VMFXX",3]
        interest_rate_op3 <- data["SPAXX",3]
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
    
    y_values <- c(amt)
    
if(year > 15) {
    for (i in seq(1, year, by = 5)) {
      if (amt_type == "Monthly") {
        y_values[i] <- inrate * 12 * y_values[i-1]
      } else if (amt_type == "Lump Sum") {
        y_values[i] <- inrate * y_values[i-1]
      } else {
        stop("Invalid amount type. Please make a different selection.")
      }
    }} else if (year <= 15){
      for (i in seq(2, year, by = 1)) {
        if (amt_type == "Monthly") {
          print(i)
          print(y_values[pmax(1, i - 1)])
          y_values[i] <- inrate * 12 * y_values[i-1]
        } else if (amt_type == "Lump Sum") {
          y_values[i] <- inrate * y_values[i-1]
        } else {
          stop("Invalid amount type. Please make a different selection.")
        }
      }
    }
    
    data.frame(
      year = seq_along(1:year),
      amt = amt,
      inrate = inrate,
      total = y_values
    )
  })
  
  output$graph_out <- renderPlotly({
    plot_ly(dataInput(), x = ~year, y = ~total, type = 'scatter', mode = 'lines')
  })
})

