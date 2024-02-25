
library(shiny)
shinyServer(function(input, output) {
  
    investoptions <- reactive({
    if (input$goals_in == "Retirement" & input$year_in %in% c(5,10,15) & 
        input$risk_in == "Medium" & input$type_in == "Monthly"){
      option1 <- "This is your first option for retirement: MVRXX"
      option2 <- "This is your second option for retirement: AGTHX"
      option3 <- "this is your third option for retirement: TFDXX"
      interest_rate_op1 <- data["MVRXX",3]
      interest_rate_op2 <- data["AGTHX",3]
      interest_rate_op3 <- data["TFDXX",3]
    }
    else if (input$goals_in == "Retirement" & input$year_in %in% c(5,10,15) & 
               input$risk_in == "Low" & input$type_in == "Monthly"){
        option1 <- "This is your first option for retirement: VTSAX"
        option2 <- "This is your second option for retirement: VFIAX"
        option3 <- "this is your third option for retirement: VSMPX"
        interest_rate_op1 <- data["VTSAX",3]
        interest_rate_op2 <- data["VFIAX",3]
        interest_rate_op3 <- data["VSMPX",3]
    }
    else if (input$goals_in == "Retirement" & input$year_in %in% c(5,10,15) & 
               input$risk_in == "High" & input$type_in == "Monthly"){
        option1 <- "This is your first option for retirement: OGVXX"
        option2 <- "This is your second option for retirement: VTBNX"
        option3 <- "this is your third option for retirement: TTTXX"
        interest_rate_op1 <- data["OGVXX",3]
        interest_rate_op2 <- data["VTBNX",3]
        interest_rate_op3 <- data["TTTXX",3]
    }
    else if (input$goals_in == "Retirement" & input$year_in %in% c(5,10,15) & 
          input$risk_in == "Medium" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for retirement: MVRXX"
        option2 <- "This is your second option for retirement: FXAIX"
        option3 <- "this is your third option for retirement: AGTHX"
        interest_rate_op1 <- data["MVRXX",3]
        interest_rate_op2 <- data["TFDXX",3]
        interest_rate_op3 <- data["AGTHX",3]
      }
    else if (input$goals_in == "Retirement" & input$year_in %in% c(5,10,15) & 
            input$risk_in == "Low" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for retirement: VTSAX"
        option2 <- "This is your second option for retirement: VFIAX"
        option3 <- "this is your third option for retirement: VSMPX"
        interest_rate_op1 <- data["VTSAX",3]
        interest_rate_op2 <- data["VFIAX",3]
        interest_rate_op3 <- data["VSMPX",3]
      }
      else if (input$goals_in == "Retirement" & input$year_in %in% c(5,10,15) & 
               input$risk_in == "High" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for retirement: OGVXX"
        option2 <- "This is your second option for retirement: VTBNX"
        option3 <- "this is your third option for retirement: TTTXX"
        interest_rate_op1 <- data["OGVXX",3]
        interest_rate_op2 <- data["VTBNX",3]
        interest_rate_op3 <- data["TTTXX",3]
      }
    else if (input$goals_in == "Retirement" & input$year_in %in% c(20,25,30,35,40,45,50) & 
          input$risk_in == "Medium" & input$type_in == "Monthly"){
        option1 <- "This is your first option for retirement: VFFSX"
        option2 <- "This is your second option for retirement: TFDXX"
        option3 <- "this is your third option for retirement: VINIX"
        interest_rate_op1 <- data["VFFSX",4]
        interest_rate_op2 <- data["TFDXX",4]
        interest_rate_op3 <- data["VINIX",4]
      }
    else if (input$goals_in == "Retirement" & input$year_in %in% c(20,25,30,35,40,45,50) & 
               input$risk_in == "Low" & input$type_in == "Monthly"){
        option1 <- "This is your first option for retirement: VMFXX"
        option2 <- "This is your second option for retirement: VTSAX"
        option3 <- "this is your third option for retirement: FXAIX"
        interest_rate_op1 <- data["VMFXX",4]
        interest_rate_op2 <- data["VTSAX",4]
        interest_rate_op3 <- data["FXAIX",4]
      }
    else if (input$goals_in == "Retirement" & input$year_in %in% c(20,25,30,35,40,45,50) & 
               input$risk_in == "High" & input$type_in == "Monthly"){
        option1 <- "This is your first option for retirement: OGVXX"
        option2 <- "This is your second option for retirement: GVMXX"
        option3 <- "this is your third option for retirement: TTTXX"
        interest_rate_op1 <- data["OGVXX",4]
        interest_rate_op2 <- data["GVMXX",4]
        interest_rate_op3 <- data["TTTXX",4]
      }
      else if (input$goals_in == "Retirement" & input$year_in %in% c(20,25,30,35,40,45,50) & 
          input$risk_in == "Medium" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for retirement: VFFSX"
        option2 <- "This is your second option for retirement: TFDXX"
        option3 <- "this is your third option for retirement: VINIX"
        interest_rate_op1 <- data["VFFSX",4]
        interest_rate_op2 <- data["TFDXX",4]
        interest_rate_op3 <- data["VINIX",4]
      }
      else if (input$goals_in == "Retirement" & input$year_in %in% c(20,25,30,35,40,45,50) & 
               input$risk_in == "Low" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for retirement: VMFXX"
        option2 <- "This is your second option for retirement: VTSAX"
        option3 <- "this is your third option for retirement: FXAIX"
        interest_rate_op1 <- data["VMFXX",4]
        interest_rate_op2 <- data["VTSAX",4]
        interest_rate_op3 <- data["FXAIX",4]
      }
      else if (input$goals_in == "Retirement" & input$year_in %in% c(20,25,30,35,40,45,50) & 
               input$risk_in == "High" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for retirement: OGVXX"
        option2 <- "This is your second option for retirement: GVMXX"
        option3 <- "this is your third option for retirement: TTTXX"
        interest_rate_op1 <- data["OGVXX",4]
        interest_rate_op2 <- data["GVMXX",4]
        interest_rate_op3 <- data["TTTXX",4]
      }
      
      
    else if (input$goals_in == "Savings" & input$year_in %in% c(5,10,15) & 
             input$risk_in == "Medium" & input$type_in == "Monthly"){
      option1 <- "This is your first option for savings: MVRXX"
      option2 <- "This is your second option for savings: VMRXX"
      option3 <- "this is your third option for savings: AGTHX"
      interest_rate_op1 <- data["MVRXX",3]
      interest_rate_op2 <- data["VMRXX",3]
      interest_rate_op3 <- data["AGTHX",3]
    }
    else if (input$goals_in == "Savings" & input$year_in %in% c(5,10,15) & 
               input$risk_in == "Low" & input$type_in == "Monthly"){
        option1 <- "This is your first option for Savings: VSMPX"
        option2 <- "This is your second option for Savings: SPAXX"
        option3 <- "this is your third option for Savings: VFIAX"
        interest_rate_op1 <- data["VSMPX",3]
        interest_rate_op2 <- data["SPAXX",3]
        interest_rate_op3 <- data["VFIAX",3]
      }
      else if (input$goals_in == "Savings" & input$year_in %in% c(5,10,15) & 
               input$risk_in == "High" & input$type_in == "Monthly"){
        option1 <- "This is your first option for Savings: FRGXX"
        option2 <- "This is your second option for Savings: VTBNX"
        option3 <- "this is your third option for Savings: VTBIX"
        interest_rate_op1 <- data["FRGXX",3]
        interest_rate_op2 <- data["VTBNX",3]
        interest_rate_op3 <- data["VTBIX",3]
      }
      else if (input$goals_in == "Savings" & input$year_in %in% c(5,10,15) & 
               input$risk_in == "Medium" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for Savings: MVRXX"
        option2 <- "This is your second option for Savings: VMRXX"
        option3 <- "this is your third option for Savings: AGTHX"
        interest_rate_op1 <- data["MVRXX",3]
        interest_rate_op2 <- data["VMRXX",3]
        interest_rate_op3 <- data["AGTHX",3]
      }
      else if (input$goals_in == "Savings" & input$year_in %in% c(5,10,15) & 
               input$risk_in == "Low" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for Savings: VSMPX"
        option2 <- "This is your second option for Savings: SPAXX"
        option3 <- "this is your third option for Savings: VFIAX"
        interest_rate_op1 <- data["VSMPX",3]
        interest_rate_op2 <- data["SPAXX",3]
        interest_rate_op3 <- data["VFIAX",3]
      }
      else if (input$goals_in == "Savings" & input$year_in %in% c(5,10,15) & 
               input$risk_in == "High" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for Savings: FRGXX"
        option2 <- "This is your second option for Savings: VTBNX"
        option3 <- "this is your third option for Savings: VTBIX"
        interest_rate_op1 <- data["FRGXX",3]
        interest_rate_op2 <- data["VTBNX",3]
        interest_rate_op3 <- data["VTBIX",3]
      }
      else if (input$goals_in == "Savings" & input$year_in %in% c(20,25,30,35,40,45,50) & 
               input$risk_in == "Medium" & input$type_in == "Monthly"){
        option1 <- "This is your first option for Savings: MVRXX"
        option2 <- "This is your second option for Savings: VFFSX"
        option3 <- "this is your third option for Savings: AGTHX"
        interest_rate_op1 <- data["MVRXX",4]
        interest_rate_op2 <- data["VFFSX",4]
        interest_rate_op3 <- data["AGTHX",4]
      }
      else if (input$goals_in == "Savings" & input$year_in %in% c(20,25,30,35,40,45,50) & 
               input$risk_in == "Low" & input$type_in == "Monthly"){
        option1 <- "This is your first option for Savings: VMFXX"
        option2 <- "This is your second option for Savings: VTSAX"
        option3 <- "this is your third option for Savings: FXAIX"
        interest_rate_op1 <- data["VMFXX",4]
        interest_rate_op2 <- data["VTSAX",4]
        interest_rate_op3 <- data["FXAIX",4]
      }
      else if (input$goals_in == "Savings" & input$year_in %in% c(20,25,30,35,40,45,50) & 
               input$risk_in == "High" & input$type_in == "Monthly"){
        option1 <- "This is your first option for Savings: VTSAX"
        option2 <- "This is your second option for Savings: VMFXX"
        option3 <- "this is your third option for Savings: SPAXX"
        interest_rate_op1 <- data["VTSAX",4]
        interest_rate_op2 <- data["VMFXX",4]
        interest_rate_op3 <- data["SPAXX",4]
      }
      else if (input$goals_in == "Savings" & input$year_in %in% c(20,25,30,35,40,45,50) & 
               input$risk_in == "Medium" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for Savings: MVRXX"
        option2 <- "This is your second option for Savings: VFFSX"
        option3 <- "this is your third option for Savings: AGTHX"
        interest_rate_op1 <- data["MVRXX",4]
        interest_rate_op2 <- data["VFFSX",4]
        interest_rate_op3 <- data["AGTHX",4]
      }
      else if (input$goals_in == "Savings" & input$year_in %in% c(20,25,30,35,40,45,50) & 
               input$risk_in == "Low" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for Savings: VMFXX"
        option2 <- "This is your second option for Savings: VTSAX"
        option3 <- "this is your third option for Savings: FXAIX"
        interest_rate_op1 <- data["VMFXX",4]
        interest_rate_op2 <- data["VTSAX",4]
        interest_rate_op3 <- data["FXAIX",4]
      }
      else if (input$goals_in == "Savings" & input$year_in %in% c(20,25,30,35,40,45,50) & 
               input$risk_in == "High" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for Savings: VTSAX"
        option2 <- "This is your second option for Savings: VMFXX"
        option3 <- "this is your third option for Savings: SPAXX"
        interest_rate_op1 <- data["VTSAX",4]
        interest_rate_op2 <- data["VMFXX",4]
        interest_rate_op3 <- data["SPAXX",4]
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
    
    y_values <- numeric()
    
  if(year > 15) {
    for (i in seq(2, (year/5), by = 1)) {
      if (amt_type == "Monthly") {
        y_values[1] <- (1 + inrate)^(1) + amt
        y_values[i] <- (1 + inrate)^(i) * (y_values[i-1]) + amt
      } else if (amt_type == "Lump Sum") {
        y_values[1] <- (1+ inrate)^(1) + amt
        y_values[i] <- (1+inrate)^(i)  * (y_values[i-1])
      } else {
        stop("Invalid amount type. Please make a different selection.")
      }
    }
    } else if (year <= 15){
      for (i in seq(2, year, by = 1)) {
        if (amt_type == "Monthly") {
          y_values[1] <- (1 + inrate)^(1) + amt
          y_values[i] <- (1 + inrate)^(i) * (y_values[i-1]) + amt
        } else if (amt_type == "Lump Sum") {
          y_values[1] <- (1+ inrate)^(1) + amt
          y_values[i] <- (1+inrate)^(i)  * (y_values[i-1])
        } else {
          stop("Invalid amount type. Please make a different selection.")
        }
      }
    }
    if(year > 15) {
      data.frame(
        year = seq(1, year, by = 5),
        amt = amt,
        inrate = inrate,
        total = y_values
      )
    } else{
    data.frame(
      year = seq_along(1:year),
      amt = amt,
      inrate = inrate,
      total = y_values
    )}
  })
  
  output$graph_out <- renderPlotly({
    plot_ly(dataInput(), x = ~year, y = ~total, type = 'scatter', mode = 'lines')
  })
})

