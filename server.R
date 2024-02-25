
library(shiny)
shinyServer(function(input, output) {
  
    investoptions <- reactive({
    if (input$goals_in == "Retirement" & input$year_in %in% c(5,10,15) & 
        input$risk_in == "Medium" & input$type_in == "Monthly"){
      option1 <- "This is your first option for retirement: Morgan Stanley Inst Liq Government Port; Institutional"
      option2 <- "This is your second option for retirement: American Funds Growth Fund of America"
      option3 <- "This is your third option for retirement: BlackRock Liquidity FedFund; Institutional"
      interest_rate_op1 <- data["MVRXX",3]
      interest_rate_op2 <- data["AGTHX",3]
      interest_rate_op3 <- data["TFDXX",3]
    }
    else if (input$goals_in == "Retirement" & input$year_in %in% c(5,10,15) & 
               input$risk_in == "Low" & input$type_in == "Monthly"){
        option1 <- "This is your first option for retirement: Vanguard Total STock Market Index Fund; Admiral"
        option2 <- "This is your second option for retirement: Vanguard 500 Index Fund; Admiral"
        option3 <- "This is your third option for retirement: Vanguard Total STock Market Index Fund; Institutional Plus"
        interest_rate_op1 <- data["VTSAX",3]
        interest_rate_op2 <- data["VFIAX",3]
        interest_rate_op3 <- data["VSMPX",3]
    }
    else if (input$goals_in == "Retirement" & input$year_in %in% c(5,10,15) & 
               input$risk_in == "High" & input$type_in == "Monthly"){
        option1 <- "This is your first option for retirement: JPMorgan US Government Money Market Fund; Capital"
        option2 <- "This is your second option for retirement: Vanguard Total Bond Market II Index Fund; Instituational"
        option3 <- "This is your third option for retirement: BlackRock Liquidity Treaury Trust Fund; Institutional"
        interest_rate_op1 <- data["OGVXX",3]
        interest_rate_op2 <- data["VTBNX",3]
        interest_rate_op3 <- data["TTTXX",3]
    }
    else if (input$goals_in == "Retirement" & input$year_in %in% c(5,10,15) & 
          input$risk_in == "Medium" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for retirement: Morgan Stanley Inst Liq Government Port; Institutional"
        option2 <- "This is your second option for retirement: Fidelity 500 Index Fund"
        option3 <- "This is your third option for retirement: American Funds Growth Fund of America"
        interest_rate_op1 <- data["MVRXX",3]
        interest_rate_op2 <- data["TFDXX",3]
        interest_rate_op3 <- data["AGTHX",3]
      }
    else if (input$goals_in == "Retirement" & input$year_in %in% c(5,10,15) & 
            input$risk_in == "Low" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for retirement: Vanguard Total STock Market Index Fund; Admiral"
        option2 <- "This is your second option for retirement: Vanguard 500 Index Fund; Admiral"
        option3 <- "This is your third option for retirement: Vanguard Total STock Market Index Fund; Institutional Plus"
        interest_rate_op1 <- data["VTSAX",3]
        interest_rate_op2 <- data["VFIAX",3]
        interest_rate_op3 <- data["VSMPX",3]
      }
      else if (input$goals_in == "Retirement" & input$year_in %in% c(5,10,15) & 
               input$risk_in == "High" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for retirement: JPMorgan US Government Money Market Fund;Capital"
        option2 <- "This is your second option for retirement: Vanguard Total Bond Market II Index Fund; Instituational"
        option3 <- "This is your third option for retirement: BlackRock Liquidity Treaury Trust Fund; Institutional"
        interest_rate_op1 <- data["OGVXX",3]
        interest_rate_op2 <- data["VTBNX",3]
        interest_rate_op3 <- data["TTTXX",3]
      }
    else if (input$goals_in == "Retirement" & input$year_in %in% c(20,25,30,35,40,45,50) & 
          input$risk_in == "Medium" & input$type_in == "Monthly"){
        option1 <- "This is your first option for retirement: Vanguard 500 Index Fund; Institutional Select"
        option2 <- "This is your second option for retirement: BlackRock Liquidity FedFund; Institutional"
        option3 <- "This is your third option for retirement: Vanguard Institutional Index Fund; Institutional"
        interest_rate_op1 <- data["VFFSX",4]
        interest_rate_op2 <- data["TFDXX",4]
        interest_rate_op3 <- data["VINIX",4]
      }
    else if (input$goals_in == "Retirement" & input$year_in %in% c(20,25,30,35,40,45,50) & 
               input$risk_in == "Low" & input$type_in == "Monthly"){
        option1 <- "This is your first option for retirement: Vanguard Federal Money Market Fund; Investor"
        option2 <- "This is your second option for retirement: Vanguard Total STock Market Index Fund; Admiral"
        option3 <- "This is your third option for retirement: Fidelity 500 Index Fund"
        interest_rate_op1 <- data["VMFXX",4]
        interest_rate_op2 <- data["VTSAX",4]
        interest_rate_op3 <- data["FXAIX",4]
      }
    else if (input$goals_in == "Retirement" & input$year_in %in% c(20,25,30,35,40,45,50) & 
               input$risk_in == "High" & input$type_in == "Monthly"){
        option1 <- "This is your first option for retirement: JPMorgan US Government Money Market Fund;Capital"
        option2 <- "This is your second option for retirement: State Street US Government Money Market Fund; Prem"
        option3 <- "This is your third option for retirement: BlackRock Liquidity Treaury Trust Fund; Institutional"
        interest_rate_op1 <- data["OGVXX",4]
        interest_rate_op2 <- data["GVMXX",4]
        interest_rate_op3 <- data["TTTXX",4]
      }
      else if (input$goals_in == "Retirement" & input$year_in %in% c(20,25,30,35,40,45,50) & 
          input$risk_in == "Medium" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for retirement: Vanguard 500 Index Fund; Institutional Select"
        option2 <- "This is your second option for retirement: BlackRock Liquidity FedFund; Institutional"
        option3 <- "This is your third option for retirement: Vanguard Institutional Index Fund; Institutional"
        interest_rate_op1 <- data["VFFSX",4]
        interest_rate_op2 <- data["TFDXX",4]
        interest_rate_op3 <- data["VINIX",4]
      }
      else if (input$goals_in == "Retirement" & input$year_in %in% c(20,25,30,35,40,45,50) & 
               input$risk_in == "Low" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for retirement: Vanguard Federal Money Market Fund; Investor"
        option2 <- "This is your second option for retirement: Vanguard Total STock Market Index Fund; Admiral"
        option3 <- "This is your third option for retirement: Fidelity 500 Index Fund"
        interest_rate_op1 <- data["VMFXX",4]
        interest_rate_op2 <- data["VTSAX",4]
        interest_rate_op3 <- data["FXAIX",4]
      }
      else if (input$goals_in == "Retirement" & input$year_in %in% c(20,25,30,35,40,45,50) & 
               input$risk_in == "High" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for retirement: JPMorgan US Government Money Market Fund;Capital"
        option2 <- "This is your second option for retirement: State Street US Government Money Market Fund; Prem"
        option3 <- "This is your third option for retirement: BlackRock Liquidity Treaury Trust Fund; Institutional"
        interest_rate_op1 <- data["OGVXX",4]
        interest_rate_op2 <- data["GVMXX",4]
        interest_rate_op3 <- data["TTTXX",4]
      }
      
      
    else if (input$goals_in == "Savings" & input$year_in %in% c(5,10,15) & 
             input$risk_in == "Medium" & input$type_in == "Monthly"){
      option1 <- "This is your first option for savings: Morgan Stanley Inst Liq Government Port; Institutional"
      option2 <- "This is your second option for savings: Vanguard Cash Reserved Federal Money Market Fd; Adm"
      option3 <- "This is your third option for savings: American Funds Growth Fund of America"
      interest_rate_op1 <- data["MVRXX",3]
      interest_rate_op2 <- data["VMRXX",3]
      interest_rate_op3 <- data["AGTHX",3]
    }
    else if (input$goals_in == "Savings" & input$year_in %in% c(5,10,15) & 
               input$risk_in == "Low" & input$type_in == "Monthly"){
        option1 <- "This is your first option for savings: Vanguard Total STock Market Index Fund; Institutional Plus"
        option2 <- "This is your second option for savings: Fidelity Government Money Market Fund"
        option3 <- "This is your third option for savings: Vanguard 500 Index Fund; Admiral"
        interest_rate_op1 <- data["VSMPX",3]
        interest_rate_op2 <- data["SPAXX",3]
        interest_rate_op3 <- data["VFIAX",3]
      }
      else if (input$goals_in == "Savings" & input$year_in %in% c(5,10,15) & 
               input$risk_in == "High" & input$type_in == "Monthly"){
        option1 <- "This is your first option for savings: Fidelity Instl Government Portfolio; Instiutional"
        option2 <- "This is your second option for savings: Vanguard Total Bond Market II Index Fund; Instituational"
        option3 <- "This is your third option for savings: Vanguard Total Bond Market II Index Fund; Investor"
        interest_rate_op1 <- data["FRGXX",3]
        interest_rate_op2 <- data["VTBNX",3]
        interest_rate_op3 <- data["VTBIX",3]
      }
      else if (input$goals_in == "Savings" & input$year_in %in% c(5,10,15) & 
               input$risk_in == "Medium" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for savings: Morgan Stanley Inst Liq Government Port; Institutional"
        option2 <- "This is your second option for savings: Vanguard Cash Reserved Federal Money Market Fd; Adm"
        option3 <- "This is your third option for savings: American Funds Growth Fund of America"
        interest_rate_op1 <- data["MVRXX",3]
        interest_rate_op2 <- data["VMRXX",3]
        interest_rate_op3 <- data["AGTHX",3]
      }
      else if (input$goals_in == "Savings" & input$year_in %in% c(5,10,15) & 
               input$risk_in == "Low" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for savings: Vanguard Total STock Market Index Fund; Institutional Plus"
        option2 <- "This is your second option for savings: Fidelity Government Money Market Fund"
        option3 <- "This is your third option for savings: Vanguard 500 Index Fund; Admiral"
        interest_rate_op1 <- data["VSMPX",3]
        interest_rate_op2 <- data["SPAXX",3]
        interest_rate_op3 <- data["VFIAX",3]
      }
      else if (input$goals_in == "Savings" & input$year_in %in% c(5,10,15) & 
               input$risk_in == "High" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for savings: Fidelity Instl Government Portfolio; Instiutional"
        option2 <- "This is your second option for savings: Vanguard Total Bond Market II Index Fund; Instituational"
        option3 <- "This is your third option for savings: Vanguard Total Bond Market II Index Fund; Investor"
        interest_rate_op1 <- data["FRGXX",3]
        interest_rate_op2 <- data["VTBNX",3]
        interest_rate_op3 <- data["VTBIX",3]
      }
      else if (input$goals_in == "Savings" & input$year_in %in% c(20,25,30,35,40,45,50) & 
               input$risk_in == "Medium" & input$type_in == "Monthly"){
        option1 <- "This is your first option for savings: Morgan Stanley Inst Liq Government Port; Institutional"
        option2 <- "This is your second option for savings: Vanguard 500 Index Fund; Institutional Select"
        option3 <- "This is your third option for savings: American Funds Growth Fund of America"
        interest_rate_op1 <- data["MVRXX",4]
        interest_rate_op2 <- data["VFFSX",4]
        interest_rate_op3 <- data["AGTHX",4]
      }
      else if (input$goals_in == "Savings" & input$year_in %in% c(20,25,30,35,40,45,50) & 
               input$risk_in == "Low" & input$type_in == "Monthly"){
        option1 <- "This is your first option for savings: Vanguard Federal Money Market Fund; Investor"
        option2 <- "This is your second option for savings: Vanguard Total STock Market Index Fund; Admiral"
        option3 <- "This is your third option for savings: Fidelity 500 Index Fund"
        interest_rate_op1 <- data["VMFXX",4]
        interest_rate_op2 <- data["VTSAX",4]
        interest_rate_op3 <- data["FXAIX",4]
      }
      else if (input$goals_in == "Savings" & input$year_in %in% c(20,25,30,35,40,45,50) & 
               input$risk_in == "High" & input$type_in == "Monthly"){
        option1 <- "This is your first option for savings: Vanguard Total STock Market Index Fund; Admiral"
        option2 <- "This is your second option for savings: Vanguard Federal Money Market Fund; Investor"
        option3 <- "This is your third option for savings: Fidelity Government Money Market Fund"
        interest_rate_op1 <- data["VTSAX",4]
        interest_rate_op2 <- data["VMFXX",4]
        interest_rate_op3 <- data["SPAXX",4]
      }
      else if (input$goals_in == "Savings" & input$year_in %in% c(20,25,30,35,40,45,50) & 
               input$risk_in == "Medium" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for savings: Morgan Stanley Inst Liq Government Port; Institutional"
        option2 <- "This is your second option for savings: Vanguard 500 Index Fund; Institutional Select"
        option3 <- "This is your third option for savings: American Funds Growth Fund of America"
        interest_rate_op1 <- data["MVRXX",4]
        interest_rate_op2 <- data["VFFSX",4]
        interest_rate_op3 <- data["AGTHX",4]
      }
      else if (input$goals_in == "Savings" & input$year_in %in% c(20,25,30,35,40,45,50) & 
               input$risk_in == "Low" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for savings: Vanguard Federal Money Market Fund; Investor"
        option2 <- "This is your second option for savings: Vanguard Total STock Market Index Fund; Admiral"
        option3 <- "This is your third option for savings: Fidelity 500 Index Fund"
        interest_rate_op1 <- data["VMFXX",4]
        interest_rate_op2 <- data["VTSAX",4]
        interest_rate_op3 <- data["FXAIX",4]
      }
      else if (input$goals_in == "Savings" & input$year_in %in% c(20,25,30,35,40,45,50) & 
               input$risk_in == "High" & input$type_in == "Lump Sum"){
        option1 <- "This is your first option for savings: Vanguard Total STock Market Index Fund; Admiral"
        option2 <- "This is your second option for savings: Vanguard Federal Money Market Fund; Investor"
        option3 <- "This is your third option for savings: Fidelity Government Money Market Fund"
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
        y_values[1] <- ((1 + inrate)^(1)*12) + (amt*12)
        y_values[i] <- (1 + inrate)^(i*5) * (y_values[i-1]) + (amt*12)
      } else if (amt_type == "Lump Sum") {
        y_values[1] <- (1+ inrate)^(1) + amt
        y_values[i] <- (1+inrate)^(i*5)  * (y_values[i-1])
      } else {
        stop("Invalid amount type. Please make a different selection.")
      }
    }
    } else if (year <= 15){
      for (i in seq(2, year, by = 1)) {
        if (amt_type == "Monthly") {
          y_values[1] <- (1 + inrate)^(1) + (amt*12)
          y_values[i] <- (1 + inrate)^(i) * (y_values[i-1]) + (amt*12)
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
    plot_ly(dataInput(), x = ~year, y = ~total, type = 'scatter', mode = 'lines') %>%
      layout(title = 'Total Expected Value', xaxis = list(title = 'Year'), 
             
             yaxis = list(title = 'Dollars'))
  })
})

