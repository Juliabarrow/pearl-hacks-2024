server <- function(input, output){
  dataInput <- reactive({
    req(input$year, input$amttype, input$amt, input$inrate)
    
    year <- as.numeric(input$year)
    amt <- as.numeric(input$amt)
    inrate <- as.numeric(input$inrate)
    amt_type <- input$amt_type 
    
    y_values <- numeric(year)
    
    for (i in 1:year) {
      if (amt_type == "monthly") {
        y_values[i] <- amt * inrate * 12 * i
      } else if (amt_type == "lump sum") {
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
  
  output$plot <- renderPlot({
    ggplot(dataInput(), aes(x = year, y = total)) +
      geom_point() +
      geom_line()
  })
}