library(shinydashboard)

skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
  skin <- "black"


sidebar <- dashboardSidebar(
  sidebarSearchForm(label = "Search...", "searchText", "searchButton"),
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem("dashboard",
            
            # Boxes with solid headers
            fluidRow(
             box(
               title = "Histogram control", width = 4, solidHeader = TRUE, status = "primary",
             sliderInput("count", "Count", min = 1, max = 500, value = 120)
            ),
              box(
                title = "Enter Stock Code", width = 4, solidHeader = TRUE, status = "primary",
                textInput("StockCode", "StockCode", value = "AAPL"),
                radioButtons("seasonal", "Select", c(LineChart = "Line Chart")),
                
                actionButton(inputId = "click", label = "Create")
              )
            ,box(
                title = "Appearance",
                width = 2, solidHeader = TRUE,
                radioButtons("fill", "Fill", # inline = TRUE,
                             c(None = "none", Blue = "blue", Black = "black", red = "red")
                )
              ),
              box(
                title = "Scatterplot control",
                width = 2, solidHeader = TRUE, status = "warning",
                selectInput("spread", "Spread",
                            choices = c("0%" = 0, "20%" = 20, "40%" = 40, "60%" = 60, "80%" = 80, "100%" = 100),
                            selected = "60"
                )
              )
            ),
      fluidRow(
        
        box(
          title = "Line Chart",
          status = "primary",
          plotOutput("auto.arima", height = 350),
          height = 400
        ),
        box(
          title = "Line Chart",
          
          width = 6,
          tableOutput("auto.arima1"),
          height = 380
        
        
      )
            )
      

       ,
       #Solid backgrounds
       fluidRow(
                 box(
           title = "Distribution",
           status = "primary",
           plotOutput("plot1", height = 240),
           height = 300
         ),
         tabBox(
           height = 300,
           tabPanel("View 1",
                    plotOutput("scatter1", height = 230)
           ),
           tabPanel("View 2",
                    plotOutput("scatter2", height = 230)
           )
      
           )
      
         ) #fluidRow Solid backgrounds
    )
  )
)

messages <- dropdownMenu(type = "messages",
  messageItem(
    from = "InvestoSure",
    message = "solutions for everyone."
  )
)


header <- dashboardHeader(
  title = "Executive Dashboard",
  messages
)

ui <- dashboardPage(header, sidebar, body, skin = skin)

server <- function(input, output) {

  
  set.seed(122)
  histdata <- rnorm(500)

  output$plot1 <- renderPlot({
    if (is.null(input$count) || is.null(input$fill))
      return()

    data <- histdata[seq(1, input$count)]
    color <- input$fill
    if (color == "none")
      color <- NULL
    hist(data, col = color, main = NULL)
  })
  
  
#Auto.Arima - plot here  Tile#4 
  output$auto.arima <- renderPlot({
    
  
     if (is.null(input$StockCode))
       return()
    library('quantmod')
    library('ggplot2')
    library('forecast')
    library('tseries')
    Stock <- as.character(input$StockCode)
    
    data <- eventReactive(input$click, {
      (input$StockCode) 
    })
    Stock <- as.character(data())
    print(Stock)
    getSymbols("AAPL", src = "yahoo",from="2017-07-01")
    plot(AAPL$AAPL.Close)  
    Stock_df<-as.data.frame(getSymbols(Symbols = Stock, 
                                   src = "yahoo", from = "2016-01-01", env = NULL))
    Stock_df$Open = Stock_df[,1]
    Stock_df$High = Stock_df[,2]
    Stock_df$Low = Stock_df[,3]
    Stock_df$Close = Stock_df[,4]
    Stock_df$Volume = Stock_df[,5]
    Stock_df$Adj = Stock_df[,6]
    Stock_df <- Stock_df[,c(7,8,9,10,11,12)] 
    
    
    
    plot(as.ts(Stock_df$Close))
    
    Stock_df$v7_MA = ma(Stock_df$Close, order=7)
    Stock_df$v30_MA <- ma(Stock_df$Close, order=30)
    
    #STL
    rental_ma <- ts(na.omit(Stock_df$v7_MA), frequency=30)
    decomp_rental <- stl(rental_ma, s.window="periodic")
    plot(decomp_rental)
    adj_rental <- seasadj(decomp_rental)
    plot(adj_rental)
    
    
    #arima
    fit <- auto.arima(Stock_df$Close,ic="bic")
    fit.forecast <- forecast(fit)
    plot(fit.forecast,  main= Stock)
    fit.forecast
   
 })

       #Auto.Arima1 - plot here  Tile#5
     output$auto.arima1 <- renderTable({
     if (is.null(input$StockCode))
       return()
     library('quantmod')
     library('ggplot2')
     library('forecast')
     library('tseries')

     Stock <- as.character(input$StockCode)
      data <- eventReactive(input$click, {
        (input$StockCode)
       })
      Stock <- as.character(data())
      print(Stock)
     getSymbols("AAPL", src = "yahoo",from="2017-07-01")
      plot(AAPL$AAPL.Close)
     Stock_df<-as.data.frame(getSymbols(Symbols = Stock,
                                        src = "yahoo", from = "2016-01-01", env = NULL))
     Stock_df$Open = Stock_df[,1]
     Stock_df$High = Stock_df[,2]
     Stock_df$Low = Stock_df[,3]
     Stock_df$Close = Stock_df[,4]
     Stock_df$Volume = Stock_df[,5]
     Stock_df$Adj = Stock_df[,6]
     Stock_df <- Stock_df[,c(7,8,9,10,11,12)]

     plot(as.ts(Stock_df$Close))

     Stock_df$v7_MA = ma(Stock_df$Close, order=7)
     Stock_df$v30_MA <- ma(Stock_df$Close, order=30)

     #STL
     rental_ma <- ts(na.omit(Stock_df$v7_MA), frequency=30)
     decomp_rental <- stl(rental_ma, s.window="periodic")
     plot(decomp_rental)
     adj_rental <- seasadj(decomp_rental)
     plot(adj_rental)


     #arima
     fit <- auto.arima(Stock_df$Close,ic="bic")
     fit.forecast <- forecast(fit)
     plot(fit.forecast,   col = "red")
     (fit.forecast)
   })
     
     #Auto.Arima Seasonal 
     output$arima.seasonal <- renderPlot({
       if (input$seasonal == "NonSeasonal")
         return()
       library('quantmod')
       library('ggplot2')
       library('forecast')
       library('tseries')
       
       Stock <- as.character(input$StockCode)
       data <- eventReactive(input$click, {
         (input$StockCode)
       })
       Stock <- as.character(data())
       print(Stock)
       getSymbols("AAPL", src = "yahoo",from="2017-07-01")
       plot(AAPL$AAPL.Close)
       Stock_df<-as.data.frame(getSymbols(Symbols = Stock,
                                          src = "yahoo", from = "2016-01-01", env = NULL))
       Stock_df$Open = Stock_df[,1]
       Stock_df$High = Stock_df[,2]
       Stock_df$Low = Stock_df[,3]
       Stock_df$Close = Stock_df[,4]
       Stock_df$Volume = Stock_df[,5]
       Stock_df$Adj = Stock_df[,6]
       Stock_df <- Stock_df[,c(7,8,9,10,11,12)]
       
      plot(as.ts(Stock_df$Close))
       
       Stock_df$v7_MA = ma(Stock_df$Close, order=7)
       Stock_df$v30_MA <- ma(Stock_df$Close, order=30)
       
       #STL
       rental_ma <- ts(na.omit(Stock_df$v7_MA), frequency=30)
       decomp_rental <- stl(rental_ma, s.window="periodic")
       plot(decomp_rental)
       adj_rental <- seasadj(decomp_rental)
       plot(adj_rental)
       
       
       #arima
       fit <- auto.arima(Stock_df$Close,ic="bic")
       fit.forecast <- forecast(fit)
       plot(fit.forecast,   col = "red")
       (fit.forecast)
       fit_s<-auto.arima(adj_rental, seasonal=TRUE)
       fcast_s <- forecast(fit_s, h=10)
       plot(fcast_s)
     })
     
     #Auto.Arima Seasonal 
     output$arima.seasonal1 <- renderTable({
       if (is.null(input$StockCode))
         return()
       if (input$seasonal == "NonSeasonal")
         return()
       library('quantmod')
       library('ggplot2')
       library('forecast')
       library('tseries')
       
       Stock <- as.character(input$StockCode)
       data <- eventReactive(input$click, {
         (input$StockCode)
       })
       Stock <- as.character(data())
       print(Stock)
       getSymbols("AAPL", src = "yahoo",from="2017-07-01")
        plot(AAPL$AAPL.Close)
       Stock_df<-as.data.frame(getSymbols(Symbols = Stock,
                                          src = "yahoo", from = "2016-01-01", env = NULL))
       Stock_df$Open = Stock_df[,1]
       Stock_df$High = Stock_df[,2]
       Stock_df$Low = Stock_df[,3]
       Stock_df$Close = Stock_df[,4]
       Stock_df$Volume = Stock_df[,5]
       Stock_df$Adj = Stock_df[,6]
       Stock_df <- Stock_df[,c(7,8,9,10,11,12)]
       
       plot(as.ts(Stock_df$Close))
       
       Stock_df$v7_MA = ma(Stock_df$Close, order=7)
       Stock_df$v30_MA <- ma(Stock_df$Close, order=30)
       
       #STL
       rental_ma <- ts(na.omit(Stock_df$v7_MA), frequency=30)
       decomp_rental <- stl(rental_ma, s.window="periodic")
       plot(decomp_rental)
       adj_rental <- seasadj(decomp_rental)
       plot(adj_rental)
       
       
       #arima
       fit <- auto.arima(Stock_df$Close,ic="bic")
       fit.forecast <- forecast(fit)
       plot(fit.forecast,   col = "red")
       (fit.forecast)
      fit_s<-auto.arima(adj_rental, seasonal=TRUE)
      fcast_s <- forecast(fit_s, h=10)
      fcast_s
     })
     
     
  output$scatter1 <- renderPlot({
    spread <- as.numeric(input$spread) / 100
    x <- rnorm(1000)
    y <- x + rnorm(1000) * spread
    plot(x, y, pch = ".", col = "blue")
  })

  output$scatter2 <- renderPlot({
    spread <- as.numeric(input$spread) / 100
    x <- rnorm(1000)
    y <- x + rnorm(1000) * spread
    plot(x, y, pch = ".", col = "red")
  })
  
  
}

shinyApp(ui, server)
