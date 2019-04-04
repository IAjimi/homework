### Loading Packages ####
library(shiny)
library(tidyverse)
library(lubridate)
library(quantmod)

###Shiny ####
# User interface#
ui <- fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      h2("Setup"),
      helpText("Select index funds from the list below or add  desired stocks."),
      
      checkboxGroupInput("checkGroup", 
                         strong("Default Index Funds:"), 
                         choices = list("Dow Jones" = "^DJI", 
                                        "SP 500" = "^GSPC", 
                                        "Russell 2000" = "^RUT",
                                        "Nasdaq" = "^NDX",
                                        "Gov Bond Index (CPTNX)" = "CPTNX"),
                         selected = c("^DJI", "^GSPC","^RUT", "^NDX", "CPTNX") ),
      
      textInput("text", strong("Additional Stocks:"), 
                value = NULL) ,
      
      dateRangeInput("dates", strong("Date range:"),
                     start = "2000-01-01",
                     end   = "2019-01-01")
    ),
    
    mainPanel(
      h1("Volatilities & Correlations Calculator"),
      p("Compute the volatilities & correlations of your portfolio in just a few clicks!"), 
      p("Note: loading more than 5 stocks at once will slow down the process, due to underlying query
        restrictions"), 
      br(),
      h3("Volatilities:"),
      tableOutput("volat"),
      br(),
      h3("Correlations:"),
      tableOutput("correl")
      )
  )
)

# Server logic
server <- function(input, output) {
  
  dataInput <- reactive({ 
    stocks <- c(input$checkGroup, input$text)
    date1 <- input$dates[1]
    date2 <- input$dates[2]
    
    e <- new.env() #creates new environment
    
    s <- getSymbols(stocks, src="yahoo",  from = date1, to = date2, env = e) #quantmode gets stock data from yahoo
    df <- do.call(merge, eapply(e, Cl)[s]) #applies loop over environment
    #only keeps closing prices of the named stocks, merges df so all stocks are in one file
    
    df <- data.frame(df) #convert result of query into dataframe
    
    for (i in c(1:ncol(df))){
      assign(paste(s[i], "Return", sep = "_"), c(diff(log(df[, i]))) , envir = e ) #calc return for every stock
      assign(paste(s[i], "vol", sep = "_"),  sqrt(256)*sd(c(diff(log(df[, i])))) , envir = e ) #calc vol
    }
    
    ##volatilities
    vol_vectors <- ls(e)[str_detect(ls(e), "vol")] #subsets environment for volatilies
    assign("volatilities", mget(vol_vectors,  envir= e), envir = globalenv()) #returns vol for everything
    
    ##correlation matrix
    return_vectors <- ls(e)[str_detect(ls(e), "Return")] #subsets enviroment for return vectors
    big_corr_matrix_data <- data.frame(mget(return_vectors,  envir= e)) #greates a df made up of the return vectors
    big_corr_matrix <- cor(big_corr_matrix_data) #gets correlation of df
    big_corr_matrix <- as.data.frame(big_corr_matrix) 
    names(big_corr_matrix) <- substr(return_vectors, 1, 3) #adds col names
    row.names(big_corr_matrix) <- names(big_corr_matrix) #adds row names
    assign("corr_matrix", round(big_corr_matrix, 2), envir = globalenv()) #returns matrix!
    
    ##saves output as list to display in tables
    list(volatilities, corr_matrix)
  })
  
  
  output$volat <- renderTable({
    thing <- dataInput()
    volatilities <- thing[[1]]
    names(volatilities) <- substr(names(volatilities), 1, 3)
    volatilities
    
  })
  
  output$correl <- renderTable({
    thing <- dataInput()
    thing[[2]]
    
  })
  
  
}

shinyApp(ui, server)