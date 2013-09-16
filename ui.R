require(rCharts)
require(quantmod)
library(shinyIncubator)
source("modProgress.R", local = TRUE)

shinyUI(pageWithSidebar(
  headerPanel(
    "Introduction to Portfolio Theory",
    tags$head(
       tags$link(rel="stylesheet", type="text/css", href="style.css")
    )
  ),
  
  sidebarPanel(
    textInput("ticker1", "Ticker 1:", "GM"),
    textInput("ticker2", "Ticker 2:", "FB"),
    textInput("ticker3", "Ticker 3:", "XOM"),
    textInput("ticker4", "Ticker 4:", "JNJ"),
    
    tags$br(),
    tags$br(),
    dateRangeInput("dateRange", "Date Range:",
                   start = as.Date("2009-01-01"), end = Sys.Date() - 1, 
                   min = as.Date("1995-01-01"), max = Sys.Date() - 1, 
                   format = "M dd, yyyy"),
    
    tags$br(),
    radioButtons("period", "Period:",
                 list("Weekly" = "weekly",
                      "Monthly" = "monthly"),
                 selected = "Weekly")
  ),
  
  mainPanel(
    progressInit.2(),   # Small mod to the original bar to change the location
    tabsetPanel(
      tabPanel("Returns Table", 
               chartOutput('retTable', 'datatables'), value = 1),
      tabPanel("Returns Time Chart",
               chartOutput('retTime', 'highcharts'), value = 2),
      tabPanel("Returns Distribution",
               chartOutput('retHist', 'highcharts'), value = 3),
      tabPanel("Returns Statistics",
               div(chartOutput('pftTable1', 'datatables'), style = 'height: 180px'),
               chartOutput('pftTable2', 'datatables'), value = 4),
      tabPanel("Possible Portfolios",
               chartOutput('pftScatter', 'highcharts'), value = 5),
      id = "tab")
  )
))