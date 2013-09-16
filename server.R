require(rCharts)
require(quantmod)
library(shinyIncubator)

shinyServer(function(input, output, session) {
  
  # Keep track of views; increment viewcount in file
  views <- as.numeric(read.table("viewFile.txt", header = FALSE)[1, 1]) + 1
  write(views, file = "viewFile.txt")
  
  # Initiate global progress bar variable
  progress <- Progress$new(session)
  progress$close()
  
  # Calculate returns of input stocks
  returns <- reactive({
    
    tickers <- c(input$ticker1, input$ticker2, input$ticker3, input$ticker4)
    returnType <- 'arithmetic'        # Alternative is 'log'
    periodType <- input$period        # 'daily', 'weekly', 'monthly', etc.
    start <- input$dateRange[1]
    end <- input$dateRange[2]
    
    df <- data.frame()
    
    for (k in 1:length(tickers)) {
      closes <- getSymbols(tickers[k], from = start, to = end, auto.assign = FALSE)
      rets <- periodReturn(closes, period = periodType, type = returnType)
      
      # Handling when start date is earlier than when stock IPOed
      if (index(closes)[1] > start) {     # If first trade day is later than input start day
        start <- index(closes)[1]
        if (nrow(df) > 0) {               # If we've already added stocks to the data frame
          df <- df[which(as.Date(df$Date, format = "%b %d, %Y") >= start), ]  # Delete the data before the new start day
        }
      }
      
      if (nrow(df) == 0) {     # Create the data frame if no stocks have been added yet
        df <- data.frame(format(index(rets), format = "%b %d, %Y"), stringsAsFactors = FALSE)
        colnames(df) <- "Date"
      }
      
      df$ret1 <- as.numeric(rets[, 1])
      colnames(df)[k+1] <- tickers[k]
    }
    
    # Remove ^ from title whenever stock has ^ (ex. ^GSPC)
    colnames(df) <- lapply(colnames(df), function(x) gsub('^', '', x, fixed = TRUE))
    
    return(df)
  })
  
  # Generate histogram bins and calculate frequencies
  histogram <- function(returns) {
    series <- as.numeric()
    
    # Combine all stocks' returns into one array
    for (k in 1:nrow(returns)) {
      series <- c(series, as.numeric(returns[k, -1]))
    }
    
    binCount <- 14          # Preset bin count to fit on the screen
    binWidth <- (max(series) - min(series)) / binCount
    avg <- binWidth * 7     # Average of max and min; use as center bin
    
    bins <- avg             # bins will hold array of bin starts
    lastBin <- avg          # lastBin serves as a temp
    
    # Add bins to the bin array until we've covered the max return
    while (lastBin < max(series)) {
      lastBin <- lastBin + binWidth
      bins <- c(bins, lastBin)
    }
    
    # Add bins to the bin array until we've covered the min return
    lastBin <- avg
    while (lastBin > min(series)) {
      lastBin <- lastBin - binWidth
      bins <- c(bins, lastBin)
    }
    
    bins <- as.data.frame(sort(bins))
    
    # Count the number of returns for each stock that fit in between the bins
    for (k in 2:ncol(returns)) {
      bins$stock <- 0
      colnames(bins)[k] <- colnames(returns)[k]
      
      for (j in 2:nrow(bins)) {
        bins[j, k] <- nrow(returns[which((returns[, k] > bins[j-1, 1]) & (returns[, k] < bins[j, 1])), ])
      }
    }
    
    bins <- bins[-1, ]
    colnames(bins)[1] <- 'Returns'
    
    bins$Returns <- round(bins$Returns, digits = 2)
    
    return(bins)
  }
  
  # Calculate annualized avg and sd for each stock
  stats1 <- function(returns) {
    df <- data.frame(matrix(data = 0, nrow = ncol(returns) - 1, ncol = 3))
    df[, 1] <- colnames(returns)[-1]
    df[, 2] <- sapply(returns[, -1], mean)
    df[, 3] <- sapply(returns[, -1], sd)
    
    days <- as.numeric(as.Date(returns[2, 1], format = "%b %d, %Y") - as.Date(returns[1, 1], format = "%b %d, %Y"))
    
    # Annualize information (assume 252 trading days in a year)
    df[, 2] <- (1 + df[, 2])^(252 / days) - 1
    df[, 3] <- df[, 3] * sqrt(252 / days)
    
    colnames(df) <- c('Annualized Return (%)', 'Average', 'SD')
    return(df)
  }
  
  # Calculate cross correlations for each stock
  stats2 <- function(returns) {
    df <- data.frame(cor(returns[, -1]))
    df$Correlations <- row.names(df)
    
    # Move Correlations column to beginning and drop date column
    df <- df[, c(ncol(df), seq(from = 1, to = ncol(df) - 1, by = 1))]
    return(df)
  }
  
  # Generate sample portfolios for risk-reward scatterplot
  portfolios <- function(stats1, stats2) {
    trials <- 1500
    stats2 <- stats2[, -1]              # Drop row headers
    
    df <- data.frame(matrix(data = 0, nrow = trials, ncol = ncol(stats2) + 3))
    colnames(df) <- c(colnames(stats2), "Expected Return", "SD Return", "Type")
    
    progress$set(value = 0.1)
    
    # Randomly generate weights for all stocks except 1
    for (k in 1:(ncol(stats2) - 1)) {
      df[, k] <- runif(trials, -2, 2)   # Weights between -2 and 2
    }
    
    # Add trials where we only invest in one stock
    for (k in 1:ncol(stats2)) {
      df[trials + k, ] <- 0
      df[trials + k, k] <- 1
    }
    
    progress$set(value = 0.15)
    df$Type <- 'Efficient'        # Initially label all as 'Efficient' portfolios
    
    for (k in 1:nrow(df)) {
      df[k, ncol(stats2)] <- 1 - sum(df[k, c(seq(from = 1, to = ncol(stats2) - 1, by = 1))])  # Calculate final weight
      weights <- as.matrix(df[k, c(seq(from = 1, to = ncol(stats2)))])
      returns <- as.matrix(stats1$Average)
      deviations <- as.matrix(stats1$SD)
      df[k, ncol(stats2) + 1] <- weights %*% returns
      
      variance <- 0
      for (m in 1:length(weights)) {
        for (n in 1:length(weights)) {
          variance <- variance + weights[m] * weights[n] * deviations[m] * deviations[n] * stats2[m, n]
        }
      }
      
      df[k, ncol(stats2) + 2] <- variance^(1/2)
      
      progress$set(value = ((k/nrow(df)) * .45 + .15))
    }
    
    # Remove trials where the final weight was greater than 2 or less than -2
    for (k in 1:length(weights)) {
      df <- df[which(abs(df[, k]) <= 2), ]
    }
    
    df <- df[order(df$'Expected Return', decreasing = TRUE), ]
    
    # Label portfolios inefficient if they have less return and more risk than others
    for (k in 1:nrow(df)) {
      if (df[k, 'Type'] == 'Efficient') {
        df[which((df$'Expected Return' < df[k, 'Expected Return']) & 
                   (df$'SD Return' > df[k, 'SD Return'])), 'Type'] <- 'Inefficient'
      }
    }
    
    progress$set(value = 0.7)
    
    for (k in 1:length(weights)) {
      df[which(df[, k] == 1), 'Type'] = 'Individual Asset'
    }
    
    # We don't need to keep all inefficient porfolios; only keep 250
    temp <- rbind(df[which(df$Type == 'Efficient'), ], df[which(df$Type == 'Individual Asset'), ])
    df <- rbind(temp, df[sample(which(df$Type == 'Inefficient'), 250), ])
    
    progress$set(value = 0.8)
    
    return(df)
  }
  
  # Generate data table of returns
  output$retTable <- renderChart2({
    progress <<- Progress$new(session)
    progress$set(message = 'Calculating Returns', value = 0.0)
    
    saved <- returns()
    
    progress$set(message = 'Creating Data Table', value = 0.8)
    
    colnames(saved)[1] <- 'Return (%)'
    saved[, -1] <- lapply(saved[, -1], function(x) round(100 * x, digits = 1))
    
    progress$set(value = 1)
    progress$close()
    
    # Disable changing display amount and search box
    dTable(saved, sPaginationType = 'two_button', iDisplayLength = 15, bFilter = FALSE, bLengthChange = FALSE)
  })

  # Generate line graph showing returns over time
  output$retTime <- renderChart({
    
    progress <<- Progress$new(session)
    progress$set(message = 'Creating Chart', value = 0.0)
    
    saved <- returns()
    saved[, -1] <- lapply(saved[, -1], function(x) round(100 * x, digits = 1))
    saved$Date <- as.numeric(as.POSIXct(as.Date(saved$Date, format = "%b %d, %Y")), 
                             origin = "1970-01-01") * 1000
    
    progress$set(value = 0.8)
    
    h1 <- Highcharts$new()
    h1$tooltip(shared = TRUE)
    h1$legend(enabled = TRUE, floating = TRUE, verticalAlign = 'top', y = 15, x = 20)
    h1$plotOptions(line = list(marker = list(enabled = FALSE)))
    h1$chart(zoomType = "x")
    
    # Generate array of (x, y) arrays
    for (k in 2:ncol(saved)) {
      points <- list()
      for (j in 1:nrow(saved)) {
        points[[j]] <- c(saved[j, 1], saved[j, k])
      }
      h1$series(name = colnames(saved)[k], type = 'line', data = points,
                tooltip = list(pointFormat = "<b>{series.name}:</b> {point.y:.1f}<br>"))
    }
    
    # Don't let us zoom in more than 7 days
    h1$xAxis(type = 'datetime', minRange = 7*24*60*60*1000, title = list(text = "Date"),
             dateTimeLabelFormats = list(day = '%b %e', week = '%b %e', month = '%b %Y', year = '%Y'))
    h1$yAxis(title = list(text = "Return (%)"))
    
    progress$set(value = 1)
    progress$close()
    
    h1$set(dom = 'retTime')
    return(h1)
  })
  
  # Generate histogram of returns
  output$retHist <- renderChart({
    
    progress <<- Progress$new(session)
    progress$set(message = 'Creating Chart', value = 0.0)
    
    saved <- histogram(returns())
    saved$Returns <- round(100 * saved$Returns, digits = 1)
    
    h2 <- Highcharts$new()
    h2$tooltip(shared = TRUE, formatter = "#! function() { var s = '';
                $.each(this.points, function(i, point) {
                    s += '<b>' + point.series.name + '</b>: ' + point.y + '<br/>';
                });
                
                return s; } !#")
    
    progress$set(value = 0.8)
    
    h2$legend(enabled = TRUE, floating = TRUE, verticalAlign = 'top', align = 'right', y = 10, x = -10)
    h2$plotOptions(series = list(marker = list(enabled = FALSE)))
    
    h2$xAxis(title = list(text = "Return (%)"), categories = saved$Returns, labels = list(x = 24, y = 15))
    h2$yAxis(title = list(text = "Count"), allowDecimals = FALSE)
    
    for (k in 2:ncol(saved)) {
      h2$series(name = colnames(saved)[k], type = 'areaspline', data = saved[, k])
    }
    
    progress$set(value = 1)
    progress$close()
    
    h2$set(dom = 'retHist')
    return(h2)
  })
  
  # Disable table with avg and sd stats
  output$pftTable1 <- renderChart2({
    
    progress <<- Progress$new(session)
    progress$set(message = 'Calculating Statistics', value = 0.0)
    
    saved <- stats1(returns())
    
    progress$set(value = 0.5)
    
    saved[, -1] <- lapply(saved[, -1], function(x) round(100 * x, digits = 1))
    
    progress$set(value = 1)
    progress$close()
    
    # Disable changing pages, searching, or changing # of entries to show
    dTable(saved, bPagination = FALSE, bFilter = FALSE, bLengthChange = FALSE)
  })
  
  # Show data table with cross correlations
  output$pftTable2 <- renderChart2({
    saved <- stats2(returns())
    saved[, -1] <- lapply(saved[, -1], function(x) round(x, digits = 3))
    
    # Disable changing pages, searching, or changing # of entries to show
    dTable(saved, bPagination = FALSE, bFilter = FALSE, bLengthChange = FALSE)
  })  
  
  # Generate scatterplot of portfolios
  output$pftScatter <- renderChart({
    
    progress <<- Progress$new(session)
    progress$set(message = 'Generating Random Portfolios', value = 0.0)
    
    saved <- returns()
    pfts <- portfolios(stats1(saved), stats2(saved))
    
    pfts[, -ncol(pfts)] <- lapply(pfts[, -ncol(pfts)], function(x) round(100 * x, digits = 1))
    colnames(pfts)[c(ncol(pfts) - 2, ncol(pfts) - 1)] <- c('y', 'x')
    
    # Manually create the tooltip function to send to JS
    tooltipFunc <- paste('#! function() {\n', 
      'var s = \'\';\n',
      's += \'<b>Expected Return:</b> \' + this.point.y + \'%<br/>\';\n',
      's += \'<b>SD Return:</b> \' + this.point.x + \'%<br/><br/>\';\n', sep = '')
    
    progress$set(message = 'Creating Chart', value = 0.9)
    
    # Add info to tooltip function for each stock
    for (k in 2:ncol(saved)) {
      tooltipFunc <- paste(tooltipFunc, 's += \'<b>Weight ', colnames(saved)[k], 
                           ':</b> \' + this.point.', colnames(saved)[k], ' + \'%<br/>\';\n', sep = '')
    }
    tooltipFunc <- paste(tooltipFunc, 'return s; } !#', sep = '')
    
    h3 <- Highcharts$new()
    h3$chart(type = 'scatter', zoomType = 'xy')
    h3$legend(enabled = TRUE, floating = TRUE, verticalAlign = 'top', align = 'left', y = 15, x = 60)
    h3$plotOptions(series = list(marker = list(radius = 6, symbol = 'circle')))
    
    h3$series(name = 'Efficient', type = 'scatter', 
              data = toJSONArray2(pfts[which(pfts$Type == 'Efficient'), ], json = FALSE))
    h3$series(name = 'Inefficient', type = 'scatter', 
              data = toJSONArray2(pfts[which(pfts$Type == 'Inefficient'), ], json = FALSE))
    h3$series(name = 'Individual Asset', type = 'scatter', 
              data = toJSONArray2(pfts[which(pfts$Type == 'Individual Asset'), ], json = FALSE))
    
    h3$xAxis(title = list(text = "Standard Deviation of Return (%)"))
    h3$yAxis(title = list(text = "Expected Return (%)"))
    h3$tooltip(enabled = TRUE, crosshairs = list(TRUE, TRUE), shared = TRUE, formatter = tooltipFunc)
    
    progress$set(value = 1)
    progress$close()
    
    h3$set(dom = 'pftScatter')
    return(h3)
  })
})