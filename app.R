# ====================================
# Shiny App for eToro Referral Strategy
# ====================================

library(shiny)
library(quantmod)
library(TTR)
library(xts)


all_assets <- c("AAPL", "NVDA", "META", "GOOG", "TSLA", "MSFT", "QCOM", "SPY", "AMD", "TSM", "BHP")
getSymbols(all_assets, src = "yahoo")

ui <- fluidPage(
  titlePanel("eToro Referral Bonus Profit Optimization Strategy"),
  
 
  fluidRow(
    column(12,  
           p("Step-by-step guide:"),
           tags$ol(
             tags$li("Referral Request: The strategy begins by getting up to 10 individuals to set up an eToro account using your referral link."),
             tags$li("Funding the Account: Once they're registered, you transfer $500 USD to each individual’s bank account."),
             tags$li("Deposit and Investment: The referred individuals deposit the $500 USD into their eToro accounts and invest $100 USD in a stock of their choice. This investment is their reward for participating, as they get to keep the stock."),
             tags$li("Withdrawal Process: After the investment is made, they withdraw the remaining $400 USD from their eToro accounts. This withdrawal will incur a $5 USD fee, leaving them with $395 USD."),
             tags$li("Returning the Funds: The referred individuals then transfer the remaining $395 USD back to your bank account."),
             tags$li("Referral Bonus: For each successful referral, you receive a $200 USD bonus in your eToro account.")
           ),
           p("Outcome: By repeating this process with 10 individuals, you invest $5,000 USD ($500 per person). After the withdrawals, you recover $3,950 USD ($395 per person) and earn $2,000 USD in referral bonuses, resulting in a net profit of $950 USD."),
           p("Simulation: This app simulates what the value of their invested money would be if they had implemented this strategy one year ago. It calculates how the $2000 investment in a stock of their choice would have grown (or shrunk) over the past year, based on actual stock prices.")
    )
  ),
  
  hr(),  
  
  # layout
  sidebarLayout(
    sidebarPanel(
      numericInput("num_referrals", 
                   "Enter the number of referrals:", 
                   value = 10, 
                   min = 1, max = 10),  # Referral Max is 10
      
      actionButton("calculate_profit", "Calculate Profit from Referrals"),
      
      hr(),  
      
      selectInput("asset1", 
                  "Choose Asset 1:",
                  choices = all_assets,
                  selected = "AAPL"),
      
      numericInput("asset1_allocation", 
                   "Allocate % of Referral Bonus to Asset 1:", 
                   value = 100, 
                   min = 0, max = 100),
      
      selectInput("asset2", 
                  "Choose Asset 2:",
                  choices = all_assets,
                  selected = "NVDA"),
      
      numericInput("asset2_allocation", 
                   "Allocate % of Referral Bonus to Asset 2:", 
                   value = 100, 
                   min = 0, max = 100),
      
      hr(),
      
      selectInput("currency", 
                  "Choose your currency:",
                  choices = c("USD", "EUR", "GBP", "AUD"),
                  selected = "USD"),
      
      hr(),
      
      checkboxInput("show_benchmark", 
                    "Compare with benchmark S&P500 (100% Allocation)", 
                    value = TRUE),
      
      checkboxInput("show_risk", 
                    "Show Risk Analysis (Standard Deviation)", 
                    value = TRUE),
      
      numericInput("time_horizon", 
                   "Historical Simulation Period (Years):", 
                   value = 1, 
                   min = 1, max = 5)
    ),
    
    mainPanel(
      uiOutput("initial_summary"),
      fluidRow(
        column(6, uiOutput("investment_summary1")),
        column(6, uiOutput("investment_summary2"))
      ),
      br(), br(), br(), br(), 
      plotOutput("investment_plot", height = "600px")  # Height of graph
    )
  )
)
server <- function(input, output) {
  
  # Conversion rates for currency conversion
  currency_rates <- reactive({
    switch(input$currency,
           "USD" = 1,
           "EUR" = 0.85,
           "GBP" = 0.75,
           "AUD" = 1.3)
  })
  
  # Calculation of Profit from Referrals
  observeEvent(input$calculate_profit, {
    total_initial_investment <- 500 * input$num_referrals
    total_referral_bonus <- 200 * input$num_referrals
    total_withdrawn <- (395 * input$num_referrals)
    total_return <- total_withdrawn + total_referral_bonus
    net_profit <- total_return - total_initial_investment
    
    output$initial_summary <- renderUI({
      HTML(paste0("<h3>Profit from Referrals</h3>",
                  "<p><strong>Initial Investment:</strong> $", format(total_initial_investment, big.mark = ","),
                  "<br><strong>Referral Bonus Total:</strong> $", format(total_referral_bonus, big.mark = ","),
                  "<br><strong>Withdrawn Investment Total:</strong> $", format(total_withdrawn, big.mark = ","),
                  "<br><strong>Total Return:</strong> $", format(total_return, big.mark = ","),
                  "<br><strong>Net Profit:</strong> $", format(net_profit, big.mark = ","), "</p>"))
    })
  })
  
  # Investment Summary for Asset 1
  observe({
    total_referral_bonus <- 200 * input$num_referrals
    asset1_percentage <- input$asset1_allocation / 100
    
    asset1_prices <- get(input$asset1)
    
    start_date <- Sys.Date() - (input$time_horizon * 365)
    end_date <- Sys.Date()
    
    asset1_prices_filtered <- asset1_prices[paste(start_date, end_date, sep = "/")]
    
    asset1_start_price <- as.numeric(Cl(asset1_prices_filtered[1]))
    asset1_end_price <- as.numeric(Cl(asset1_prices_filtered[nrow(asset1_prices_filtered)]))
    
    asset1_initial_investment <- total_referral_bonus * asset1_percentage
    asset1_investment_growth <- (asset1_initial_investment / asset1_start_price) * asset1_end_price
    
    asset1_profit_usd <- asset1_investment_growth - asset1_initial_investment
    asset1_percentage_change <- ((asset1_investment_growth - asset1_initial_investment) / asset1_initial_investment) * 100
    
    asset1_profit_converted <- asset1_profit_usd * currency_rates()
    
    risk_analysis <- if (input$show_risk) {
      asset1_sd <- sd(dailyReturn(asset1_prices_filtered))
      paste0("<br><strong>Standard Deviation (Risk):</strong> ", sprintf("%.4f", asset1_sd))
    } else {
      ""
    }
    
    output$investment_summary1 <- renderUI({
      HTML(paste0("<h4>Investment Summary: ", input$asset1, "</h4>",
                  "<p><strong>Initial Stock Price (", input$time_horizon, " Year(s) Ago):</strong> $", format(asset1_start_price, big.mark = ","),
                  "<br><strong>Current Stock Price:</strong> $", format(asset1_end_price, big.mark = ","),
                  "<br><strong>Initial Investment:</strong> $", format(asset1_initial_investment, big.mark = ","),
                  "<br><strong>Current Value:</strong> $", format(asset1_investment_growth, big.mark = ","),
                  "<br><strong>Profit/Loss:</strong> ", input$currency, " ", format(asset1_profit_converted, big.mark = ","),
                  "<br><strong>Percentage Change:</strong> ", sprintf("%.2f", asset1_percentage_change), "%", 
                  risk_analysis, "</p>"))
    })
  })
  
  # Investment Summary for Asset 2
  observe({
    total_referral_bonus <- 200 * input$num_referrals
    asset2_percentage <- input$asset2_allocation / 100
    
    asset2_prices <- get(input$asset2)
    
    start_date <- Sys.Date() - (input$time_horizon * 365)
    end_date <- Sys.Date()
    
    asset2_prices_filtered <- asset2_prices[paste(start_date, end_date, sep = "/")]
    
    asset2_start_price <- as.numeric(Cl(asset2_prices_filtered[1]))
    asset2_end_price <- as.numeric(Cl(asset2_prices_filtered[nrow(asset2_prices_filtered)]))
    
    asset2_initial_investment <- total_referral_bonus * asset2_percentage
    asset2_investment_growth <- (asset2_initial_investment / asset2_start_price) * asset2_end_price
    
    asset2_profit_usd <- asset2_investment_growth - asset2_initial_investment
    asset2_percentage_change <- ((asset2_investment_growth - asset2_initial_investment) / asset2_initial_investment) * 100
    
    asset2_profit_converted <- asset2_profit_usd * currency_rates()
    
    risk_analysis <- if (input$show_risk) {
      asset2_sd <- sd(dailyReturn(asset2_prices_filtered))
      paste0("<br><strong>Standard Deviation (Risk):</strong> ", sprintf("%.4f", asset2_sd))
    } else {
      ""
    }
    
    output$investment_summary2 <- renderUI({
      HTML(paste0("<h4>Investment Summary: ", input$asset2, "</h4>",
                  "<p><strong>Initial Stock Price (", input$time_horizon, " Year(s) Ago):</strong> $", format(asset2_start_price, big.mark = ","),
                  "<br><strong>Current Stock Price:</strong> $", format(asset2_end_price, big.mark = ","),
                  "<br><strong>Initial Investment:</strong> $", format(asset2_initial_investment, big.mark = ","),
                  "<br><strong>Current Value:</strong> $", format(asset2_investment_growth, big.mark = ","),
                  "<br><strong>Profit/Loss:</strong> ", input$currency, " ", format(asset2_profit_converted, big.mark = ","),
                  "<br><strong>Percentage Change:</strong> ", sprintf("%.2f", asset2_percentage_change), "%", 
                  risk_analysis, "</p>"))
    })
  })
  
  #  investment value over time plot
  observe({
    total_referral_bonus <- 200 * input$num_referrals
    
    asset1_percentage <- input$asset1_allocation / 100
    asset2_percentage <- input$asset2_allocation / 100
    
    asset1_prices <- get(input$asset1)
    asset2_prices <- get(input$asset2)
    
    start_date <- Sys.Date() - (input$time_horizon * 365)
    end_date <- Sys.Date()
    
    asset1_prices_filtered <- asset1_prices[paste(start_date, end_date, sep = "/")]
    asset2_prices_filtered <- asset2_prices[paste(start_date, end_date, sep = "/")]
    
    asset1_start_price <- as.numeric(Cl(asset1_prices_filtered[1]))
    asset1_end_price <- as.numeric(Cl(asset1_prices_filtered[nrow(asset1_prices_filtered)]))
    
    asset2_start_price <- as.numeric(Cl(asset2_prices_filtered[1]))
    asset2_end_price <- as.numeric(Cl(asset2_prices_filtered[nrow(asset2_prices_filtered)]))
    
    asset1_initial_investment <- total_referral_bonus * asset1_percentage
    asset2_initial_investment <- total_referral_bonus * asset2_percentage
    
    asset1_investment_growth <- (asset1_initial_investment / asset1_start_price) * asset1_end_price
    asset2_investment_growth <- (asset2_initial_investment / asset2_start_price) * asset2_end_price
    
    dates <- as.Date(index(asset1_prices_filtered))
    asset1_values <- Cl(asset1_prices_filtered) * (asset1_initial_investment / asset1_start_price)
    asset2_values <- Cl(asset2_prices_filtered) * (asset2_initial_investment / asset2_start_price)
    
    output$investment_plot <- renderPlot({
      plot(dates, asset1_values, type = "l", col = "darkblue", lwd = 2, ylim = range(c(asset1_values, asset2_values)),
           ylab = "Value in USD", xlab = "Date", main = "Investment Value Over Time", 
           xaxt = 'n', grid = TRUE)
      
      axis.Date(1, at = seq(from = min(dates), to = max(dates), by = "1 month"), format = "%b %Y")
      
      lines(dates, asset2_values, col = "darkred", lwd = 2)
      
      # horizontal lines for initial investments
      abline(h = asset1_initial_investment, col = "darkblue", lty = 2, lwd = 2)
      abline(h = asset2_initial_investment, col = "darkred", lty = 2, lwd = 2)
      
      # grid lines
      abline(h = seq(from = floor(min(c(asset1_values, asset2_values))), to = ceiling(max(c(asset1_values, asset2_values))), by = 500), col = "gray90", lty = 2)
      
      # legend on the left side
      legend("topleft", legend = c(paste(input$asset1, "(Value)"), 
                                   paste(input$asset2, "(Value)"),
                                   paste(input$asset1, "(Initial Investment)"),
                                   paste(input$asset2, "(Initial Investment)")),
             col = c("darkblue", "darkred", "darkblue", "darkred"), lty = c(1, 1, 2, 2), lwd = 2)
      
      # If benchmark comparison is selected
      if (input$show_benchmark) {
        spy_prices <- get("SPY")
        spy_prices_filtered <- spy_prices[paste(start_date, end_date, sep = "/")]
        spy_start_price <- as.numeric(Cl(spy_prices_filtered[1]))
        spy_end_price <- as.numeric(Cl(spy_prices_filtered[nrow(spy_prices_filtered)]))
        spy_initial_investment <- total_referral_bonus
        spy_investment_growth <- (spy_initial_investment / spy_start_price) * spy_end_price
        spy_values <- Cl(spy_prices_filtered) * (spy_initial_investment / spy_start_price)
        
        lines(dates, spy_values, col = "darkgreen", lwd = 2)
        legend("topright", legend = c("SPY (Value)", "SPY (Initial Investment)"), col = "darkgreen", lty = c(1, 2), lwd = 2)
      }
    })
  })
}

shinyApp(ui = ui, server = server)
