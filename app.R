# ==============================
# Shiny App for Profit Optimization
# ==============================

library(shiny)
library(quantmod)
library(rsconnect)

# List of Assets for Selection
assets <- c("AAPL", "NVDA", "META", "GOOG", "TSLA", "MSFT", "QCOM", "SPY", "AMD", "TSM", "BHP")

# Download Stock Data from Yahoo Finance
getSymbols(assets, src = "yahoo")

# UI 
ui <- fluidPage(
  titlePanel("Referral Bonus Profit Optimization"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("num_referrals", 
                   "Enter the number of referrals:", 
                   value = 10, 
                   min = 1, max = 10),  # Referral Max is 10
      
      actionButton("calculate_profit", "Calculate Profit from Referrals"),
      
      hr(),  # Horizontal line for visual separation
      
      selectInput("asset1", 
                  "Choose Asset 1:",
                  choices = assets,
                  selected = "AAPL"),
      
      numericInput("asset1_allocation", 
                   "Allocate % of Referral Bonus to Asset 1:", 
                   value = 50, 
                   min = 0, max = 100),
      
      selectInput("asset2", 
                  "Choose Asset 2:",
                  choices = assets,
                  selected = "NVDA"),
      
      numericInput("asset2_allocation", 
                   "Allocate % of Referral Bonus to Asset 2:", 
                   value = 50, 
                   min = 0, max = 100)
    ),
    
    mainPanel(
      uiOutput("initial_summary"),
      fluidRow(
        column(6, uiOutput("investment_summary1")),
        column(6, uiOutput("investment_summary2"))
      ),
      br(), br(), br(), br(), 
      plotOutput("investment_plot", height = "600px")  # Graph Height
    )
  )
)

# Server Logic
server <- function(input, output) {
  
  # Initial Calculation of Profit from Referrals
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
  output$investment_summary1 <- renderUI({
    total_referral_bonus <- 200 * input$num_referrals
    asset1_percentage <- input$asset1_allocation / 100
    
    asset1_prices <- get(input$asset1)
    
    start_date <- Sys.Date() - 365
    end_date <- Sys.Date()
    
    asset1_prices_filtered <- asset1_prices[paste(start_date, end_date, sep = "/")]
    
    asset1_start_price <- as.numeric(Cl(asset1_prices_filtered[1]))
    asset1_end_price <- as.numeric(Cl(asset1_prices_filtered[nrow(asset1_prices_filtered)]))
    
    asset1_initial_investment <- total_referral_bonus * asset1_percentage
    asset1_investment_growth <- (asset1_initial_investment / asset1_start_price) * asset1_end_price
    
    asset1_profit_usd <- asset1_investment_growth - asset1_initial_investment
    asset1_percentage_change <- ((asset1_investment_growth - asset1_initial_investment) / asset1_initial_investment) * 100
    
    HTML(paste0("<h4>Investment Summary: ", input$asset1, "</h4>",
                "<p><strong>Initial Stock Price:</strong> $", format(asset1_start_price, big.mark = ","),
                "<br><strong>Current Stock Price:</strong> $", format(asset1_end_price, big.mark = ","),
                "<br><strong>Initial Investment:</strong> $", format(asset1_initial_investment, big.mark = ","),
                "<br><strong>Current Value:</strong> $", format(asset1_investment_growth, big.mark = ","),
                "<br><strong>Profit/Loss:</strong> $", format(asset1_profit_usd, big.mark = ","),
                "<br><strong>Percentage Change:</strong> ", sprintf("%.2f", asset1_percentage_change), "%</p>"))
  })
  
  # Investment Summary for Asset 2
  output$investment_summary2 <- renderUI({
    total_referral_bonus <- 200 * input$num_referrals
    asset2_percentage <- input$asset2_allocation / 100
    
    asset2_prices <- get(input$asset2)
    
    start_date <- Sys.Date() - 365
    end_date <- Sys.Date()
    
    asset2_prices_filtered <- asset2_prices[paste(start_date, end_date, sep = "/")]
    
    asset2_start_price <- as.numeric(Cl(asset2_prices_filtered[1]))
    asset2_end_price <- as.numeric(Cl(asset2_prices_filtered[nrow(asset2_prices_filtered)]))
    
    asset2_initial_investment <- total_referral_bonus * asset2_percentage
    asset2_investment_growth <- (asset2_initial_investment / asset2_start_price) * asset2_end_price
    
    asset2_profit_usd <- asset2_investment_growth - asset2_initial_investment
    asset2_percentage_change <- ((asset2_investment_growth - asset2_initial_investment) / asset2_initial_investment) * 100
    
    HTML(paste0("<h4>Investment Summary: ", input$asset2, "</h4>",
                "<p><strong>Initial Stock Price:</strong> $", format(asset2_start_price, big.mark = ","),
                "<br><strong>Current Stock Price:</strong> $", format(asset2_end_price, big.mark = ","),
                "<br><strong>Initial Investment:</strong> $", format(asset2_initial_investment, big.mark = ","),
                "<br><strong>Current Value:</strong> $", format(asset2_investment_growth, big.mark = ","),
                "<br><strong>Profit/Loss:</strong> $", format(asset2_profit_usd, big.mark = ","),
                "<br><strong>Percentage Change:</strong> ", sprintf("%.2f", asset2_percentage_change), "%</p>"))
  })
  
  output$investment_plot <- renderPlot({
    total_referral_bonus <- 200 * input$num_referrals
    
    asset1_percentage <- input$asset1_allocation / 100
    asset2_percentage <- input$asset2_allocation / 100
    
    asset1_prices <- get(input$asset1)
    asset2_prices <- get(input$asset2)
    
    start_date <- Sys.Date() - 365
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
    
    asset1_percentage_change <- ((asset1_investment_growth - asset1_initial_investment) / asset1_initial_investment) * 100
    asset2_percentage_change <- ((asset2_investment_growth - asset2_initial_investment) / asset2_initial_investment) * 100
    
    dates <- as.Date(index(asset1_prices_filtered))
    asset1_values <- Cl(asset1_prices_filtered) * (asset1_initial_investment / asset1_start_price)
    asset2_values <- Cl(asset2_prices_filtered) * (asset2_initial_investment / asset2_start_price)
    
    plot(dates, asset1_values, type = "l", col = "blue", lwd = 2, ylim = range(c(asset1_values, asset2_values)),
         ylab = "Value in USD", xlab = "Date", main = "Investment Value Over Time")
    lines(dates, asset2_values, col = "red", lwd = 2)
    legend("topright", legend = c(input$asset1, input$asset2), col = c("blue", "red"), lty = 1, lwd = 2)
    
    text(x = end_date + 5, y = asset1_values[nrow(asset1_prices_filtered)], 
         labels = paste0(sprintf("%.2f", asset1_percentage_change), "%"), pos = 4, 
         col = ifelse(asset1_percentage_change > 0, "green", "red"), cex = 1.2, offset = 1)
    text(x = end_date + 5, y = asset2_values[nrow(asset2_prices_filtered)], 
         labels = paste0(sprintf("%.2f", asset2_percentage_change), "%"), pos = 4, 
         col = ifelse(asset2_percentage_change > 0, "green", "red"), cex = 1.2, offset = 1)
  })
}

# Run the Application 
shinyApp(ui = ui, server = server)
