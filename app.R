library(DT)
library(shiny)
library(lubridate)
library(tidyverse)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    theme = shinythemes::shinytheme("paper"),
    
    # Application title
    titlePanel("Property Investment Calculator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("paid_cost",
                        "Paid Cost: ", 760000),
            numericInput("est_sold_price",
                         "Estiamted Sold Price: ", 880000),
            numericInput("down_payment_perc",
                         "Down Payment Percentage: ", min = 0, max = 100, 20),
            numericInput("amort_years",
                         "Åmortization Years: ", 30),
            numericInput("mortgage_interest",
                         "Mortgage Interest: ", 3),
            numericInput("land_tx_tax",
                         "Land Transfer Tax: ", 15000),
            numericInput("prop_tx_tax",
                         "Property Tax: ", 3000),
            numericInput("num_investors",
                         "Number of Investors: ", 5),
            numericInput("expected_rental",
                         "Rental Price: ", 2200),
            numericInput("capital_gains_tax",
                         "Capital Gains Tax (%): ", 15),
            numericInput("lawyer_fees",
                         "Lawyer Fees: ", 810),
            numericInput("mort_break_mth",
                         "Months for Mortgage Break Penalty: ", 3)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h2("Summary of Numbers"),
            h5(
                htmlOutput("mortgage_requested")
            ),
            br(),
            hr(),
            h2("ROI Table"),
            br(),
            dataTableOutput("roi_table"),
            br(),
            hr(),
            h2("Amortization Table"),
            br(),
            dataTableOutput("amortization_table"),
            hr()
            )
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    values <- reactiveValues()
    
    dollarFormat <- function(x){
        format(x, nsmall = 2, big.mark = ",")
    }
    
    output$mortgage_requested <- renderUI({
        
        mortgage_monthly <- as.numeric(values$mortgage_monthly)
        
        down_payment <- as.numeric(input$paid_cost) * as.numeric(input$down_payment_perc)/100
        down_payment_show  <- dollarFormat(round(down_payment, 2))
        
        mortgage_need <- dollarFormat(round(as.numeric(input$paid_cost) - down_payment, 2))  # 1,000.6
        total_profit  <- dollarFormat(input$est_sold_price - input$paid_cost)
        cg_tax        <- dollarFormat(round((input$est_sold_price - input$paid_cost) * 0.15, 2))
        mort_penalty  <- dollarFormat(round(mortgage_monthly * as.numeric(input$mort_break_mth), 2))
        lawyer_fees   <- dollarFormat(input$lawyer_fees)
        
        HTML(
            paste0("The total down payment is $", down_payment_show, ". The total mortgage requested is $", mortgage_need, ".", br(), br(),
                    "The costs associated with the property include: (a) Capital Gains Tax at 15%: $", cg_tax, ", (b) Mortgage Penalty Break: $", mort_penalty,
                    " and , (c) Laywer Fees: $", lawyer_fees)
        )
        
    })
    
    output$amortization_table <- renderDataTable({
        P <- as.numeric(input$paid_cost) * (1 - input$down_payment_perc/100)
        n <- input$amort_years * 12
        i <- input$mortgage_interest/100/12
        
        A <- (P*i*(1+i)^n)/((1+i)^n - 1)
        values$mortgage_monthly <- A
        
        df <- data.frame(
            date = 1:n,
            payment = rep(A, n),
            principal = rep(NA, n),
            interest = rep(NA, n),
            total_interest = rep(NA, n),
            balance = rep(NA, n)
        )
        
        for (k in 1:n){
            
            if (k != 1){
                
                int_paid <- df[k-1, "balance"] * i
                df[k, "interest"] <- int_paid
                df[k, "principal"] <- A - int_paid
                df[k, "total_interest"] <- sum(df[, "interest"], na.rm = TRUE)
                df[k, "balance"] <- df[k-1, "balance"] - df[k, "principal"]
                
            } else {
                
                int_paid <- P*i
                df[k, "interest"] <- int_paid
                df[k, "principal"] <- A - int_paid
                df[k, "total_interest"] <- sum(df[, "interest"], na.rm = TRUE)
                df[k, "balance"] <- P - df[k, "principal"]
                
            }
            
            df[n, "balance"] <- 0
            
            df <- round(df, 2)
            
        }
        
        names(df) <- c("Month", "Payment", "Principal", "Interest", "Total Interest", "Balance")
        values$amor_table <- df
        
        df_out <- DT::datatable(df) %>% 
            formatCurrency(2:6) %>%
            formatStyle(names(df), fontSize = '150%') %>%
            formatStyle('Balance', color = 'red')
        
        return(df_out)
        
    })

    output$roi_table <- renderDataTable({
        
        # Extract from amortization table
        mortgage_monthly <- isolate(values$mortgage_monthly)
        amor_table <- isolate(values$amor_table)
        
        # Parameters to set
        num_investors <- input$num_investors
        num_years <- 5
        down_payment <- input$paid_cost * input$down_payment_perc/100
        
        # Revenue (price sold)
        total_profit <- input$est_sold_price - input$paid_cost

        # Costs (laywer fees, capital gains, mortgage penalty)
        lawyer_fees <- input$lawyer_fees
        cg_tax = round(total_profit * 0.15, 2)
        mort_penalty = round(mortgage_monthly * input$mort_break_mth, 2)
        
        # Maintenance cost paid to date (mortgage, property tax)

        df <- data.frame(
                Month = 12 * 1:5,
                price_sold = input$est_sold_price) %>%
            left_join(amor_table %>% select(Month, Balance), by = c("Month")) %>%
            mutate(net_profit = price_sold - lawyer_fees - cg_tax - mort_penalty - Balance - input$land_tx_tax) %>%
            mutate(invested_money_per_investor = (down_payment + mortgage_monthly*Month - input$expected_rental*Month + input$prop_tx_tax*Month/12)/num_investors) %>%
            mutate(return_per_investor = net_profit/num_investors) %>%
            mutate(profit_per_investor = return_per_investor - invested_money_per_investor) %>%
            mutate(roi = ((return_per_investor/invested_money_per_investor)^(1/(Month/12)) - 1))
        
        names(df) <- c("Month", "Sold Price", "Mortgage Owed", "Net Profit", "Invested Money per Investor", "Return per Investor", "Profit per Investor", "Annual ROI")
        
        df_out <- DT::datatable(df) %>%
            formatCurrency(2:7) %>%
            formatPercentage(8) %>%
            formatStyle(names(df), fontSize = '150%') %>%
            formatStyle(c('Sold Price', 'Net Profit', 'Profit per Investor'),  color = 'green')
            #formatStyle(c('Lawyer Fees', 'Capital Gains Tax', 'Mortgage Penalty', 'Mortgage Owed'),  color = 'red') %>%
        
        return(df_out)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
