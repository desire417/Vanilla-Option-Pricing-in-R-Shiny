library(shiny)
library(shinydashboard)
library(RQuantLib)
library(LSMonteCarlo)
library(ggplot2)

#Header <- dashboardHeader(title = "Basic dashboard")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Pricing", tabName = "Pricing"),
    menuItem("LSMonteCarlo", tabName = "LSMC"),
    menuItem("Option Strategies", tabName = "Strategy")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Pricing",
            fluidPage(
              sidebarLayout(
                sidebarPanel(
                  radioButtons("type", "Option Type", 
                               c("call", "put"),
                               selected = "call", inline = T),
                  radioButtons("eu_am", "Option Type",
                               list("European" = 1, "American(Finite Difference)" = 2),
                               selected = 1, inline = T),
                  sliderInput("strike", "Strike Price", value = 100,
                              min = 1, max = 250),
                  sliderInput("spot", "Current Value Underlying",
                              value = 100, min = 1, max = 250),
                  sliderInput("riskfree", "Risk-Free Rate (in %)", value = 2,
                              min = 0, max = 20),
                  sliderInput("sigma", "Volatility (in %)", value = 20,
                              min = 0, max = 50),
                  sliderInput("dividend", "Dividend Yield (in %)", value = 0,
                              min = 0, max =20),
                  sliderInput("maturity", "Matuirty (in years)", value = 1,
                              min = 0, max = 5, step = 0.1)
                ),
                
                mainPanel(
                  tabsetPanel(
                    tabPanel("Pricing",
                             tableOutput("price"),
                             plotOutput("price_p")
                    ),
                    tabPanel("Greeks",
                             tableOutput("greeks"),
                             fluidRow(column(width = 4,
                                             plotOutput("greek_value")),
                                      column(width = 4,
                                             plotOutput("greek_delta")),
                                      column(width = 4,
                                             plotOutput("greek_gamma"))
                             ),
                             fluidRow(column(width = 4,
                                             plotOutput("greek_vega")),
                                      column(width = 4,
                                             plotOutput("greek_theta")),
                                      column(width = 4,
                                             plotOutput("greek_rho"))
                             )
                   )
                  )
                )
              )
            )
    ),
    tabItem(tabName = "LSMC",
            fluidPage(
              sidebarLayout(
                sidebarPanel(
                  sliderInput("n", "n (Number of paths simulated)", value = 100,
                              min = 1, max = 250),
                  sliderInput("m", "m (Number of time steps in the simulation)", value = 100,
                              min = 1, max = 250),
                  h4("Variance Reduction"),
                  radioButtons("vr", "Variance Reduction Method",
                               list("No" = 1, "Antithetic Variables" = 2, "Control Variates" = 3),
                               selected = 2, inline = T),
                  
                  
                  sliderInput("strike_mc", "Strike Price", value = 100,
                              min = 1, max = 250),
                  sliderInput("spot_mc", "Current Value Underlying",
                              value = 100, min = 1, max = 250),
                  sliderInput("riskfree_mc", "Risk-Free Rate (in %)", value = 2,
                              min = 0, max = 20),
                  sliderInput("sigma_mc", "Volatility (in %)", value = 20,
                              min = 0, max = 50),
                  sliderInput("dividend_mc", "Dividend Yield (in %)", value = 0,
                              min = 0, max =20),
                  sliderInput("maturity_mc", "Matuirty (in years)", value = 1,
                              min = 0, max = 5, step = 0.1)
                ),
                
                mainPanel(
                  tabsetPanel(
                    tabPanel("Pricing",
                             tableOutput("mc_price"),
                             plotOutput("mc_p")
                    )
                  )
                )
              )
            )
    ),
    tabItem(tabName = "Strategy",
            fluidPage(
              sidebarLayout(
                sidebarPanel(
                  radioButtons("type_s", "Option Type", 
                               c("call", "put"),
                               selected = "call", inline = T),
                  radioButtons("position_s", "Position",
                               list("Long" = 1, "Short" = 2),
                               selected = 1, inline = T),
                  sliderInput("strike_s", "Strike Price", value = 100,
                              min = 1, max = 250),
                  sliderInput("spot_s", "Current Value Underlying",
                              value = 100, min = 1, max = 250),
                  sliderInput("riskfree_s", "Risk-Free Rate (in %)", value = 2,
                              min = 0, max = 20),
                  sliderInput("sigma_s", "Volatility (in %)", value = 20,
                              min = 0, max = 50),
                  sliderInput("dividend_s", "Dividend Yield (in %)", value = 0,
                              min = 0, max =20),
                  sliderInput("maturity_s", "Matuirty (in years)", value = 1,
                              min = 0, max = 5, step = 0.1),
                  actionButton("insert", "Add")
                ),
                
                mainPanel(
                  tabsetPanel(
                    tabPanel("Pricing",
                             tableOutput("test")
                    )
                  )
                )
              )
            )
    )
  )
)

ui <- dashboardPage(header = dashboardHeader(),
                    sidebar = sidebar,
                    body = body
)

server <- function(input, output, session){
  option_price <- reactive({
    if (input$eu_am == 1){
      df <- EuropeanOption(input$type, input$spot, input$strike, input$dividend * 0.01, 
                           input$riskfree * 0.01, input$maturity, input$sigma * 0.01)
    }else{
      df <- AmericanOption(input$type, input$spot, input$strike, input$dividend * 0.01, 
                           input$riskfree * 0.01, input$maturity, input$sigma * 0.01,
                           engine = "CrankNicolson")
    }
    return(df)
  })
  
  price_t <- reactive({
    S <- seq(1, 200, 1)
    if (input$eu_am == 1){
      f <- function(x, output){
        df <- EuropeanOption(input$type, x, input$strike, input$dividend * 0.01, 
                             input$riskfree * 0.01, input$maturity, input$sigma * 0.01)
        return(unlist(df))
      }
    }else{
      f <- function(x, output){
        df <- AmericanOption(input$type, x, input$strike, input$dividend * 0.01, 
                             input$riskfree * 0.01, input$maturity, input$sigma * 0.01,
                             engine = "CrankNicolson")
        return(unlist(df))
      }
    }
    df <- data.frame(S, do.call(rbind, lapply(S,f)))
    colnames(df)[1:2] <- c("Spot", "Price")
    return(df)
  })
  
  mc_option <- reactive({
    if (input$vr == 1){
      df <- AmerPutLSM(input$spot_mc, input$sigma_mc * 0.01, input$n, input$m, input$strike_mc,
                       input$riskfree_mc * 0.01, input$dividend_mc * 0.01, input$maturity_mc)
    }else if (input$vr == 2){
      df <- AmerPutLSM_AV(input$spot_mc, input$sigma_mc * 0.01, input$n, input$m, input$strike_mc,
                       input$riskfree_mc * 0.01, input$dividend_mc * 0.01, input$maturity_mc)
    }else{
      df <- AmerPutLSM_CV(input$spot_mc, input$sigma_mc * 0.01, input$n, input$m, input$strike_mc,
                       input$riskfree_mc * 0.01, input$dividend_mc * 0.01, input$maturity_mc)
    }
    return(df$price)
  })
  
  mc_plot_t <- reactive({
    S <- seq(1, 200, 5)
    if (input$vr == 1){
      f <- function(x, output){
        df <- AmerPutLSM(x, input$sigma_mc * 0.01, input$n, input$m, input$strike_mc,
                         input$riskfree_mc * 0.01, input$dividend_mc * 0.01, input$maturity_mc)
        return(df$price)
      }
    }else if(input$vr == 2){
      f <- function(x, output){
        df <- AmerPutLSM_AV(x, input$sigma_mc * 0.01, input$n, input$m, input$strike_mc,
                         input$riskfree_mc * 0.01, input$dividend_mc * 0.01, input$maturity_mc)
        return(df$price)
      }
    }else{
      f <- function(x, output){
        df <- AmerPutLSM_CV(x, input$sigma_mc * 0.01, input$n, input$m, input$strike_mc,
                         input$riskfree_mc * 0.01, input$dividend_mc * 0.01, input$maturity_mc)
        return(df$price)
      }
    }
    f2 <- function(x, output){
      df <- AmericanOption("put", x, input$strike_mc, input$dividend_mc * 0.01, 
                           input$riskfree_mc * 0.01, input$maturity_mc, input$sigma_mc * 0.01,
                           engine = "CrankNicolson")
      return(df$value)
    }
    df <- data.frame(S, do.call(rbind, lapply(S,f)), do.call(rbind, lapply(S,f2)))
    colnames(df) <- c("Spot", "Price", "Bench")
    return(df)
  })
  
  dynamic_df <- reactiveValues(
    a = data.frame()
  )
  
  observeEvent(input$insert, {
    p <- option_price()
    df <- data.frame(input$type, input$spot, p$value)
    colnames(df) <- c("Type", "Underlying Price", "Option Price")
    dynamic_df$a <- df
  })
  
  output$test <- renderTable({
    dynamic_df$a
  })
  
  output$price <- renderTable({
    p <- option_price()
    df <- data.frame(input$type, input$spot, p$value)
    colnames(df) <- c("Type", "Underlying Price", "Option Price")
    return(df)
  })
  
  output$price_p <- renderPlot({
    ggplot(price_t(), aes(x=Spot, y=Price)) + 
      geom_point() +
      geom_vline(xintercept = input$strike, colour="grey", linetype = "dashed")
  })
  
  output$mc_p <- renderPlot({
    ggplot(mc_plot_t(), aes(x=Spot)) + 
      geom_point(aes(y = Price, colour = "Least Square MC")) +
      geom_line(aes(y = Bench, colour = "Finite Difference")) + 
      geom_vline(xintercept = input$strike_mc, colour="grey", linetype = "dashed")
  })
  
  output$greeks <- renderTable({
    t(do.call(rbind, option_price()))
  })
  
  output$mc_price <- renderTable({
    p <- mc_option()
    df <- data.frame("put", input$spot_mc, p)
    colnames(df) <- c("Type", "Underlying Price", "Option Price")
    return(df)
  })
  
  output$greek_value <- renderPlot({
    ggplot(price_t(), aes(x=Spot, y=Price)) + 
      geom_point() +
      geom_vline(xintercept = input$strike, colour="grey", linetype = "dashed")
  })
  
  output$greek_delta <- renderPlot({
    ggplot(price_t(), aes(x=Spot, y=delta)) + 
      geom_point() +
      geom_vline(xintercept = input$strike, colour="grey", linetype = "dashed")
  })
  
  output$greek_gamma <- renderPlot({
    ggplot(price_t(), aes(x=Spot, y=gamma)) + 
      geom_point() +
      geom_vline(xintercept = input$strike, colour="grey", linetype = "dashed")
  })
  
  output$greek_vega <- renderPlot({
    ggplot(price_t(), aes(x=Spot, y=vega)) + 
      geom_point() +
      geom_vline(xintercept = input$strike, colour="grey", linetype = "dashed")
  })
  
  output$greek_theta <- renderPlot({
    ggplot(price_t(), aes(x=Spot, y=theta)) + 
      geom_point() +
      geom_vline(xintercept = input$strike, colour="grey", linetype = "dashed")
  })
  
  output$greek_rho <- renderPlot({
    ggplot(price_t(), aes(x=Spot, y=rho)) + 
      geom_point() +
      geom_vline(xintercept = input$strike, colour="grey", linetype = "dashed")
  })
  
}

shinyApp(ui, server)
