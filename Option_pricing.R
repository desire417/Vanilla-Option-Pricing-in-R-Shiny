library(shiny)
library(shinydashboard)
library(LSMonteCarlo)
library(ggplot2)
library(qrmtools)
source("util.R")

Header <- dashboardHeader(title = "Vanilla Option Pricing")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Basic Pricing", tabName = "Pricing"),
    menuItem("Option Strategies", tabName = "Strategy"),
    menuItem("LSMonteCarlo", tabName = "LSMC")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Pricing",
            fluidPage(
              sidebarLayout(
                sidebarPanel(
                  h3("Vanilla Option Input"),
                  radioButtons("type", "Option Type", 
                               c("call", "put"),
                               selected = "call", inline = T),
                  radioButtons("eu_am", "Option Style",
                               list("European" = 1, "American(Finite Difference)" = 2),
                               selected = 1, inline = T),
                  sliderInput("spot", "Underlying Asset Price (S)",
                              value = 100, min = 1, max = 200),
                  sliderInput("strike", "Strike Price (K)", value = 100,
                              min = 1, max = 200),
                  sliderInput("riskfree", "Risk-Free Rate (r) (in %)", value = 2,
                              min = 0, max = 20),
                  sliderInput("sigma", "Volatility (sigma) (in %)", value = 20,
                              min = 0, max = 50),
                  sliderInput("maturity", "Matuirty (T) (in years)", value = 1,
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
                  h3("American Option"),
                  h3("Least Square Monte Carlo Simulation"),
                  radioButtons("n", "n (Number of paths simulated)", 
                              c(100,500,1000,2000,5000), selected = 100, inline = T),
                  radioButtons("m", "m (Number of time steps in the simulation)", 
                               c(100,500,1000,2000,5000), selected = 100, inline = T),
                  h4("Variance Reduction"),
                  radioButtons("vr", "Variance Reduction Method",
                               list("No" = 1, "Antithetic Variables" = 2, "Control Variates" = 3),
                               selected = 2, inline = T),
                  sliderInput("spot_mc", "Underlying Asset Price (S)",
                              value = 100, min = 1, max = 200),
                  sliderInput("strike_mc", "Strike Price (K)", value = 100,
                              min = 1, max = 200),
                  sliderInput("riskfree_mc", "Risk-Free Rate (r) (in %)", value = 2,
                              min = 0, max = 20),
                  sliderInput("sigma_mc", "Volatility (sigma) (in %)", value = 20,
                              min = 0, max = 50),
                  sliderInput("dividend_mc", "Dividend Yield (q) (in %)", value = 0,
                              min = 0, max =20),
                  sliderInput("maturity_mc", "Matuirty (T) (in years)", value = 1,
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
                  h3("Option Strategies Input"),
                  radioButtons("type_s", "Option Type", 
                               c("call", "put"),
                               selected = "call", inline = T),
                  radioButtons("position_s", "Position",
                               c("Long", "Short"),
                               selected = "Long", inline = T),
                  h5("Given current underlying asset price is 100"),
                  sliderInput("strike_s", "Strike Price (K)", value = 100,
                              min = 1, max = 200),
                  sliderInput("riskfree_s", "Risk-Free Rate (r) (in %)", value = 2,
                              min = 0, max = 20),
                  sliderInput("sigma_s", "Volatility (sigma) (in %)", value = 20,
                              min = 0, max = 50),
                  sliderInput("maturity_s", "Matuirty (T) (in years)", value = 1,
                              min = 0, max = 5, step = 0.1),
                  h4("Click Add Option button to add option into option basket"),
                  actionButton("insert", "Add Option", icon("plus-circle")),
                  actionButton("reset", "Reset", icon("redo-alt"))
                ),
                
                mainPanel(
                  tabsetPanel(
                    tabPanel("Pricing",
                             plotOutput("comb"),
                             br(),
                             h4("Option Strategy Components:"),
                             tableOutput("portfolio")
                    )
                  )
                )
              )
            )
    )
  )
)

ui <- dashboardPage(header = Header,
                    sidebar = sidebar,
                    body = body
)

server <- function(input, output, session){
  option_price <- reactive({
    if (input$eu_am == 1){
      df <- Black_Scholes(0, input$spot, input$riskfree * 0.01, input$sigma * 0.01, 
                          input$strike, input$maturity, input$type)
    }else{
      df <- fdcn(input$spot, input$strike, input$riskfree * 0.01, input$maturity, 
                 input$sigma * 0.01, 50, 50, input$type, "american")
    }
    return(df)
  })
  
  price_t_p <- reactive({
    S <- seq(1, 200, 1)
    if (input$eu_am == 1){
      f <- function(x, output){
        v <- Black_Scholes(0, x, input$riskfree * 0.01, input$sigma * 0.01, 
                            input$strike, input$maturity, input$type)
        return(v)
      }
    }else{
      f <- function(x, output){
        v <- fdcn(x, input$strike, input$riskfree * 0.01, input$maturity, 
                   input$sigma * 0.01, 50, 50, input$type, "american")
        return(v)
      }
    }
    df <- data.frame(S, do.call(rbind, lapply(S,f)))
    colnames(df) <- c("Spot", "Price")
    return(df)
  })
  
  option_greek <- reactive({
    v <- Black_Scholes(0, input$spot, input$riskfree * 0.01, input$sigma * 0.01, 
                       input$strike, input$maturity, input$type)
    df <- BS_greeks(0, input$spot, input$riskfree * 0.01, input$sigma * 0.01, 
                        input$strike, input$maturity, input$type)
    df <- data.frame(df)
    df$Price <- v
    df$Spot <- input$spot
    df <- df[c(9, 8, 1, 5, 2, 3, 4)]
    return(df)
  })
  
  option_greek_p <- reactive({
    S <- seq(1, 200, 1)
    v <- Black_Scholes(0, S, input$riskfree * 0.01, input$sigma * 0.01, 
                       input$strike, input$maturity, input$type)
    df <- BS_greeks(0, S, input$riskfree * 0.01, input$sigma * 0.01, 
                    input$strike, input$maturity, input$type)
    df <- data.frame(df)
    df$Price <- v
    df$Spot <- S
    df <- df[c(9, 8, 1, 5, 2, 3, 4)]
    return(df)
  })
  
  mc_option <- reactive({
    if (input$vr == 1){
      df <- AmerPutLSM(input$spot_mc, input$sigma_mc * 0.01, as.numeric(input$n), as.numeric(input$m), input$strike_mc,
                       input$riskfree_mc * 0.01, input$dividend_mc * 0.01, input$maturity_mc)
    }else if (input$vr == 2){
      df <- AmerPutLSM_AV(input$spot_mc, input$sigma_mc * 0.01, as.numeric(input$n), as.numeric(input$m), input$strike_mc,
                       input$riskfree_mc * 0.01, input$dividend_mc * 0.01, input$maturity_mc)
    }else{
      df <- AmerPutLSM_CV(input$spot_mc, input$sigma_mc * 0.01, as.numeric(input$n), as.numeric(input$m), input$strike_mc,
                       input$riskfree_mc * 0.01, input$dividend_mc * 0.01, input$maturity_mc)
    }
    return(df$price)
  })
  
  mc_plot_t <- reactive({
    S <- seq(1, 200, 5)
    if (input$vr == 1){
      f <- function(x, output){
        df <- AmerPutLSM(x, input$sigma_mc * 0.01, as.numeric(input$n), as.numeric(input$m), input$strike_mc,
                         input$riskfree_mc * 0.01, input$dividend_mc * 0.01, input$maturity_mc)
        return(df$price)
      }
    }else if(input$vr == 2){
      f <- function(x, output){
        df <- AmerPutLSM_AV(x, input$sigma_mc * 0.01, as.numeric(input$n), as.numeric(input$m), input$strike_mc,
                         input$riskfree_mc * 0.01, input$dividend_mc * 0.01, input$maturity_mc)
        return(df$price)
      }
    }else{
      f <- function(x, output){
        df <- AmerPutLSM_CV(x, input$sigma_mc * 0.01, as.numeric(input$n), as.numeric(input$m), input$strike_mc,
                         input$riskfree_mc * 0.01, input$dividend_mc * 0.01, input$maturity_mc)
        return(df$price)
      }
    }
    f2 <- function(x, output){
      df <- fdcn(x, input$strike_mc, input$riskfree_mc * 0.01, input$maturity_mc, 
                 input$sigma_mc * 0.01, 50, 50, "put", "american")
      return(df)
    }
    df <- data.frame(S, do.call(rbind, lapply(S,f)), do.call(rbind, lapply(S,f2)))
    colnames(df) <- c("Spot", "Price", "Bench")
    return(df)
  })
  
  dynamic_df <- reactiveValues(a = data.frame(Position = character(), 
                                              Type = character(), 
                                              Underlying_Price = double(), 
                                              Strike_Price = double(), 
                                              Option_Price = double()))
  
  dynamic_p <- reactiveValues(a=0)
  
  dynamic_pf <- reactiveValues(a=0)
  
  observeEvent(input$insert, {
    posi <- input$position_s
    type <- input$type_s
    strike <- input$strike_s
    S <- seq(1, 200, 1)
    v <- Black_Scholes(0, 100, input$riskfree_s * 0.01, input$sigma_s * 0.01, 
                       input$strike_s, input$maturity_s, input$type_s)
    if (input$type_s == "call"){
      if (input$position_s == "Long"){
        payoff <- do.call(rbind, lapply(S-input$strike_s, function(x) max(x, 0))) - v
      }else{
        payoff <- -1 *do.call(rbind, lapply(S-input$strike_s, function(x) max(x, 0))) + v
      }
    }else{
      if (input$position_s == "Long"){
        payoff <- do.call(rbind, lapply(input$strike_s - S, function(x) max(x, 0))) - v
      }else{
        payoff <- -1 * do.call(rbind, lapply(input$strike_s - S, function(x) max(x, 0))) + v
      }
    }
    df <- data.frame(input$position_s, input$type_s, 100, input$strike_s, v)
    colnames(df) <- c("Position", "Type", "Underlying Price", "Strike Price", "Option Price")
    df2 <- data.frame(S, payoff)
    colnames(df2) <- c("Spot", "Price")
    
    if (nrow(dynamic_df$a) == 0){
      dynamic_pf$a <- df2
      dynamic_df$a <- df
      dynamic_p$a <- ggplot() + geom_line(aes(x=Spot, y=Price,
                                              colour = paste0(posi, type, strike)), data = df2)
    }else{
      last_pf <- dynamic_pf$a
      df3 <- data.frame(S, last_pf$Price + payoff)
      colnames(df3) <- c("Spot", "Price")
      dynamic_pf$a <- df3
      dynamic_df$a <- rbind(dynamic_df$a, df)
      dynamic_p$a <- dynamic_p$a + geom_line(aes(x=Spot, y=Price,
                                                 colour = paste0(posi, type, strike)), data = df2)
    }
  })
  
  observeEvent(input$reset,{
    dynamic_p$a <- 0
    dynamic_pf$a <- 0
    dynamic_df$a <- data.frame(Position = character(), 
                                  Type = character(), 
                                  Underlying_Price = double(), 
                                  Strike_Price = double(), 
                                  Option_Price = double())
  })
  
  output$portfolio <- renderTable({
    dynamic_df$a
  })
  
  output$price <- renderTable({
    p <- option_price()
    df <- data.frame(input$type, input$spot, p)
    colnames(df) <- c("Type", "Underlying Price", "Option Price")
    return(df)
  })
  
  output$price_p <- renderPlot({
    ggplot(price_t_p(), aes(x=Spot, y=Price)) + 
      geom_point() +
      geom_vline(xintercept = input$strike, colour="grey", linetype = "dashed") +
      theme_bw() + 
      xlab("Underlying Asset Price") + ylab("Option Price") +
      geom_text(aes(x=input$strike, label=paste0("K=", input$strike), y=0.5), colour="black")
  })
  
  output$mc_p <- renderPlot({
    ggplot(mc_plot_t(), aes(x=Spot)) + 
      geom_point(aes(y = Price, colour = "Least Square MC")) +
      geom_line(aes(y = Bench, colour = "Finite Difference")) + 
      geom_vline(xintercept = input$strike_mc, colour="grey", linetype = "dashed") +
      geom_text(aes(x=input$strike, label=paste0("K=", input$strike), y=0.5), colour="black") +
      theme_bw()
  })
  
  output$greeks <- renderTable({
    option_greek()
  })
  
  output$mc_price <- renderTable({
    p <- mc_option()
    df <- data.frame("put", input$spot_mc, p)
    colnames(df) <- c("Type", "Underlying Price", "Option Price")
    return(df)
  })
  
  output$greek_value <- renderPlot({
    ggplot(option_greek_p(), aes(x=Spot, y=Price)) + 
      geom_point() +
      geom_vline(xintercept = input$strike, colour="grey", linetype = "dashed")
  })
  
  output$greek_delta <- renderPlot({
    ggplot(option_greek_p(), aes(x=Spot, y=delta)) + 
      geom_point() +
      geom_vline(xintercept = input$strike, colour="grey", linetype = "dashed")
  })
  
  output$greek_gamma <- renderPlot({
    ggplot(option_greek_p(), aes(x=Spot, y=gamma)) + 
      geom_point() +
      geom_vline(xintercept = input$strike, colour="grey", linetype = "dashed")
  })
  
  output$greek_vega <- renderPlot({
    ggplot(option_greek_p(), aes(x=Spot, y=vega)) + 
      geom_point() +
      geom_vline(xintercept = input$strike, colour="grey", linetype = "dashed")
  })
  
  output$greek_theta <- renderPlot({
    ggplot(option_greek_p(), aes(x=Spot, y=theta)) + 
      geom_point() +
      geom_vline(xintercept = input$strike, colour="grey", linetype = "dashed")
  })
  
  output$greek_rho <- renderPlot({
    ggplot(option_greek_p(), aes(x=Spot, y=rho)) + 
      geom_point() +
      geom_vline(xintercept = input$strike, colour="grey", linetype = "dashed")
  })
  
  output$comb <- renderPlot({
    if (nrow(dynamic_df$a) == 0){
      ggplot() + geom_line(aes(x = seq(1, 200, 1), y = 0, colour = "Net Payoff")) +
        theme_bw() + 
        xlab("Underlying Asset Price") + ylab("Payoff")
    }else{
      dynamic_p$a  + 
        geom_hline(yintercept = 0, col = "grey") +
        theme_bw() +
        geom_line(aes(x=Spot, y=Price,
                      colour = "Net Payoff"), linetype = 2, data = dynamic_pf$a) +
        xlab("Underlying Asset Price") + ylab("Payoff") + labs(colour = 'Colour')
    }
  })
  
}

shinyApp(ui, server)
