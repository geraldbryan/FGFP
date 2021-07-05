#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinythemes)
library(shiny)
library(tidyverse)
library(tidyquant)
library(highcharter)
library(ggplot2)
library(rccdates)
library(plotly)
library(PerformanceAnalytics)
library(GGally)
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
library(GGally)

# Define UI for application that draws a histogram 
ui <- navbarPage( "Fresh Graduate Financial Planner",position=c("static-top"),collapsible = TRUE,inverse = TRUE, theme= shinytheme("lumen"),
                  tabPanel(title = "Home",
                           h1("Welcome to Fresh Graduate Financial Planner",align = "center", style="color:darkblue"),
                 column(width = 4, align= "center",offset =4,
                        textInput("name", "Enter your Name"),
                        imageOutput("image")
                 )
               ),
                  
              tabPanel("Financial Summary",sidebarLayout(
                            sidebarPanel(sliderInput("age", "Select your Age", min = 17, max = 30, value=22),
                                         sliderInput("saving", "Number of savings per months",min = 100000, max = 20000000, value = 2000000, step = 100000),
                                         numericInput("married", "Age to married",value = 28),
                                         numericInput("married_cost", "Expectation Married Cost", value = 200000000),
                                         numericInput("house", "Age to Buy a House", value = 26),
                                         numericInput("house_price", "Expectation Down Payment (min 10% of House prices, according to BI)", value=150000000)),
                                         mainPanel(tabsetPanel(tabPanel("Data Input",p(strong(h3(textOutput("title")))),
                                                   tableOutput("table_summary")),
                                                  tabPanel("Cost after Inflation Summary", h2(p(strong("Inflation Rate"))),
                                                  plotOutput("inflation"),tags$small(p(strong("Data inflasi pada bulan Desember tahun 2010-2019, Courtesy:www.BI.go.id"))),
                                                  h4(textOutput("totinfcost"))))
                ))),
      
              navbarMenu("Investment Method",
                  tabPanel("Deposits",
                           tabsetPanel(tabPanel("Deposits Interest Rate",h2("Data bunga deposito Bank per Agustus 2020",style="color:darkblue"),
                                       plotOutput("plot_deposit"), tags$small(p(strong("Data diambil pada tanggal 14 Agustus 2020, Courtesy: Pusat Informasi Pasar Uang")))),
                           navbarMenu("Return",
                                      tabPanel("Table",br(),tableOutput("deposit_return")),
                                      tabPanel("Chart", 
                                               splitLayout(plotlyOutput("bca_rtr"),plotlyOutput("mandiri_rtr")),
                                               splitLayout(plotlyOutput("niaga_rtr"),plotlyOutput("bri_rtr")),
                                               splitLayout(plotlyOutput("bni_rtr"),""))))),
                  tabPanel("Stocks", 
                           tabsetPanel(
                             tabPanel("Price",h2("Data Harga index IHSG dan S&P 500", style="color:darkblue"),
                           highchartOutput("plot1"),tags$small("Courtesy: Yahoo Finance")),
                           tabPanel("Return Rate", h2("Return Index"),
                                    plotOutput("idx_plot"),plotOutput("snp_plot")),
                           tabPanel("Return",br(),tableOutput("stock_return"),
                                    splitLayout(plotlyOutput("idx_rtr"),plotlyOutput("snp_rtr"))))),
                  tabPanel("Gold",
                           tabsetPanel(
                              tabPanel("Price",h2("Gold Price in USD (per Ounce)", style="color:darkblue"),
                              plotlyOutput("plot3")), 
                              tabPanel("Return", br(),
                                       tableOutput("gold_return"),br(),
                                    plotlyOutput("gld_rtr")
                                      )
                                      )
                           ),
                  tabPanel("Return Summary", tableOutput("sumtab"))
               ),
              tabPanel("Portfolio Summary",
                       selectInput("Risk",h3("Define your Willingness to Take Risk"), choices = c("Low", "Medium","High"),width = "100%"),
                       tabsetPanel(
                         navbarMenu("Optimal Portfolio",
                                    tabPanel("Portfolio Return Performance", plotOutput("perfchart")),
                                    tabPanel("Portfolio Correlation",plotOutput("cor")),
                                    tabPanel("Portfolio Return Summary",br(), tableOutput("portosum"),
                                             splitLayout(plotlyOutput("low_rtr"),plotlyOutput("med_rtr")),
                                             splitLayout(plotlyOutput("high_rtr")))
                                  ),
                         tabPanel("Investment Plan Summary",br(), tableOutput("tab")),
                         tabPanel("Simulation")
                       )
                       ),
  
              navbarMenu("More",
                   tabPanel("Investment Method Definition",
                            includeMarkdown("invest method.Rmd"),tags$small("Courtesy: www.investopedia.com")),
                   tabPanel("About",
                            includeMarkdown("about.Rmd"))
                   )
  )



server <- function(input, output,session) {
  # Home page
  output$image <- renderImage({list(src = "financial planner.png",
                              contentType = "image/png",width=435, height=375,
                              alt = "Face")
  })
  
  #Menu Input Data
  output$title <- renderText({paste(input$name,"'s", "Financial Plan Summary")})
  
  Data <- c("Now","Buy a house","Married", "Total Cost", "Savings per Months" )
  Age <- reactive({c(input$age,input$house,input$married," ","")})
  Value <- reactive({c("",input$house_price,input$married_cost,input$house_price+input$married_cost,input$saving)})
  Year_to_Accomplish <- reactive({c("",input$house-input$age,input$married-input$age,"","")})
  tablesum <-reactive({data.frame(Data,Age(),Value(),Year_to_Accomplish())})
  output$table_summary <- renderTable(tablesum(), width= "100%",bordered=T, align="c")
  
  # Ivestment Method page
  Tahun <- c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
  Tingkat_inflasi <- c(0.0696,0.0379,0.043,0.0838,0.0836,0.0335,0.0302,0.0361,0.0313,0.0272)
  inflasi <- data.frame(Tahun,Tingkat_inflasi)
  output$inflation <- renderPlot({ggplot(inflasi, aes(x=Tahun,y=Tingkat_inflasi),)+geom_col()+geom_line(aes(col="red"),show.legend = F)+scale_x_continuous(breaks=c(2010:2019))})
  
  mean_inflation <- mean(inflasi$Tingkat_inflasi)
  years <-reactive(if(input$married>= input$house){input$married-input$age}else{input$house-input$age})
  output$totinfcost <- renderText({paste("Total Biaya setelah inflasi",(input$married_cost+input$house_price)*((1+mean_inflation)^years()), "dengan tingkat inflasi sebesar", mean_inflation*100,"% per tahun")}) 
    

  #deposits
Bank <- c("BCA","Mandiri","CIMB Niaga", "BRI", "BNI")
Bank <- as.factor(Bank)
One_month_interest <- c(0.003,0.00211,0.00354,0.0039,0.00396)
deposit <- data.frame(Bank,One_month_interest)
output$plot_deposit <- renderPlot({ggplot(deposit,aes(x=Bank,y=One_month_interest))+geom_point(aes(size=5,col=Bank))+theme(legend.position = "none")})

BCA <- reactive(input$saving*((((1+deposit[1,2])^(years()*12))-1)/(deposit[1,2]/(1+deposit[1,2]))))
Mandiri <- reactive(input$saving*((((1+deposit[2,2])^(years()*12))-1)/(deposit[2,2]/(1+deposit[2,2]))))
Niaga <- reactive(input$saving*((((1+deposit[3,2])^(years()*12))-1)/(deposit[3,2]/(1+deposit[3,2]))))
BRI <- reactive(input$saving*((((1+deposit[4,2])^(years()*12))-1)/(deposit[4,2]/(1+deposit[4,2]))))
BNI <- reactive(input$saving*((((1+deposit[5,2])^(years()*12))-1)/(deposit[5,2]/(1+deposit[5,2]))))
return_deposit <- reactive(c(BCA(),Mandiri(),Niaga(),BRI(),BNI()))

bca <- reactive(BCA()- ((input$married_cost+input$house_price)*((1+mean_inflation)^years())))
bcafull <- reactive(if(bca()>=0){0}else{bca()})
mandiri <- reactive(Mandiri()-((input$married_cost+input$house_price)*((1+mean_inflation)^years())))
mandirifull <- reactive(if (mandiri()>=0){0}else{mandiri()})
niaga <- reactive(Niaga()-((input$married_cost+input$house_price)*((1+mean_inflation)^years())))
niagafull <- reactive(if(niaga() >=0){0}else{niaga()})
bri <- reactive(BRI()-((input$married_cost+input$house_price)*((1+mean_inflation)^years())))
brifull <- reactive(if(bri() >=0){0}else{bri()})
bni <- reactive(BNI()-((input$married_cost+input$house_price)*((1+mean_inflation)^years())))
bnifull <- reactive(if(bni() >=0){0}else{bni()})
deposit_money_need <- reactive(c(bcafull(),mandirifull(),niagafull(),brifull(),bnifull()))

dep <- reactive(data.frame(Bank,One_month_interest,return_deposit(),deposit_money_need()))
output$deposit_return <- renderTable(dep(),width= "100%",bordered=T, align="c")

fig3 <- reactive(plot_ly(
  type = "indicator",
  mode = "gauge+number",
  value = ((BCA()/((input$married_cost+input$house_price)*((1+mean_inflation)^years())))*100),
  title = list(text = "Return Accomplishment of BCA Deposit", font = list(size = 18)),
  gauge = list(
    axis = list(range = list(NULL, 100), tickwidth = 0.5, tickcolor = "darkblue"),
    bar = list(color = "black"),
    bgcolor = "white",
    borderwidth = 1,
    bordercolor = "black",
    steps = list(
      list(range = c(0, 50), color = "red"),
      list(range = c(50, 90), color = "yellow"),
      list(range = c(90,100), color = "green")),
    threshold = list(
      line = list(color = "blue", width = 4),
      thickness = 0.75,
      value = 100)))) 
output$bca_rtr <- renderPlotly(fig3())

fig4 <- reactive(plot_ly(
  type = "indicator",
  mode = "gauge+number",
  value = ((Mandiri()/((input$married_cost+input$house_price)*((1+mean_inflation)^years())))*100),
  title = list(text = "Return Accomplishment of Mandiri Deposit", font = list(size = 18)),
  gauge = list(
    axis = list(range = list(NULL, 100), tickwidth = 0.5, tickcolor = "darkblue"),
    bar = list(color = "black"),
    bgcolor = "white",
    borderwidth = 1,
    bordercolor = "black",
    steps = list(
      list(range = c(0, 50), color = "red"),
      list(range = c(50, 90), color = "yellow"),
      list(range = c(90,100), color = "green")),
    threshold = list(
      line = list(color = "blue", width = 4),
      thickness = 0.75,
      value = 100)))) 
output$mandiri_rtr <- renderPlotly(fig4())

fig5 <- reactive(plot_ly(
  type = "indicator",
  mode = "gauge+number",
  value = ((Niaga()/((input$married_cost+input$house_price)*((1+mean_inflation)^years())))*100),
  title = list(text = "Return Accomplishment of Niaga Deposit", font = list(size = 18)),
  gauge = list(
    axis = list(range = list(NULL, 100), tickwidth = 0.5, tickcolor = "darkblue"),
    bar = list(color = "black"),
    bgcolor = "white",
    borderwidth = 1,
    bordercolor = "black",
    steps = list(
      list(range = c(0, 50), color = "red"),
      list(range = c(50, 90), color = "yellow"),
      list(range = c(90,100), color = "green")),
    threshold = list(
      line = list(color = "blue", width = 4),
      thickness = 0.75,
      value = 100)))) 
output$niaga_rtr <- renderPlotly(fig5())

fig6 <- reactive(plot_ly(
  type = "indicator",
  mode = "gauge+number",
  value = ((BRI()/((input$married_cost+input$house_price)*((1+mean_inflation)^years())))*100),
  title = list(text = "Return Accomplishment of BRI Deposit", font = list(size = 18)),
  gauge = list(
    axis = list(range = list(NULL, 100), tickwidth = 0.5, tickcolor = "darkblue"),
    bar = list(color = "black"),
    bgcolor = "white",
    borderwidth = 1,
    bordercolor = "black",
    steps = list(
      list(range = c(0, 50), color = "red"),
      list(range = c(50, 90), color = "yellow"),
      list(range = c(90,100), color = "green")),
    threshold = list(
      line = list(color = "blue", width = 4),
      thickness = 0.75,
      value = 100)))) 
output$bri_rtr <- renderPlotly(fig6())

fig7 <- reactive(plot_ly(
  type = "indicator",
  mode = "gauge+number",
  value = ((BNI()/((input$married_cost+input$house_price)*((1+mean_inflation)^years())))*100),
  title = list(text = "Return Accomplishment of BNI Deposit", font = list(size = 18)),
  gauge = list(
    axis = list(range = list(NULL, 100), tickwidth = 0.5, tickcolor = "darkblue"),
    bar = list(color = "black"),
    bgcolor = "white",
    borderwidth = 1,
    bordercolor = "black",
    steps = list(
      list(range = c(0, 50), color = "red"),
      list(range = c(50, 90), color = "yellow"),
      list(range = c(90,100), color = "green")),
    threshold = list(
      line = list(color = "blue", width = 4),
      thickness = 0.75,
      value = 100)))) 
output$bni_rtr <- renderPlotly(fig7())

#stocks
idx_prices <- getSymbols("^JKSE", auto.assign = FALSE, from="2010-12-01", to="2020-08-31") %>% na.omit()
snp500_prices <- getSymbols("^GSPC", auto.assign = FALSE, from="2010-12-01", to="2020-08-31") %>% na.omit()
output$plot1 <- renderHighchart({highchart(type = "stock") %>% hc_add_series(idx_prices) %>% hc_add_series(snp500_prices)}) 

stocks <- c("IHSG","S&P 500")
stocks <- as.factor(stocks)

snp500_monthly <-to.monthly(snp500_prices)
snp500_monthly <- Ad(snp500_monthly)
idx_monthly <- to.monthly(idx_prices)
idx_monthly <- Ad(idx_monthly)

snp500_return <- Return.calculate(snp500_monthly)
snp500_return <- snp500_return[-1,]
idx_return <- Return.calculate(idx_monthly)
idx_return <- idx_return[-1,]

rate_idx <- mean(idx_return)
rate_snp500 <- mean(snp500_return)
rate <- c(rate_idx,rate_snp500)

SNP500 <- reactive(input$saving*((((1+rate_snp500)^(years()*12))-1)/(rate_snp500/(1+rate_snp500))))
IDX <- reactive(input$saving*((((1+rate_idx)^(years()*12))-1)/(rate_idx/(1+rate_idx))))
return_stocks <- reactive(c(IDX(),SNP500()))

idx<- reactive(IDX()- ((input$married_cost+input$house_price)*((1+mean_inflation)^years())))
idxfull <- reactive(if(idx()>=0){0}else{idx()})
snp500<- reactive(SNP500()- ((input$married_cost+input$house_price)*((1+mean_inflation)^years())))
snp500full <- reactive(if(snp500()>=0){0}else{snp500()})
stocks_money_need <- reactive(c(idxfull(),snp500full()))
stck <- reactive(data.frame(stocks,rate,return_stocks(),stocks_money_need()))
output$stock_return <- renderTable(stck(),width= "100%",bordered=T, align="c")
output$idx_plot <- renderPlot({plot(idx_return)})
output$snp_plot <- renderPlot({plot(snp500_return)})

fig1 <- reactive(plot_ly(
  type = "indicator",
  mode = "gauge+number",
  value = ((IDX()/((input$married_cost+input$house_price)*((1+mean_inflation)^years())))*100),
  title = list(text = "Return Accomplishment of IDX", font = list(size = 18)),
  gauge = list(
    axis = list(range = list(NULL, 100), tickwidth = 0.5, tickcolor = "darkblue"),
    bar = list(color = "black"),
    bgcolor = "white",
    borderwidth = 1,
    bordercolor = "black",
    steps = list(
      list(range = c(0, 50), color = "red"),
      list(range = c(50, 90), color = "yellow"),
      list(range = c(90,100), color = "green")),
    threshold = list(
      line = list(color = "blue", width = 4),
      thickness = 0.75,
      value = 100)))) 
output$idx_rtr <- renderPlotly(fig1())

fig2 <- reactive(plot_ly(
  type = "indicator",
  mode = "gauge+number",
  value = ((SNP500()/((input$married_cost+input$house_price)*((1+mean_inflation)^years())))*100),
  title = list(text = "Return Accomplishment of S&P 500", font = list(size = 18)),
  gauge = list(
    axis = list(range = list(NULL, 100), tickwidth = 0.5, tickcolor = "darkblue"),
    bar = list(color = "black"),
    bgcolor = "white",
    borderwidth = 1,
    bordercolor = "black",
    steps = list(
      list(range = c(0, 50), color = "red"),
      list(range = c(50, 90), color = "yellow"),
      list(range = c(90,100), color = "green")),
    threshold = list(
      line = list(color = "blue", width = 4),
      thickness = 0.75,
      value = 100)))) 
output$snp_rtr <- renderPlotly(fig2())

#gold
gold_prices <- tq_get("GC=F",get = "stock.prices",from="2010-12-01") %>% na.omit()
plot <- ggplot(gold_prices,aes(x=date,y=close))+geom_line(col="goldenrod")
output$plot3 <-renderPlotly({ggplotly(plot)})

gold <- "Gold"

gold_price <- getSymbols("GC=F",auto.assign = FALSE,from="2010-12-01", to="2020-08-31")
gold_price <- na.omit(gold_price)
gold_monthly <- to.monthly(gold_price)
gold_monthly <- Cl(gold_monthly)

gold_return <- Return.calculate(gold_monthly)
gold_return <- gold_return[-1,]
rate_gold <- mean(gold_return)

GOLD <- reactive(input$saving*((((1+rate_gold)^(years()*12))-1)/(rate_gold/(1+rate_gold))))
return_gold <- reactive(GOLD())

golds<- reactive(GOLD()- ((input$married_cost+input$house_price)*((1+mean_inflation)^years())))
goldfull <- reactive(if(golds()>=0){0}else{golds()})
gold_money_need <- reactive(goldfull())

gld <- reactive(data.frame(gold,rate_gold,return_gold(),gold_money_need()))

output$gold_return <- renderTable(gld(),width= "100%",bordered=T, align="c")


fig <- reactive(plot_ly(
  type = "indicator",
  mode = "gauge+number",
  value = ((return_gold()/((input$married_cost+input$house_price)*((1+mean_inflation)^years())))*100),
  title = list(text = "Return Accomplishment of Gold", font = list(size = 14)),
  gauge = list(
    axis = list(range = list(NULL, 100), tickwidth = 0.5, tickcolor = "darkblue"),
    bar = list(color = "black"),
    bgcolor = "white",
    borderwidth = 1,
    bordercolor = "black",
    steps = list(
      list(range = c(0, 50), color = "red"),
      list(range = c(50, 90), color = "yellow"),
      list(range = c(90,100), color = "green")),
    threshold = list(
      line = list(color = "blue", width = 4),
      thickness = 0.75,
      value = 100)))) 
output$gld_rtr <- renderPlotly(fig())

# Return Summary
  invest_name <- c("BCA","Mandiri","CIMB Niaga", "BRI", "BNI", "IHSG","S&P 500", "Gold")
  category <- c("Deposits","Deposits","Deposits","Deposits","Deposits","Stocks","Stocks", "Gold")
  money_need <-  reactive(c(bcafull(),mandirifull(),niagafull(),brifull(),bnifull(),idxfull(),snp500full(),goldfull()))
  summ <- reactive(data.frame(invest_name,category,money_need()))
  output$sumtab <- renderTable(summ(),width= "100%",bordered=T, align="c")


# Optimal Portfolio

port <- cbind(idx_return,snp500_return,gold_return)
port

output$cor <- renderPlot(ggcorr(port, label = T))

## Low risk portfolio
port_spec <- portfolio.spec(colnames(port))
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")
port_spec <- add.constraint(portfolio = port_spec, type = "long_only")
port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")
opt1 <- optimize.portfolio(port, portfolio = port_spec, optimize_method = "ROI")
opt1

## High Return Portfolio
port_spec <- portfolio.spec(colnames(port))
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")
port_spec <- add.constraint(portfolio = port_spec, type = "long_only")
port_spec <- add.objective(portfolio = port_spec, type = "return", name = "mean")
opt2 <- optimize.portfolio(port, portfolio = port_spec, optimize_method = "ROI")
opt2

## Optimization Portfolio
port_spec <- portfolio.spec(colnames(port))
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")
port_spec <- add.constraint(portfolio = port_spec, type = "long_only")
port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev",risk_aversion = 10)
port_spec <- add.objective(portfolio = port_spec, type = "return", name = "mean")
opt3 <- optimize.portfolio(port, portfolio = port_spec, optimize_method = "ROI")
opt3

returns_base <- Return.portfolio(R = port)
colnames(returns_base) <- "Without Optimization/ Equal Weight"
returns_1 <- Return.portfolio(R = port, weights = extractWeights(opt1))
colnames(returns_1) <- "Low Risk"
returns_2 <- Return.portfolio(R = port, weights = extractWeights(opt2))
colnames(returns_2) <- "High Rerturn"
returns_3 <- Return.portfolio(R = port, weights = extractWeights(opt3))
colnames(returns_3) <- "Optimize"

ret_port <- cbind(returns_base,returns_1,returns_2,returns_3)
output$perfchart <- renderPlot(charts.PerformanceSummary(ret_port, main = "Portfolio Return Chart"))

ret_low <- (mean(returns_1)*0.3)+(0.7*deposit[5,2])
ret_high <- (mean(returns_2)*0.7) + (0.3*deposit[5,2])
ret_medium <- (mean(returns_3)*0.5) + (0.5*deposit[5,2])


## Investment Plan Summary

Investment_method <- c("Deposits (BNI)","IHSG","S&P 500","Gold")
Proportion_low <- c(0.8, 0.2*0.25, 0.2*0.43, 0.2*0.32)
Proportion_medium <- c(0.5, 0.5*0.0758, 0.5*0.6426,0.5*0.2816)
Proportion_high <- c(0.2, 0,0.8*1,0)

Budget_Allocation_low <- reactive(c(input$saving*Proportion_low[1],input$saving*Proportion_low[2],input$saving*Proportion_low[3],input$saving*Proportion_low[4]))
Budget_Allocation_medium <- reactive(c(input$saving*Proportion_medium[1],input$saving*Proportion_medium[2],input$saving*Proportion_medium[3],input$saving*Proportion_medium[4]))
Budget_Allocation_high <- reactive(c(input$saving*Proportion_high[1],input$saving*Proportion_high[2],input$saving*Proportion_high[3],input$saving*Proportion_high[4]))

low <- reactive(data.frame(Investment_method,Proportion_low, Budget_Allocation_low()))
medium <- reactive(data.frame(Investment_method,Proportion_medium,Budget_Allocation_medium()))
high <- reactive(data.frame(Investment_method,Proportion_high,Budget_Allocation_high()))
                
risk <- reactive(if(input$Risk == "Low"){low()}
                 else if(input$Risk == "Medium"){medium()}
                 else {high()})

output$tab <- renderTable(risk(),width= "100%",bordered=T, align="c")

rate_low <- reactive(input$saving*((((1+ret_low)^(years()*12))-1)/(ret_low/(1+ret_low))))
return_low_risk <- reactive(rate_low())
rate_lows<- reactive(rate_low()- ((input$married_cost+input$house_price)*((1+mean_inflation)^years())))
rate_low_full <- reactive(if(rate_lows()>=0){0}else{rate_lows()})
low_risk_money_need <- reactive(rate_low_full())

rate_medium <- reactive(input$saving*((((1+ret_medium)^(years()*12))-1)/(ret_medium/(1+ret_medium))))
return_medium_risk <- reactive(rate_medium())
rate_mediums<- reactive(rate_medium()- ((input$married_cost+input$house_price)*((1+mean_inflation)^years())))
rate_medium_full <- reactive(if(rate_mediums()>=0){0}else{rate_mediums()})
medium_risk_money_need <- reactive(rate_medium_full())

rate_high <- reactive(input$saving*((((1+ret_high)^(years()*12))-1)/(ret_high/(1+ret_high))))
return_high_risk <- reactive(rate_high())
rate_highs<- reactive(rate_high()- ((input$married_cost+input$house_price)*((1+mean_inflation)^years())))
rate_high_full <- reactive(if(rate_highs()>=0){0}else{rate_highs()})
high_risk_money_need <- reactive(rate_high_full())

Portfolio <- c("Low Risk Portfolio","Optimize Portfolio","High Return Portfolio")
Portfolio_Return <- c(ret_low,ret_medium,ret_high)
Portfolio_Money_Return <- reactive(c(return_low_risk(),return_medium_risk(),return_high_risk()))
Portfolio_Money_Need <- reactive(c(low_risk_money_need(),medium_risk_money_need(),high_risk_money_need()))

Porto <- reactive(data.frame(Portfolio,Portfolio_Return,Portfolio_Money_Return(),Portfolio_Money_Need()))
output$portosum <- renderTable(Porto(),width= "100%",bordered=T, align="c")

fig8 <- reactive(plot_ly(
  type = "indicator",
  mode = "gauge+number",
  value = ((rate_low()/((input$married_cost+input$house_price)*((1+mean_inflation)^years())))*100),
  title = list(text = "Return Accomplishment of Low Risk Portfolio", font = list(size = 18)),
  gauge = list(
    axis = list(range = list(NULL, 100), tickwidth = 0.5, tickcolor = "darkblue"),
    bar = list(color = "black"),
    bgcolor = "white",
    borderwidth = 1,
    bordercolor = "black",
    steps = list(
      list(range = c(0, 50), color = "red"),
      list(range = c(50, 90), color = "yellow"),
      list(range = c(90,100), color = "green")),
    threshold = list(
      line = list(color = "blue", width = 4),
      thickness = 0.75,
      value = 100)))) 
output$low_rtr <- renderPlotly(fig8())

fig9 <- reactive(plot_ly(
  type = "indicator",
  mode = "gauge+number",
  value = ((rate_medium()/((input$married_cost+input$house_price)*((1+mean_inflation)^years())))*100),
  title = list(text = "Return Accomplishment of Optimization Portfolio", font = list(size = 18)),
  gauge = list(
    axis = list(range = list(NULL, 100), tickwidth = 0.5, tickcolor = "darkblue"),
    bar = list(color = "black"),
    bgcolor = "white",
    borderwidth = 1,
    bordercolor = "black",
    steps = list(
      list(range = c(0, 50), color = "red"),
      list(range = c(50, 90), color = "yellow"),
      list(range = c(90,100), color = "green")),
    threshold = list(
      line = list(color = "blue", width = 4),
      thickness = 0.75,
      value = 100)))) 
output$med_rtr <- renderPlotly(fig9())

fig10 <- reactive(plot_ly(
  type = "indicator",
  mode = "gauge+number",
  value = ((rate_high()/((input$married_cost+input$house_price)*((1+mean_inflation)^years())))*100),
  title = list(text = "Return Accomplishment of High Return Portfolio", font = list(size = 16)),
  gauge = list(
    axis = list(range = list(NULL, 100), tickwidth = 0.5, tickcolor = "darkblue"),
    bar = list(color = "black"),
    bgcolor = "white",
    borderwidth = 1,
    bordercolor = "black",
    steps = list(
      list(range = c(0, 50), color = "red"),
      list(range = c(50, 90), color = "yellow"),
      list(range = c(90,100), color = "green")),
    threshold = list(
      line = list(color = "blue", width = 4),
      thickness = 0.75,
      value = 100)))) 
output$high_rtr <- renderPlotly(fig10())

}

shinyApp(ui = ui, server = server)

