# BDA400 Assignment 6 — Portfolio Shiny Dashboard
# Student: Sabouh Mohamed
# Setup: install.packages(c("shiny","ggplot2","quantmod","TTR","xts","zoo"))
library(shiny)
library(ggplot2)
library(quantmod)

symbol_ok <- function(x) {
  x <- toupper(trimws(x))
  if (!grepl("^[A-Z0-9.^_-]{1,15}$",x)) stop("Invalid symbol")
  x
}
make_bars <- function(x, frame) {
  if(frame=="Weekly") return(xts::to.weekly(x,indexAt="endof",drop.time=TRUE,name=NULL))
  if(frame=="Monthly") return(xts::to.monthly(x,indexAt="endof",drop.time=TRUE,name=NULL))
  x
}
analyze <- function(x,short,long,rsi_n) {
  cl <- as.numeric(Cl(x)); n <- length(cl)
  d <- data.frame(Date=as.Date(zoo::index(x)),Open=as.numeric(Op(x)),
      High=as.numeric(Hi(x)),Low=as.numeric(Lo(x)),Close=cl,Volume=as.numeric(Vo(x)))
  d$Fast <- if(n>=short) as.numeric(TTR::SMA(cl,n=short)) else rep(NA_real_,n)
  d$Slow <- if(n>=long) as.numeric(TTR::SMA(cl,n=long)) else rep(NA_real_,n)
  d$RSI <- if(n>rsi_n) as.numeric(TTR::RSI(cl,n=rsi_n)) else rep(NA_real_,n)
  if(n>=35) {
    m <- TTR::MACD(cl,nFast=12,nSlow=26,nSig=9,percent=FALSE)
    d$MACD <- as.numeric(m[,1]); d$MACD_Signal <- as.numeric(m[,2])
  } else { d$MACD <- rep(NA_real_,n); d$MACD_Signal <- rep(NA_real_,n) }
  d$Histogram <- d$MACD-d$MACD_Signal
  d$Signal <- rep("Hold",n)
  if(n>1) for(i in 2:n) {
    a <- c(d$Fast[i-1],d$Slow[i-1],d$Fast[i],d$Slow[i])
    if(all(is.finite(a))) {
      if(a[1]<=a[2] && a[3]>a[4]) d$Signal[i] <- "Buy"
      if(a[1]>=a[2] && a[3]<a[4]) d$Signal[i] <- "Sell"
    }
  }
  d
}
ui <- fluidPage(
  titlePanel("BDA400 | Portfolio Technical Analysis"),
  p("Yahoo Finance • SMA, RSI, MACD • Buy/Sell crossover signals"),
  sidebarLayout(sidebarPanel(
    selectInput("stock","Portfolio stock",c("Apple"="AAPL","Microsoft"="MSFT",
      "Alphabet"="GOOG","NVIDIA"="NVDA","Tesla"="TSLA")),
    textInput("custom","Or enter a ticker (leave blank for selection)",""),
    dateRangeInput("dates","Date range",start=Sys.Date()-730,end=Sys.Date(),max=Sys.Date()),
    selectInput("frame","Time frame",c("Daily","Weekly","Monthly")),
    selectInput("style","Price chart",c("Line","Area","Candlestick")),
    checkboxGroupInput("ind","Indicator overlays",c("Moving averages","RSI","MACD"),
       selected=c("Moving averages","RSI","MACD")),
    checkboxInput("signals","Show Buy/Sell labels",TRUE),
    sliderInput("fast","Short SMA period",3,40,20),
    sliderInput("slow","Long SMA period",15,150,50),
    numericInput("rsi_n","RSI period",14,min=2,max=100),
    actionButton("reload","Refresh Yahoo Finance")
  ),mainPanel(
    h4(textOutput("summary")),
    plotOutput("price",height="400px"),
    conditionalPanel("input.ind.includes('RSI')",plotOutput("rsi_plot",height="190px")),
    conditionalPanel("input.ind.includes('MACD')",plotOutput("macd_plot",height="220px")),
    h4("Recent observations"),tableOutput("recent"),
    downloadButton("export","Export analysis as CSV"),
    p("Educational demonstration only; signals are not investment advice.")
  ))
)
server <- function(input,output,session) {
  ticker <- reactive(symbol_ok(if(nzchar(trimws(input$custom))) input$custom else input$stock))
  prices <- reactive({
    input$reload
    validate(need(!is.null(input$dates)&&length(input$dates)==2,"Select dates"))
    validate(need(input$dates[1]<input$dates[2],"Start must precede end"))
    tryCatch(getSymbols(ticker(),src="yahoo",from=input$dates[1],
          to=as.Date(input$dates[2])+1,auto.assign=FALSE),
      error=function(e) validate(need(FALSE,paste("Yahoo Finance unavailable:",conditionMessage(e)))))
  })
  dataset <- reactive({
    validate(need(input$fast<input$slow,"Short SMA must be less than long SMA"))
    x <- make_bars(prices(),input$frame)
    validate(need(NROW(x)>1,"Insufficient historical bars: expand date range"))
    analyze(x,input$fast,input$slow,input$rsi_n)
  })
  output$summary <- renderText({
    d <- dataset()
    paste(ticker(),":",nrow(d),"bars; last close $",round(tail(d$Close,1),2),
      "; last-bar signal:",tail(d$Signal,1))
  })
  output$price <- renderPlot({
    d <- dataset(); p <- ggplot(d,aes(x=Date))
    if(input$style=="Line") p <- p+geom_line(aes(y=Close),color="steelblue",linewidth=.8)
    if(input$style=="Area") p <- p+geom_area(aes(y=Close),fill="lightblue",alpha=.6)+
        geom_line(aes(y=Close),color="steelblue")
    if(input$style=="Candlestick") {
      d$Direction <- ifelse(d$Close>=d$Open,"Up","Down")
      p <- ggplot(d,aes(x=Date))+
          geom_linerange(aes(ymin=Low,ymax=High,color=Direction))+
          geom_segment(aes(xend=Date,y=Open,yend=Close,color=Direction),linewidth=2.3)+
          scale_color_manual(values=c("Up"="forestgreen","Down"="firebrick"))
    }
    if("Moving averages" %in% input$ind) p <- p+
       geom_line(aes(y=Fast),color="orange",linewidth=.8,na.rm=TRUE)+
       geom_line(aes(y=Slow),color="purple",linewidth=.8,na.rm=TRUE)
    if(isTRUE(input$signals)) {
      marks <- d[d$Signal!="Hold",,drop=FALSE]
      if(nrow(marks)>0) p <- p+
        geom_point(data=marks,aes(y=Close,color=Signal),size=2.5,shape=17)+
        geom_text(data=marks,aes(y=Close,label=Signal),vjust=-.8,size=3)
    }
    p+labs(title=paste(ticker(),input$frame,input$style),
      subtitle="Orange: short SMA • Purple: long SMA",x=NULL,y="Price (USD)")+
      theme_minimal()
  })
  output$rsi_plot <- renderPlot({
    d <- dataset()
    ggplot(d,aes(Date,RSI))+geom_hline(yintercept=c(30,70),linetype="dashed")+
      geom_line(color="purple",na.rm=TRUE)+coord_cartesian(ylim=c(0,100))+
      labs(title="RSI",x=NULL,y="Index")+theme_minimal()
  })
  output$macd_plot <- renderPlot({
    d <- dataset()
    ggplot(d,aes(Date))+geom_col(aes(y=Histogram),fill="lightblue",na.rm=TRUE)+
       geom_line(aes(y=MACD),color="steelblue",na.rm=TRUE)+
       geom_line(aes(y=MACD_Signal),color="orange",na.rm=TRUE)+
       labs(title="MACD (12, 26, 9)",x=NULL,y="MACD")+theme_minimal()
  })
  output$recent <- renderTable({
    tail(dataset()[,c("Date","Close","Fast","Slow","RSI","Signal")],10)
  },digits=2)
  output$export <- downloadHandler(
    filename=function() paste0(ticker(),"_analysis.csv"),
    content=function(file) write.csv(dataset(),file,row.names=FALSE))
}
shinyApp(ui,server)
