
library(shiny)
library(plotly)
library(leaflet)
library(tidyverse)
library(RColorBrewer)
library(lubridate) 
library(ggplot2)
library(dplyr)

options(scipen=5)

# Data of Zhendong
duration_histogram_data <-
  read.csv(
    "./Data/duration_histogram_data.csv"
  )
duration_boxplot_data <-
  read.csv(
    "./Data/duration_boxplot_data.csv"
  )
duration_injure_data <-
  read.csv(
    "./Data/duration_injure_data.csv"
  )
duration_mean_data <-
  read.csv(
    "./Data/duration_mean_data.csv"
  )
duration_magnitude_data <-
  read.csv(
    "./Data/duration_magnitude_data.csv"
  )
magnitude_histogram_data <-
  read.csv(
    "./Data/magnitude_histogram_data.csv"
  )
magnitude_mean_data <-
  read.csv(
    "./Data/magnitude_mean_data.csv"
  )
Tornado_damage_data <-
  read.csv(
    "./Data/Tornado_damage_data.csv"
  )
Tornado_month_data <-
  read.csv(
    "./Data/Tornado_month_data.csv"
  )
Tornado_state <-
  read.csv(
    "./Data/Tornado_state.csv"
  )
Tornado_timeseries <-
  read.csv(
    "./Data/Tornado_timeseries.csv"
  )
damage_time <-
  read.csv(
    "./Data/damage_time.csv"
  )



# Data of Inhenn

# Data of Xiaowei
data_1117=read.csv('./Data/2011_2017.csv')
new_data=data_1117 %>% group_by(YEAR,EVENT_TYPE) %>% summarize(Injuries_death=sum(INJURIES_DEATH))
new_data=new_data %>% spread(EVENT_TYPE,Injuries_death)
new_data=new_data[colSums(is.na(new_data))==0]
new_data$YEAR = NULL
new_matrix=data.matrix(new_data)
rownames(new_matrix)=c(2011,2012,2013,2014,2015,2016,2017)

# Data of EJ
data = read.csv("./Data/StormEvents_details-ftp_v1.0_d2017_c20181017.csv")
a=as.numeric(sub("K", "e3", data$DAMAGE_PROPERTY, fixed = TRUE))
b=as.numeric(sub("M", "e6", data$DAMAGE_PROPERTY, fixed = TRUE))
a[is.na(a)]=b[is.na(a)]
data$DAMAGE_PROPERTY=a
data$DAMAGE_PROPERTY[is.na(data$DAMAGE_PROPERTY)]=0

a=as.numeric(sub("K", "e3", data$DAMAGE_CROPS, fixed = TRUE))
b=as.numeric(sub("M", "e6", data$DAMAGE_CROPS, fixed = TRUE))
a[is.na(a)]=b[is.na(a)]
data$DAMAGE_CROPS=a
data$DAMAGE_CROPS[is.na(data$DAMAGE_CROPS)]=0
data = data[data$DAMAGE_PROPERTY!=0,]
data$code = state.abb[match(data$STATE,toupper(state.name))]
data = data[complete.cases(data$code),]
data$LOG_DAMAGE_PROPERTY = log10(data$DAMAGE_PROPERTY)
data$BEGIN_DATE_TIME = dmy_hms(data$BEGIN_DATE_TIME)

ag1 = aggregate(data$DAMAGE_PROPERTY,by=list(Category=data$EVENT_TYPE), FUN=sum)
df = data[data$TOR_F_SCALE!="",]
ag2 = aggregate(df$DAMAGE_PROPERTY,by=list(Category=df$TOR_F_SCALE), FUN=sum)
ag3 = aggregate(data$DAMAGE_PROPERTY,by=list(Category=data$MONTH_NAME), FUN=sum)
ag4 = aggregate(data$DAMAGE_PROPERTY,by=list(Category=data$STATE), FUN=sum)

# Define UI for application that draws a histogram
ui <- navbarPage(
  "2017 US Weather Events",
  id = "uwe",
  tabPanel(
    "Event Type",
    titlePanel("2017 US Storm Top 5 Event Type by State"),
    
    fixedRow(column(
      5,
      selectInput(
        "var_lyh01",
        "Choose a State",
        choices = data$STATE,
        selected = "Average Life Expectancy"
      )
    )),
    fixedRow(column(8, offset = 2, plotlyOutput("plotl1")) ),
    
    
    titlePanel("2017 US Storm Top 5 State by Event Type"),
    
    fixedRow(column(
      5,
      selectInput(
        "var_lyh02",
        "Choose a Event Type",
        choices = data$EVENT_TYPE,
        selected = "Average Life Expectancy"
      )
    )),
    fixedRow(column(8, offset = 2, plotlyOutput("plotl2")) )
    
  ),
  
  
  
  tabPanel(
    "Duration Analysis",
    titlePanel("Histogram"),
    # Histogram
    selectInput(
      inputId = "var_zhendong01",
      label = "Choose an event type to display",
      choices = unique(duration_histogram_data$EVENT_TYPE),
      selected = "Heavy Snow"
    ),
    
    fixedRow(column(8, offset = 2, plotlyOutput("plotw1"))),
    
    # Boxplot
    titlePanel("Boxplot"),
    selectInput(
      inputId = "var_zhendong02",
      label = "Choose a event class to display",
      choices = c("short duration","short-mid duration", "mid-long duration", "long duration"),
      selected = "short-mid duration"
    ),
    
    fixedRow(column(8, offset = 2, plotlyOutput("plotw2"))),
    
    # ClevelandDotPlot
    titlePanel("ClevelandDotPlot"),
    fixedRow(column(6, offset = 3, plotOutput("plotw3"))),
    
    
    # Duration with injuries
    titlePanel("Duration with Injuries"),
    selectInput(
      inputId = "var_zhendong03",
      label = "Choose an event type to display",
      choices = unique(duration_injure_data$EVENT_TYPE),
      selected = "Strong Wind"
    ),
    
    fixedRow(column(8, offset = 2, plotlyOutput("plotw4"))),
    
    
    fixedRow(column(
      12,
      helpText(
        "Relationship between duration with injuries and deaths of events."
      )
    ))
    
  ),
  
  tabPanel(
    "Magnitude Analysis",
    titlePanel("Histogram"),
    
    selectInput(
      inputId = "var_zhendong04",
      label = "Choose an event type to display",
      choices = unique(magnitude_histogram_data$EVENT_TYPE),
      selected = "Marine Thunderstorm Wind"
    ),
    fixedRow(column(8, offset = 2, plotlyOutput("plotw5"))),
    
    
    # ClevelandDotPlot
    titlePanel("ClevelandDotPlot"),
    fixedRow(column(6, offset = 3, plotOutput("plotw6"))),
    
    titlePanel("Magnitude with Duration"),
    
    fixedRow(column(
      10,
      selectInput(
        inputId = "var_zhendong05",
        label = "Choose an event type to display",
        choices = unique(duration_magnitude_data$EVENT_TYPE),
        selected = "Marine Thunderstorm Wind"
      )),
      fixedRow(column(8, offset = 2, plotlyOutput("plotw7")) )
    )
    
  ),
  
  tabPanel(
    "Casualty Analysis",
    titlePanel("Injuries and Death"),
    # Histogram
    
    fixedRow(column(8, selectInput("region", "Event type:", 
                    choices=colnames(new_matrix)),
                    hr(),
                    helpText("Death and Injuries caused by different event type from 2011-2017.")
    ),offset = 2, plotOutput("plotz1"))),

  
    
  
  tabPanel(
    "Event Topics",
    titlePanel("Tornado"),
    fixedRow(column(
      8,
      helpText(
        "Histogram"
      )
    )),
    
    fixedRow(column(
      8,
      offset = 2,
      plotlyOutput("plotw8")
    )),
    
    fixedRow(column(
      8,
      helpText(
        "Spatial Distribution"
      )
    )),
    
    fixedRow(column(
      8,offset = 2, plotlyOutput("plotw9")
    )),
    
    fixedRow(column(
      8,
      helpText(
        "Monthly Frequency of Tornado in US (2017)"
      )
    )),
    
    fixedRow(column(
      8,offset = 2, plotOutput("plotw10")
    )
    ),
    
    fixedRow(column(
      8,
      helpText(
        "Year trend of Tornado"
      )
    )),
    
    selectInput(
      inputId = "var_zhendong06",
      label = "Choose a variable to display",
      choices = c("Frequency", "Total Damage", "Mean Damage"),
      selected = "Frequency"
    ),
    
    fixedRow(column(
      8,offset = 2, plotlyOutput("plotw11")
    )
    )
  ),
  
  tabPanel(
    "Property Damage Analysis",
    titlePanel("US Storm Property Damage by Factors"),
    sidebarPanel(
      selectInput(
        "var_ej01",
        "Choose a variable",
        choices = c(
          "Damage by Event Type",
          "Damage by Tornado F-scale",
          "Damage by Month",
          "Damage by State"
        ),
        selected = "Damage by Event Type"
        
      ),
      hr(),
      helpText("2017 US Storm Property Damage Total by Factor in Clevelend Dot Plot")
    ),
    mainPanel(
      htmltools::div(style = "display:inline-block", plotlyOutput("plotc1", width = 600, height = 600))
    ),
    
    titlePanel("US Storm Property Damage by State"),
    sidebarPanel(
      
      selectInput(
        "var_ej02",
        "Choose a State",
        choices = data$STATE,
        selected = "Damage by State"
      ),
      
      hr(),
      helpText("2017 US Storm Property Damage Total by State Histogram with Bins=15")
    ),
    mainPanel(
      htmltools::div(style = "display:inline-block", plotlyOutput("plotc2", width = 600, height = 600))
    ),
    
    titlePanel("Histogram of Log Property Damage with Changing Bins"),
    mainPanel(
      htmltools::div(style = "display:inline-block", plotlyOutput("plotc3", width = 1000, height = 600)),
      wellPanel(
        style = "display:inline-block; vertical-align:top;", 
        sliderInput("xbins", "Number of x bins", 
                    min = 1, max = 50, value = 20, width = 250)
        
      ),
      br()
    )
    
    
    
  )
  
  
)




# Define server logic required to draw a histogram
server <- function(input,
                   output,
                   session) {
  # Output of Zhendong
  
  output$plotw1 <- renderPlotly({
    event = input$var_zhendong01
    data <- duration_histogram_data[duration_histogram_data$EVENT_TYPE == event,]
    #plot_ly(x = ~data$DURATION, type = "histogram", marker = list(color = 'orange')) %>%
      #layout(title="Histogram of Duration", xaxis=list(title="Duration"))
    h1 <- ggplot(data, aes(DURATION)) +
      geom_histogram(bins = 60, fill = "lightblue", color = "blue") +
      ggtitle("Histogram of Duration by Event type")+
      theme(plot.title = element_text(hjust = 0.5)) +
      ylab("Frequency")
    ggplotly(h1)
  })
  
  
  
  output$plotw2 <- renderPlotly({
    level = switch(input$var_zhendong02,
                   "short duration" = "d1",
                   "short-mid duration" = "d2",
                   "mid-long duration" = "d3",
                   "long duration" = "d4")
  
    data <- duration_boxplot_data[duration_boxplot_data$dlevel == level,]
    plot_ly(data, x = ~DURATION, color = ~EVENT_TYPE, type = "box") %>% 
      layout(title="Boxplots of Duration",
             xaxis=list(title="Duration"),
             margin = list(l = 100))
    
    
  })
  
  output$plotw3 <- renderPlot({
    theme_dotplot <- theme_bw(16) +
      theme(axis.text.y = element_text(size = rel(.75)),
            axis.ticks.y = element_blank(),
            axis.title.x = element_text(size = rel(.75)),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(size = 0.5),
            panel.grid.minor.x = element_blank())+
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplot(duration_mean_data) +
      geom_point(aes(mean, fct_reorder(EVENT_TYPE, mean)), size = 2, alpha = 0.8, color = "blue") +
      theme_dotplot +
      ylab("EVENT_TYPE")+
      xlab("Mean of Duration") + 
      ggtitle("Mean of Duration over Event_types")
    
  })
  
  output$plotw4 <- renderPlotly({
    class = input$var_zhendong03
    data <- duration_injure_data[duration_injure_data$EVENT_TYPE == class, ]
    fv <- data %>% lm( INJURE_DEATH ~ DURATION,.) %>% fitted.values()
    plot_ly(
      data, x = ~DURATION, y = ~INJURE_DEATH,
      color = ~INJURE_DEATH, size = ~INJURE_DEATH
    ) %>% add_markers(y = ~INJURE_DEATH) %>%
      add_trace(x = ~DURATION, y = fv, mode = "lines") %>%
      layout(title="Scatter plot",
             xaxis=list(title="Duration"),
             yaxis=list(title="Injuries and Deaths"),
             margin = list(l = 100),
             showlegend = F)
    
  })
  
  output$plotw5 <- renderPlotly({
    event = input$var_zhendong04
    data <- magnitude_histogram_data[magnitude_histogram_data$EVENT_TYPE == event, ]
  
    #plot_ly(x = ~data$DURATION, type = "histogram", marker = list(color = 'orange')) %>%
      #layout(title="Histogram of Duration", xaxis=list(title="Duration"))
    h2 <- ggplot(data, aes(MAGNITUDE)) +
      geom_histogram(bins = 60, fill = "lightblue", color = "blue") +
      ggtitle("Histogram of Magnitude")+
      theme(plot.title = element_text(hjust = 0.5)) +
      ylab("Frequency")
    ggplotly(h2)

  
  })
  
  output$plotw6 <- renderPlot({
    theme_dotplot <- theme_bw(16) +
      theme(axis.text.y = element_text(size = rel(.75)),
            axis.ticks.y = element_blank(),
            axis.title.x = element_text(size = rel(.75)),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(size = 0.5),
            panel.grid.minor.x = element_blank())+
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplot(magnitude_mean_data) +
      geom_point(aes(mean, fct_reorder(EVENT_TYPE, mean)), size = 2, alpha = 0.8, color = "blue") +
      theme_dotplot +
      ylab("EVENT_TYPE")+
      xlab("Mean of Magnitude") + 
      ggtitle("Mean of Magnitude over Event_types")
  })
  
  output$plotw7 <- renderPlotly({
    class = input$var_zhendong05
    data <- duration_magnitude_data[duration_magnitude_data$EVENT_TYPE == class, ]
    fv <- data %>% lm( MAGNITUDE ~ DURATION,.) %>% fitted.values()
    plot_ly(
      data, x = ~DURATION, y = ~MAGNITUDE,
      color = ~MAGNITUDE, size = ~MAGNITUDE
    ) %>% add_markers(y = ~MAGNITUDE) %>%
      add_trace(x = ~DURATION, y = fv, mode = "lines") %>%
      layout(title="Scatter plot",
             xaxis=list(title="Duration"),
             yaxis=list(title="Magnitude"),
             margin = list(l = 100),
             showlegend = F)
    
  })
  
  output$plotw8 <- renderPlotly({
    
    ggplot(Tornado_state,aes(x=reorder(region,value),y=value)) +
      geom_bar(stat = 'identity',fill='darkslateblue') +
      xlab("State") + 
      ylab("Frequency of Tornado") +
      coord_flip() +
      ggtitle('Tornado distribution over US')+
      theme(axis.title.x = element_text(size = 12, face = "bold"),axis.title.y = element_text(size = 15, face = "bold"),axis.text=element_text(size=6,vjust = 0.5, hjust = 0.5, angle = 0))+
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
  })
  
  output$plotw9 <- renderPlotly({
    
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa')
      
    )
    
    plot_geo(Tornado_state, locationmode = 'USA-states') %>%
      add_trace(
        z = ~value, locations = ~code,
        color = ~value, colors = colorRampPalette(brewer.pal(11,"RdPu"))(100)
      ) %>%
      colorbar(title = "Frequency") %>%
      layout(
        title = '2017 USA Tornado Frequency by State',
        geo = g
      )
    
  })
  
  output$plotw10 <- renderPlot({
    theme_dotplot <- theme_bw(12) +
      theme(axis.text.y = element_text(size = rel(.75)),
            axis.ticks.y = element_blank(),
            axis.title.x = element_text(size = rel(.75)),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(size = 0.5),
            panel.grid.minor.x = element_blank())+
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplot(Tornado_month_data) +
      geom_point(aes(value, fct_reorder(MONTH_NAME, value)), size = 2, alpha = 0.8, color = "blue") +
      theme_dotplot +
      ylab("Occuring Month")+
      xlab("Frequency of Month") + 
      ggtitle("2017 Monthly Frequency of Tornado")
  })
  
  output$plotw11 <- renderPlotly({
    type = input$var_zhendong06
    if(type == "Frequency") {
      ggplot(Tornado_timeseries, aes(YEAR, Frequency)) +
        geom_line() +
        geom_point() 
    }else if(type == "Total Damage"){
      ggplot(Tornado_timeseries, aes(YEAR, Total_damage)) +
        geom_line() +
        geom_point() +
        ylab("Total Damage(k)")
    }else{
      ggplot(Tornado_timeseries, aes(YEAR, Mean_damage)) +
        geom_line() +
        geom_point()+
        ylab("Mean Damage(k)")
    }
    
  })
  
  output$plotz1 <- renderPlot({
    # Render a barplot
    barplot(new_matrix[,input$region], 
            main=input$region,
            ylab="Number of Death and Injuries",
            xlab="Year",
            col=c('deepskyblue'))
  })
  
  
  
  #lyh
  
  output$plotl1 <- renderPlotly({
    
    df <- data[which(data$STATE==input$var_lyh01),]
    topk=sort(table(df$EVENT_TYPE),decreasing = T)
    topk_df=as.data.frame(topk)
    newdf=topk_df[1:5,]
    
    p <-ggplot(newdf,aes(x=reorder(Var1, -Freq),y=Freq))+geom_bar(stat = "identity")+xlab("Top 5 Event")
    ggplotly(p)
  })
  
  output$plotl2 <- renderPlotly({
    df <- data[which(data$EVENT_TYPE==input$var_lyh02),]
    topk=sort(table(df$STATE),decreasing = T)
    topk_df=as.data.frame(topk)
    newdf=topk_df[1:5,]
    
    p <-ggplot(newdf,aes(x=reorder(Var1, -Freq),y=Freq))+geom_bar(stat = "identity")+xlab("Top 5 STATE")
    ggplotly(p)
  })
  
  
#ej
  output$plotc1 <- renderPlotly({
    datainput <- switch(
      input$var_ej01,
      "Damage by Event Type" = ag1,
      "Damage by Tornado F-scale" = ag2,
      "Damage by Month" = ag3,
      "Damage by State" = ag4
    )
    
    p <-ggplot(datainput,aes(x=reorder(Category, x),y=x))+geom_point(color="green")+coord_flip()+ylab("Property Damage")+xlab(input$factor)
    ggplotly(p)
  })
  
  output$plotc2 <- renderPlotly({
    df <- data[which(data$STATE==input$var_ej02),]
    p <-ggplot(df)+geom_histogram(aes(x=df$LOG_DAMAGE_PROPERTY,y = ..density..), bins = 15)+geom_density(aes(x=df$LOG_DAMAGE_PROPERTY),color='red',alpha=.2,fill="#FF6666")+xlab("Log Property Damage")
    ggplotly(p)
  })
  
  m <- list(color = toRGB("black"))
  m2 <- list(color = toRGB("black", 0.2))
  compute_bins <- function(x, n) {
    list(
      start = min(x),
      end = max(x),
      size = (max(x) - min(x)) / n
    )
  }
  
  output$plotc3 <- renderPlotly({
    x <- data$LOG_DAMAGE_PROPERTY
    xbins <- compute_bins(x, input$xbins)
    p <- plot_ly(x = x, type = "histogram", autobinx = F, 
                 xbins = xbins, marker = m2)
    s <- event_data("plotly_selected")
    if (length(s$x) > 0) {
      p <- add_trace(p, x = s$x, type = "histogram", autobinx = F, 
                     xbins = xbins, marker = m)
    }
    p %>%
      config(displayModeBar = T, showLink = F) %>%
      layout(showlegend = F, barmode = "overlay", yaxis = list(title = "count"),
             xaxis = list(title = "", showticklabels = T))
  })
  
}


# Run the application
shinyApp(ui = ui, server = server)
