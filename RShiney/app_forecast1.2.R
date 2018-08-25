#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(png)
library(readr)
library(reshape2)
library(plotly)
library(forecast)
library(tseries)
#library(devtools)

setwd("C:/Users/sushama/Documents/MSIS/Summer_Semester/Capstone/Work/Shiny")
ds <- read.csv('Latest.csv', stringsAsFactors = FALSE)

ds$Timecard_Pay_Date <- as.Date(ds$Timecard_Pay_Date, format = '%Y-%m-%d')
elements <- names(ds)
elements <- elements[elements != "Timecard_Pay_Date"] 
for(i in elements)
{
  if(any(ds[[i]] == 'NULL'))
  {
    if(i == "SOW")
    {
      ds[[i]][ds[[i]] == 'NULL'] <- "Anthem"
    }
    else
    {
      ds[[i]][ds[[i]] == 'NULL'] <- 0
    }
  }
}
#due to presense of NULL, these columns are converted to character type. hence need to convert them into numeric
ds$Estimated_Total_Hours <- as.numeric(as.character(ds$Estimated_Total_Hours))
ds$Estimated_Total_Expense <- as.numeric(as.character(ds$Estimated_Total_Expense))
ds$Actual_Total_Hours <- as.numeric(as.character(ds$Actual_Total_Hours))
ds$Actual_Total_Expense <- as.numeric(as.character(ds$Actual_Total_Expense))

#counting total consumed budget till data
ds$cumulative_expense <- NULL
for (i  in 1: nrow(ds))
{ 
  if (i==1)
  {
    ds$cumulative_expense[i]<- ds$Actual_Total_Expense[i]
  }
  else
  {
    ds$cumulative_expense[i]<- ds$cumulative_expense[i-1] + ds$Actual_Total_Expense[i]
    
  }
}

# To control date range on visualization
min_date <- min(ds$Timecard_Pay_Date)
max_date <- max(ds$Timecard_Pay_Date)

#This will reshape data in a particular format
ds.reshaped <- melt(ds, id= c("Timecard_Pay_Date","SOW"))

#we need only actual and predicted expenses in the chart
ds.reshaped.expenses <- ds.reshaped[(ds.reshaped$variable == "Actual_Total_Expense") | (ds.reshaped$variable == "Estimated_Total_Expense"),]

# UI
ui <- fluidPage(
  sidebarLayout(
    
    # Input(s)
    sidebarPanel(
      
     
      
      br(), br(),
      
      img(src = "Connexionpoint.png", height = "110px"),
      h5("HEALTHCARE IS COMPLICATED. WE MAKE IT SIMPLE."),
      
      
      br(), br(),
      # Break for visual separation
      
      #SOW Selection
      selectInput("select", label = ("Select project"), 
                  choices = list("Anthem" = 1), 
                  selected = 1),
      
      # Date input
      dateRangeInput(inputId = "date",
                     label = "Select dates:",
                     start = "2018-01-02", 
                     end = "2018-01-30",
                     min = min_date,
                     max = max_date,
                     startview = "year"),

      # Select filetype
      radioButtons(inputId = "filetype",
                   label = "Select filetype:",
                   choices = c("csv", "tsv"),
                   selected = "csv"),
      
      # Select variables to download
      #checkboxGroupInput(inputId = "selected_var",
                         #label = "Select variables:",
                         #choices = names(ds),
                         #selected = c("title")),
 
      HTML("Select filetype, then hit 'Download'."),
      br(),br(),  # line break and some visual separation
      downloadButton("download_data", "Download"),
      br(),br(),br(),
      
      h5("Built with",
         img(src = "r.png", height = "30px"),
         "by",
         img(src = "shiny.png", height = "30px"),
         ".")
      
    ),
    
    # Output(s)
    mainPanel(
      #fluidRow( splitLayout(cellWidths = c("50%", "50%"), plotOutput("scatterplot"), plotOutput("scatterplot1"))),
     
     
      #HTML("Select filetype and variables, then hit 'Download data'."),
      #br(), br(), # line break and some visual separation
      #downloadButton("download_data", "Download data")
      
      #tabsetPanel(type = "tabs",
                  #tabPanel("Plot", plotOutput("plot"))),
      
      tabsetPanel(type = "tabs",
                  tabPanel("Graphs", 
                           h3("Actual Spend(Red) versus Estimates Spend(Blue)"), 
                           plotlyOutput("scatterplot", width = "80%",height = "260px" ),
                           tags$head(tags$style(HTML("div.col-sm-8 {padding:1px}"))),
                           h3("Burn Chart For Consumer Budget"), 
                           plotlyOutput("scatterplot1", width = "100%",height = "260px")
                           ),
                  tabPanel("Forecasting", 
                           plotlyOutput("scatterplot2", width = "100%",height = "260px")
                           #h3("Forecasting")
                           ))
    )
  )
)

# Server
server <- function(input, output) {
  
  # Create the plot
  output$scatterplot <- renderPlotly({
    req(input$date)
    ds.reshaped.expenses1 <- ds.reshaped.expenses %>%
    filter(Timecard_Pay_Date >= input$date[1] & Timecard_Pay_Date <= input$date[2])
    q.expenses <- ggplot(data = ds.reshaped.expenses1, 
                         aes(x=Timecard_Pay_Date, y=value,color = variable,
                             text = paste('Expenses(in $):', value,
                                          '<br>Date: ', as.Date(Timecard_Pay_Date))
                         ))+
      
      geom_line(group = 2 ) +  theme(legend.position="none")+
      labs(x = "Date", y ="Expenses($)")+
      theme(plot.margin=unit(c(0.1,0.1,0.3,0.8),"cm"))
    print(ggplotly(q.expenses, tooltip = "text"))
  })
  
  output$scatterplot1 <- renderPlotly({
    req(input$date)
    ds <- ds%>%
    filter(Timecard_Pay_Date >= input$date[1] & Timecard_Pay_Date <= input$date[2])
    g.trend <- ggplot(data=ds, aes(x=Timecard_Pay_Date, y=cumulative_expense, 
                                   text = paste('Consumed Budget(in $):', cumulative_expense,
                                                '<br>Date: ', as.Date(Timecard_Pay_Date)))) +
      geom_line( size = 1, color = "blue", group = 3) +
      theme(legend.position="none") +
      theme(plot.margin=unit(c(0.1,0.1,0.3,0.8),"cm")) +
      labs(x= "Date", y = "Budget($)" )+
      theme(plot.margin=unit(c(0.1,5.0,0.3,0.8),"cm"))
      ggplotly(g.trend, tooltip = "text")
    
    
  })
  
  output$scatterplot2 <- renderPlotly({
    ts.efforts <- ts(ds$Actual_Total_Hours, start = ds[1,1], frequency = 7)
    #ts.efforts
    #str(ts.efforts)
    
    #autoplot(ts.efforts, facets = TRUE) 
    #plot(ts.efforts)
    
    fit <- ets(ts.efforts)
    #fit
    forecast_length <- 7
    fore <- forecast(fit,forecast_length)
    #accuracy(fit)
    fore.dates <- seq(as.Date(ds$Timecard_Pay_Date[length(ds$Timecard_Pay_Date)], origin=ds[1,1]), 
                      by=ds$Timecard_Pay_Date[length(ds$Timecard_Pay_Date)] - ds$Timecard_Pay_Date[length(ds$Timecard_Pay_Date)-1], 
                      len=forecast_length)
    # ds.out is dataset to hold forecasted values
    ds.out <- data.frame(fore.dates,fore$mean);
    names(ds.out )[names(ds.out ) == "fore.dates"] <- "Date"
    names(ds.out )[names(ds.out ) == "fore.mean"] <- "Forecasted Efforts"
    #ds.out 
    #fore.dates
    
    p <- plot_ly() %>%
      add_lines(x = as.Date(ds$Timecard_Pay_Date, origin=ds[1,1] ) , y = ts.efforts,
                color = I("black"), name = "Observed hours allocation") %>%
      add_ribbons(x = fore.dates, ymin = fore$lower[, 2], ymax = fore$upper[, 2],
                  color = I("gray95"), name = "95% confidence") %>%
      add_ribbons(x = fore.dates, ymin = fore$lower[, 1], ymax = fore$upper[, 1],
                  color = I("gray80"), name = "80% confidence") %>%
      add_lines(x =fore.dates, y = fore$mean, color = I("blue"), name = "Predicted hours allocation") %>%
      layout(title = "Forecasting of Time allocation(in hours) for next 7 days",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Time allocation (in hr)"))  
    
    ggplotly(p)
    
    
  })
 
  # Download file
  output$download_data <- downloadHandler(
    
    
    filename = function() {
      paste0("ds.", input$filetype)
    },
    
    
    content = function(file) { 
      if(input$filetype == "csv"){ 
        write_csv(ds %>%
                    mutate(Timecard_Pay_Date = as.Date(Timecard_Pay_Date)) %>% 
                    filter(Timecard_Pay_Date >= input$date[1] & Timecard_Pay_Date <= input$date[2]),path = file)
      }
      if(input$filetype == "tsv"){ 
        write_tsv(ds%>%
                     mutate(Timecard_Pay_Date = as.Date(Timecard_Pay_Date)) %>% 
                     filter(Timecard_Pay_Date >= input$date[1] & Timecard_Pay_Date <= input$date[2]),path = file
                    # %>%select(input$selected_var)
                  )
      }
    }
  )
  
  
  
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
