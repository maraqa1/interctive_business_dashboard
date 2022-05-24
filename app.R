

library(shiny)
library(shinydashboard)
library(fontawesome)
source("R/map.R")
source("R/graphs.R")

ui <- 
  shinydashboard::dashboardPage( 
    
    dashboardHeader(title = "UK Business Dashboard"),
    #side bar - setup the filters
    dashboardSidebar(
      #setup regional filters
      checkboxGroupInput(inputId ="regional_select","Choose Regions", 
                         choices=c("England"="England","Wales"="Wales","Scotland"="Scotland","Northern Ireland"="Northern Ireland"),
                         selected=c("England","Wales","Scotland","Northern Ireland"))
    ),
    
  dashboardBody(
    fluidRow(
    column(4,

           valueBoxOutput("value_1",width = "100%"),
           box(title="UK Regions Map",leafletOutput("bid_no_bid_map", width="100%",height="400px"),width = NULL ,solidHeader = TRUE,status = "primary")
           
    ),
    column(4,
           box(title="Balance",plotOutput("Plot.balance",width="100%",height="200px"),width = NULL ,solidHeader = TRUE,status = "primary"),
           box(title="Age",plotOutput("Plot.age",width="100%",height="200px"),width = NULL ,solidHeader = TRUE,status = "primary")
           #box(title="UK regions",plotOutput("bid_no_bid_map"),width = NULL ,solidHeader = TRUE,status = "primary"),
           
    ),
    column(4,
           box(title="Gender",plotOutput("Plot.gender",width="100%",height="200px"),width = NULL ,solidHeader = TRUE,status = "primary"),
           box(title="Job",plotOutput("Plot.job",width="100%",height="200px"),width = NULL ,solidHeader = TRUE,status = "primary")

           #box(title="UK regions",plotOutput("bid_no_bid_map"),width = NULL ,solidHeader = TRUE,status = "primary"),
    )
  ) #fluid page
  ) #dashboardbody
)

server <- function(input, output,session) {
  
  # read the data
  
  data.file="data/P6-UK-Bank-Customers.csv"
  
  df.read <<-utils::read.csv(data.file)
# filter input
  df <- reactive({ 
      df.read
 
  })
  output$support_list_1 = renderTable({
    #support_list()
    data.table::data.table(temp)
  }) 
  
  
 
########################################################################  
  
  
  
  # regional value filter "input$regional_select"
  output$regional_value <- renderText({ 
    print(input$regional_select)
    #paste("You have selected", output$number) 
  })
 
  
  
  #column 1 output - value box
  output$bid_no_bid_map <- renderLeaflet({plot_uk_map(regional.filter=input$regional_select ) }) 
  
  #column 1 output - map
  output$value_1<-renderValueBox({
    
    update_plots(regional.filter=input$regional_select)$count  %>% 
      
      as.data.frame()%>%
      valueBox(subtitle = "Cases",
               icon = icon("table",lib = "font-awesome"),
               color = "purple"
      )
    })
  
  #column 2 output plot 1
  output$Plot.balance <- renderPlot({update_plots(regional.filter=input$regional_select)$Balance })

  #column 2 output plot 2
  output$Plot.age <- renderPlot({update_plots(regional.filter=input$regional_select)$Age })
  
  
  
  #column 3 output plot 1
  output$Plot.gender <- renderPlot({update_plots(regional.filter=input$regional_select)$Gender })
  
  #column 3 output plot 2
  output$Plot.job <- renderPlot({update_plots(regional.filter=input$regional_select)$Job })
  
}

shinyApp(ui = ui, server = server)

