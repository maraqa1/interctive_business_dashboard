
library(shinycssloaders)
library(shiny)
library(shinydashboard)
library(fontawesome)
library(ggplot2)
source("R/map.R")
source("R/graphs.R")

renv::isolate()

ui <- 
  shinydashboard::dashboardPage( 
    

    dashboardHeader(title = "UK Business Dashboard",
                    #long title
                    titleWidth = 450),

    #side bar - setup the filters
    dashboardSidebar(
      #setup regional filters
      checkboxGroupInput(inputId ="regional_select","Choose Regions", 
                         choices=c("England"="England","Wales"="Wales","Scotland"="Scotland","Northern Ireland"="Northern Ireland"),
                         selected=c("England","Wales","Scotland","Northern Ireland"))
    ),
    
  dashboardBody(
    fluidRow( 
              h2(" Welcome to UK business Sample Dashboard", style = "font-family: monospace",style="color:#0000A0"),
              tags$br(),
              tags$br(),
              ),
    
    fluidRow(
    column(4,

           valueBoxOutput("value_1",width = "100%"),
           box(title="UK Regions Map",leafletOutput("bid_no_bid_map", width="100%",height="400px") %>% withSpinner(color="#0dc5c1"),width = NULL ,solidHeader = TRUE,status = "primary")
           
    ),
    column(4,
           box(title="Bfliudalance",plotOutput("Plot.balance",width="100%",height="200px")%>% withSpinner(color="#0dc5c1")%>% withSpinner(color="#0dc5c1"),width = NULL ,solidHeader = TRUE,status = "primary"),
           box(title="Age",plotOutput("Plot.age",width="100%",height="200px")%>% withSpinner(color="#0dc5c1"),width = NULL ,solidHeader = TRUE,status = "primary")
           #box(title="UK regions",plotOutput("bid_no_bid_map"),width = NULL ,solidHeader = TRUE,status = "primary"),
           
    ),
    column(4,
           box(title="Gender",plotOutput("Plot.gender",width="100%",height="200px")%>% withSpinner(color="#0dc5c1"),width = NULL ,solidHeader = TRUE,status = "primary"),
           box(title="Job",plotOutput("Plot.job",width="100%",height="200px")%>% withSpinner(color="#0dc5c1"),width = NULL ,solidHeader = TRUE,status = "primary")

           #box(title="UK regions",plotOutput("bid_no_bid_map"),width = NULL ,solidHeader = TRUE,status = "primary"),
    )
  ) #fluidRow
  ) #fluidBody
)

server <- function(input, output,session) {


  # Part A. Filters  
  # A.1 Get regional Filters
  # regional value filter "input$regional_select"
  output$regional_value <- renderText({ 
    print(input$regional_select)
    #paste("You have selected", output$number) 
  })
  
########################################################################  
  


  
  #Part B. The data
  
  # B.1.0 Set the path
  data.file="data/P6-UK-Bank-Customers.csv"
  
  # B.2.0 Read the data & apply create fields
  df.read <<-utils::read.csv(data.file) %>% 
      # transform the data to create bins for 1_ age and 2-income
      # 1.0 create bin for age
      dplyr::mutate(age.bin=cut(Age, seq(min(Age), max(Age) + 4, 5), right = FALSE)) %>% 
      #dplyr::select(age.bin) %>% unique()
      # 2.0 create bin for Balance - Divide by 1000
      #dplyr::mutate(balance.bin=cut(Balance, seq(min(Balance), max(Balance) + 4, 5), right = FALSE)) %>%
      dplyr::mutate(balance.bin=cut(Balance/1000, pretty(Balance/1000,15),right = FALSE ) )# %>%
    #dplyr::mutate_at(dplyr::vars(balance.bin), function(x) as.character(paste0(as.character(x)," K") )  )
   
  # B.3.0 apply filters in the regions to the whole data-set before feeding it to the functions
  df.filter <- reactive({ 
    return(
    df.read %>%
      dplyr::filter(Region %in% input$regional_select ) 
    )
  })
########################################################################  
  #Part C. Call the functions to generate output.

 
  #column 1 output - Value box
  output$value_1<-renderValueBox({
    
    update_plots(df.filter(),regional.filter=input$regional_select)$count  %>% 
      as.data.frame()%>%
      valueBox(subtitle = "Cases",
               icon = icon("table",lib = "font-awesome"),
               color = "purple"
      )
    })
  
  #column 1 output - Map data
  output$bid_no_bid_map <- renderLeaflet({plot_uk_map(regional.filter=input$regional_select ) }) 
  

  #column 2 output plot 1

  output$Plot.balance <- renderPlot({update_plots(df.filter(),regional.filter=input$regional_select)$Balance })

  #column 2 output plot 2
  output$Plot.age <- renderPlot({update_plots(df.filter(),regional.filter=input$regional_select)$Age })
  
  #column 3 output plot 1
  output$Plot.gender <- renderPlot({update_plots(df.filter(),regional.filter=input$regional_select)$Gender })
  
  #column 3 output plot 2
  output$Plot.job <- renderPlot({update_plots(df.filter(),regional.filter=input$regional_select)$Job })
  # the End


  

}

shinyApp(ui = ui, server = server)

