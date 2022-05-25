

library(shiny)
library(shinydashboard)
library(fontawesome)
source("R/map.R")
source("R/graphs.R")

ui <- 
  shinydashboard::dashboardPage( 
    
<<<<<<< HEAD
    dashboardHeader(title = "UK Business Dashboard",
                    #long title
                    titleWidth = 450),
=======
    dashboardHeader(title = "UK Business Dashboard"),
>>>>>>> 81c77f5eedf8cfdd6c0e7e309e03212e7affbd3a
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
<<<<<<< HEAD

  # Part A. Filters  
  # A.1 Get regional Filters
=======
  
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
  
  
  
>>>>>>> 81c77f5eedf8cfdd6c0e7e309e03212e7affbd3a
  # regional value filter "input$regional_select"
  output$regional_value <- renderText({ 
    print(input$regional_select)
    #paste("You have selected", output$number) 
  })
<<<<<<< HEAD
  
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
=======
 
  
  
>>>>>>> 81c77f5eedf8cfdd6c0e7e309e03212e7affbd3a
  #column 1 output - value box
  output$bid_no_bid_map <- renderLeaflet({plot_uk_map(regional.filter=input$regional_select ) }) 
  
  #column 1 output - map
  output$value_1<-renderValueBox({
    
<<<<<<< HEAD
    update_plots(df.filter(),regional.filter=input$regional_select)$count  %>% 
=======
    update_plots(regional.filter=input$regional_select)$count  %>% 
>>>>>>> 81c77f5eedf8cfdd6c0e7e309e03212e7affbd3a
      
      as.data.frame()%>%
      valueBox(subtitle = "Cases",
               icon = icon("table",lib = "font-awesome"),
               color = "purple"
      )
    })
  
  #column 2 output plot 1
<<<<<<< HEAD
  output$Plot.balance <- renderPlot({update_plots(df.filter(),regional.filter=input$regional_select)$Balance })

  #column 2 output plot 2
  output$Plot.age <- renderPlot({update_plots(df.filter(),regional.filter=input$regional_select)$Age })
  
  #column 3 output plot 1
  output$Plot.gender <- renderPlot({update_plots(df.filter(),regional.filter=input$regional_select)$Gender })
  
  #column 3 output plot 2
  output$Plot.job <- renderPlot({update_plots(df.filter(),regional.filter=input$regional_select)$Job })
  # the End
=======
  output$Plot.balance <- renderPlot({update_plots(regional.filter=input$regional_select)$Balance })

  #column 2 output plot 2
  output$Plot.age <- renderPlot({update_plots(regional.filter=input$regional_select)$Age })
  
  
  
  #column 3 output plot 1
  output$Plot.gender <- renderPlot({update_plots(regional.filter=input$regional_select)$Gender })
  
  #column 3 output plot 2
  output$Plot.job <- renderPlot({update_plots(regional.filter=input$regional_select)$Job })
  
>>>>>>> 81c77f5eedf8cfdd6c0e7e309e03212e7affbd3a
}

shinyApp(ui = ui, server = server)

