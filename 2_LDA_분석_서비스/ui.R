##################################################################
# 연구동향 분석 서비스
##################################################################

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("LDA Analysis"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      textInput(inputId = "nTopics", label = "# of topics", value = "5"),
      textInput(inputId = "seed", label = "SEED", value = "2007"),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      
      tags$head(tags$script(src = "message-handler.js")),
      actionButton("do", "Do analysis")
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----

      
      tabsetPanel(type = "tabs",
                  tabPanel("Data", tableOutput("data")),
                  tabPanel("LDAVis", tableOutput("ldavis")),
                  tabPanel("DocTopics", tableOutput("doc_topics")),
                  tabPanel("TopicDistVis", plotOutput("topicdist", width = "100%", height= 800)),
                  tabPanel("Trends", plotOutput("trends", width = "100%", height= 800))
      )
    )

  )
)
