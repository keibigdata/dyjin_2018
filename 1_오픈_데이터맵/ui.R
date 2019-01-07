library(stringr)
library(htmlwidgets)
library(shiny)
library(ggplot2)  # for the diamonds dataset

setwd("/srv/shiny-server/odm")
ui <- fluidPage(
  title = "OpenDataMap",
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel('기후변화', DT::dataTableOutput("r1")),
        tabPanel('대기환경', DT::dataTableOutput("r2")),
        tabPanel('물환경', DT::dataTableOutput("r3")),
        tabPanel('자연환경', DT::dataTableOutput("r4")),
        tabPanel('자원순환', DT::dataTableOutput("r5")),
        tabPanel('지구환경', DT::dataTableOutput("r6")),
        tabPanel('환경보건', DT::dataTableOutput("r7")),
        tabPanel('환경영향평가', DT::dataTableOutput("r8")),
        tabPanel('환경정책', DT::dataTableOutput("r9")),
        tabPanel('기타', DT::dataTableOutput("r10"))
        
    ),
    width=12
  )
)