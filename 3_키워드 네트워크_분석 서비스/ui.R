##################################################################
# 연구동향 분석 서비스
##################################################################

rm(list=ls())
setwd("/srv/shiny-server/kn")
options(shiny.maxRequestSize=5000*1024^2) 
options(shiny.plot.res=300)
# Shiny
library(shiny)
library(NLP4kec)
library(readr)
library(KoNLP)
library(arules)
library(igraph)
library(combinat)



# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Keyword Association Analysis"),
  
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

      textInput(inputId = "category", label = "Category", value = "물환경"),
      textInput(inputId = "start_date", label = "StartDate", value = "20100101"),
      textInput(inputId = "end_date", label = "EndDate", value = "20171231"),
      actionButton("filtering", "Do filtering"),
      
      # Horizontal line ----
      tags$hr(),
      
      textInput(inputId = "SEED", label = "SEED", value = "1001"),
      textInput(inputId = "p_supp", label = "Support", value = "0.01"),
      textInput(inputId = "p_conf", label = "Conf", value = "0.01"),
      textInput(inputId = "n_rel", label = "# of relation", value = "30"),
      
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
      tabsetPanel(type = "tabs",
                  tabPanel("Data", tableOutput("data")),
                  tabPanel("Result", tableOutput("result")),
                  tabPanel("Plot", plotOutput("plot", width = "100%", height= 800))
      )
      # Output: Data file ----
      #tableOutput("contents"),
      #uiOutput("tab")
    )
    
  )
)