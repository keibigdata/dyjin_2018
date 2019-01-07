library(stringr)
library(htmlwidgets)
setwd("/srv/shiny-server/odm")

result <- read.csv("./result_mod.csv",stringsAsFactors = FALSE)

cnt <- 1000

result[,6:15] <- result[,6:15] / (1+log(result[,5]))
result[,16] <- str_replace_all(result[,16],"nan,", "")
result[,16] <- substr(result[,16],2,1000)
result[,16] <- str_replace_all(result[,16],",nan", "")
result[,16] <- str_replace_all(result[,16],"nan", "")
result[,16] <- trimws(result[,16])

result[,17] <- str_replace_all(result[,17],"nan,", "")
result[,17] <- substr(result[,17],2,1000)
result[,17] <- str_replace_all(result[,17],",nan", "")
result[,17] <- str_replace_all(result[,17],"nan", "")
result[,17] <- trimws(result[,17])
result[,16] <- substr(result[,16],2,1000)
result[,17] <- substr(result[,17],2,1000) 


# 각 분야별 Rank로 정렬

r1 <- result[order(-result[,"기후변화"]),]
r1 <- r1[1:cnt,c("Website","타이틀","설명","KR_Keywords","EN_Keywords")]
ranking <- 1:cnt
r1 <- cbind(1:cnt,r1)
names(r1)[1] <- "Rank"


r2 <- result[order(-result[,"대기환경"]),]
r2 <- r2[1:cnt,c("Website","타이틀","설명","KR_Keywords","EN_Keywords")]
ranking <- 1:cnt
r2 <- cbind(1:cnt,r2)
names(r2)[1] <- "Rank"

r3 <- result[order(-result[,"물환경"]),]
r3 <- r3[1:cnt,c("Website","타이틀","설명","KR_Keywords","EN_Keywords")]
ranking <- 1:cnt
r3 <- cbind(1:cnt,r3)
names(r3)[1] <- "Rank"

r4 <- result[order(-result[,"자연환경"]),]
r4 <- r4[1:cnt,c("Website","타이틀","설명","KR_Keywords","EN_Keywords")]
ranking <- 1:cnt
r4 <- cbind(1:cnt,r4)
names(r4)[1] <- "Rank"

r5 <- result[order(-result[,"자원순환"]),]
r5 <- r5[1:cnt,c("Website","타이틀","설명","KR_Keywords","EN_Keywords")]
ranking <- 1:cnt
r5 <- cbind(1:cnt,r5)
names(r5)[1] <- "Rank"

r6 <- result[order(-result[,"지구환경"]),]
r6 <- r6[1:cnt,c("Website","타이틀","설명","KR_Keywords","EN_Keywords")]
ranking <- 1:cnt
r6 <- cbind(1:cnt,r6)
names(r6)[1] <- "Rank"


r7 <- result[order(-result[,"환경보건"]),]
r7 <- r7[1:cnt,c("Website","타이틀","설명","KR_Keywords","EN_Keywords")]
ranking <- 1:cnt
r7 <- cbind(1:cnt,r7)
names(r7)[1] <- "Rank"

r8 <- result[order(-result[,"환경영향평가"]),]
r8 <- r8[1:cnt,c("Website","타이틀","설명","KR_Keywords","EN_Keywords")]
ranking <- 1:cnt
r8 <- cbind(1:cnt,r8)
names(r8)[1] <- "Rank"

r9 <- result[order(-result[,"환경정책"]),]
r9 <- r9[1:cnt,c("Website","타이틀","설명","KR_Keywords","EN_Keywords")]
ranking <- 1:cnt
r9 <- cbind(1:cnt,r9)
names(r9)[1] <- "Rank"

r9 <- result[order(-result[,"환경정책"]),]
r9 <- r9[1:cnt,c("Website","타이틀","설명","KR_Keywords","EN_Keywords")]
ranking <- 1:cnt
r9 <- cbind(1:cnt,r9)
names(r9)[1] <- "Rank"

r10 <- result[order(-result[,"기타"]),]
r10 <- r10[1:cnt,c("Website","타이틀","설명","KR_Keywords","EN_Keywords")]
ranking <- 1:cnt
r10 <- cbind(1:cnt,r10)
names(r10)[1] <- "Rank"

library(shiny)
library(ggplot2)  # for the diamonds dataset

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
    width=15
  )
)

cnt <- 50

server <- function(input, output) {
  
  # choose columns to display
  output$r1 <- DT::renderDataTable({
    DT::datatable(r1,rownames=FALSE, selection = 'none',options = list(columnDefs = list(list(
      targets = c(2,3,4,5),
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data.length > 150 ?",
        "'<span title=\"' + data + '\">' + data.substr(0, 150) + '...</span>' : data;",
        "}")
    ))), callback = JS('table.page(3).draw(false);'))
  })
  
  output$r2 <- DT::renderDataTable({
    DT::datatable(r2,rownames=FALSE, selection = 'none',options = list(columnDefs = list(list(
      targets = c(2,3,4,5),
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data.length > 150 ?",
        "'<span title=\"' + data + '\">' + data.substr(0, 150) + '...</span>' : data;",
        "}")
    ))), callback = JS('table.page(3).draw(false);'))
  })
  
  output$r3 <- DT::renderDataTable({
    DT::datatable(r3,rownames=FALSE, selection = 'none',options = list(columnDefs = list(list(
      targets = c(2,3,4,5),
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data.length > 150 ?",
        "'<span title=\"' + data + '\">' + data.substr(0, 150) + '...</span>' : data;",
        "}")
    ))), callback = JS('table.page(3).draw(false);'))
  })
  
  
  output$r4 <- DT::renderDataTable({
    DT::datatable(r4,rownames=FALSE, selection = 'none',options = list(columnDefs = list(list(
      targets = c(2,3,4,5),
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data.length > 150 ?",
        "'<span title=\"' + data + '\">' + data.substr(0, 130) + '...</span>' : data;",
        "}")
    ))), callback = JS('table.page(3).draw(false);'))
  })
  
  output$r5 <- DT::renderDataTable({
    DT::datatable(r5,rownames=FALSE, selection = 'none',options = list(columnDefs = list(list(
      targets = c(2,3,4,5),
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data.length > 150 ?",
        "'<span title=\"' + data + '\">' + data.substr(0, 150) + '...</span>' : data;",
        "}")
    ))), callback = JS('table.page(3).draw(false);'))
  })
  
  output$r6 <- DT::renderDataTable({
    DT::datatable(r6,rownames=FALSE, selection = 'none',options = list(columnDefs = list(list(
      targets = c(2,3,4,5),
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data.length > 150 ?",
        "'<span title=\"' + data + '\">' + data.substr(0, 150) + '...</span>' : data;",
        "}")
    ))), callback = JS('table.page(3).draw(false);'))
  })
  
  output$r7 <- DT::renderDataTable({
    DT::datatable(r7,rownames=FALSE, selection = 'none',options = list(columnDefs = list(list(
      targets = c(2,3,4,5),
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data.length > 150 ?",
        "'<span title=\"' + data + '\">' + data.substr(0, 150) + '...</span>' : data;",
        "}")
    ))), callback = JS('table.page(3).draw(false);'))
  })
  
  output$r8 <- DT::renderDataTable({
    DT::datatable(r8,rownames=FALSE, selection = 'none',options = list(columnDefs = list(list(
      targets = c(2,3,4,5),
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data.length > 150 ?",
        "'<span title=\"' + data + '\">' + data.substr(0, 150) + '...</span>' : data;",
        "}")
    ))), callback = JS('table.page(3).draw(false);'))
  })
  
  output$r9 <- DT::renderDataTable({
    DT::datatable(r9,rownames=FALSE, selection = 'none',options = list(columnDefs = list(list(
      targets = c(2,3,4,5),
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data.length > 150 ?",
        "'<span title=\"' + data + '\">' + data.substr(0, 150) + '...</span>' : data;",
        "}")
    ))), callback = JS('table.page(3).draw(false);'))
  })
  
  
  output$r10 <- DT::renderDataTable({
    DT::datatable(r10,rownames=FALSE, selection = 'none',options = list(columnDefs = list(list(
      targets = c(2,3,4,5),
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data.length > 150 ?",
        "'<span title=\"' + data + '\">' + data.substr(0, 150) + '...</span>' : data;",
        "}")
    ))), callback = JS('table.page(3).draw(false);'))
  })
}  

#shinyApp(ui, server)