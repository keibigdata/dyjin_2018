##################################################################
# 연구동향 분석 서비스
##################################################################

rm(list=ls())
setwd("/srv/shiny-server/kn")

options(shiny.maxRequestSize=5000*1024^2) 
options(shiny.plot.res=250)
# Shiny
library(shiny)
library(NLP4kec)
library(readr)
library(KoNLP)
library(arules)
library(igraph)
library(combinat)
library(DT)

s_date <- Sys.Date()
rn <- sample(1:100000000,1)

fn1 <- paste("./result/", s_date,"_",rn,".csv",sep="")
fn2 <- paste("./result/", s_date,"_",rn,".pdf",sep="")


assoc_analysis <- function(p_supp, p_conf,SEED,n_rel,s_words)
{
  
  n_rel <- as.numeric(n_rel)
  df <- read.csv(fn1)
  s_words = unlist(strsplit(s_words,","))
  rules <- as.vector(df[,"TITLE"])
  rules <- rules[!is.na(rules)]
  tran <- Map(extractNoun, rules)
  print("OK")
  tran <- unique(tran)
  
  tran <- sapply(tran, unique)
  tran <- sapply(tran, function(x) {Filter(function(y) {nchar(y) > 1 && is.hangul(y)},x)})
  tran <- Filter(function(x){length(x)>=2},tran)
  tran <- Filter(function(x){all(s_words != x)},tran)

  print(p_supp)
  print(p_conf)
  
  #print(names(tran))
  
  names(tran) <- paste("Tr", 1:length(tran), sep="")
  wordtran <- as(tran, "transactions")

  wordtab <- crossTable(wordtran)
  

  
  #지지도와 신뢰도를 낮게 설정할수록 결과 자세히 나옴
  ares <- apriori(wordtran, parameter=list(supp=as.numeric(p_supp), conf=as.numeric(p_conf))) 
  ares <- sort(ares, by="lift")
  
  #print(ares)
  
  print("Whyerror")
  result_mat <- as.matrix(inspect(ares))
  colnames(result_mat)[2] <- "->"
  print(as.numeric(result_mat[,"count"]))
  idx <- which(result_mat[,"lhs"] != "{}")
  idx <- intersect(idx,which(as.numeric(result_mat[,"count"]) >= 2))
 
  print(length(idx))
  result_mat <- result_mat[idx,]
  ares <- ares[idx,]
  
  if(length(idx) < n_rel)
    n_rel <- length(idx)

  idx <- 1:n_rel

  result_mat <- result_mat[idx,]
  
  ares <- ares[idx,]
  
  rules <- labels(ares, ruleSep=" ")
  rules <- gsub("\\{","",rules)
  rules <- gsub("\\}","",rules)
  rules <- sapply(rules, strsplit, " ",  USE.NAMES=F)
  rulemat <- do.call("rbind", rules)
  
  
  
  
  # idx에 포함되는것은 단어들간의 연관관계가 아니기 때문에 제외
  ruleg <- graph.edgelist(rulemat[idx,],directed=F) 
  

  #plot.igraph(ruleg, vertex.label=V(ruleg)$name, vertex.label.cex=0.5, vertex.size=20,layout=layout.fruchterman.reingold.grid)
  
  #### 단어근접중심성파악 ####
  
  closen <- closeness(ruleg)
  
  #closen <- closen[c(1:n_words)] # 상위 1~10 단어 근접중심성 보기
  
  #plot(closen, col="red",xaxt="n", lty="solid", type="b", xlab="단어", ylab="closeness")
  #points(closen, pch=16, col="navy")
  #axis(1, seq(1, length(closen)), V(ruleg)$name[c(1:n_words)], cex=5)
  
  
  #### node(vertex), link(edge) 크기 조절 (복잡) ####
  
  V(ruleg)$size<- degree(ruleg)/ (0.25 * (n_rel/20))
  #condition<-V(ruleg)[degree(ruleg)<0.4]
  #ruleg1<-delete.vertices(ruleg, condition)
  #E(ruleg1)$color<-ifelse(E(ruleg1)$grade>=1, "red", "gray")
  #set.seed(1001)
  #plot(ruleg1)
  
  #### node(vertex), link(edge) 크기 조절 (단순) ####
  ruleg<-simplify(ruleg)
  head(sort(degree(ruleg), decreasing=T))
  head(sort(closeness(ruleg), decreasing=T))
  head(sort(betweenness(ruleg), decreasing=T))
  set.seed(SEED)
  #plot(ruleg)
  
  #### 매개중심성 #### 
  btw<-betweenness(ruleg)
  btw.score<-round(btw)+1
  btw.colors<-rev(heat.colors(max(btw.score)))
  V(ruleg)$color<-btw.colors[btw.score]
  #V(ruleg)$degree<-degree(ruleg)
  #V(ruleg)$label.cex<-2*(V(ruleg)$degree / max(V(ruleg)$degree))
  ret <- list('result_mat' = result_mat, 'ruleg' = ruleg)
  return (ret)
}



# Define server logic to read selected file ----
server <- function(input, output,session) {

  #print(input$seed)
  #print(input$nTopics)

  observeEvent(input$do, {
    output$result <- renderText({"Processing..."})
    print(input$p_supp)
    print(input$p_conf)
    print(input$SEED)
    ret <- assoc_analysis(input$p_supp,input$p_conf,input$SEED,input$n_rel,input$s_words)
    #print(ret$ruleg)
   
    #output$result <- ret$result_mat
    
    output$result <- renderTable({
      return(ret$result_mat)
    })
    
    output$plot <- renderPlot({
    
    # Render a barplot
    plot(ret$ruleg)
      
	cairo_pdf(fn2,family="Nanumgothic")
	plot(ret$ruleg)
	dev.off()
    })
  })
  
  observeEvent(input$filtering, {
    
    output$data <- DT::renderDataTable({
      
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.
      
      req(input$file1)
      
      #df <- read.csv(input$file1$datapath, header = input$header,sep = input$sep, quote = input$quote
      df <- read_csv(input$file1$datapath,locale = locale(encoding = "EUC-KR"))
      df_idx <- 1:nrow(df)
      
      print(df_idx)
      category <- input$category
      start_date <- input$start_date
      end_date <- input$end_date
      
      category_idx <- which(df[,"CATEGORY"] == category)
      start_idx <- which(df[,"DATE"] >= as.numeric(start_date))
      end_idx <- which(df[,"DATE"] <= as.numeric(end_date))
      
      if(length(category_idx) == 0)
        category_idx <- df_idx
      
      if(length(start_idx) == 0)
        start_idx <- df_idx
      
      if(length(end_idx) == 0)
        end_idx <- df_idx
        
      c_idx <- intersect(intersect(category_idx,start_idx),end_idx)
      df <- df[c_idx,]
      write.table(df,fn1,col.names=TRUE,row.names=FALSE,sep=",")
      if(input$disp == "head") {
        return(head(df))
      }
      else {
        return(df)
      }
      
    })
      
  })
  
  output$data <- DT::renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
	
    req(input$file1)
    
    #df <- read.csv(input$file1$datapath, header = input$header,sep = input$sep, quote = input$quote

    df <- read_csv(input$file1$datapath,locale = locale(encoding = "EUC-KR"))
    category_list <- df$CATEGORY
    updateSelectInput(session, "category",
                      #label = c('기후','물'),
                      choices = category_list
    )
    write.table(df,fn1,col.names=TRUE,row.names=FALSE,sep=",")
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
}

# Create Shiny app ----
#shinyApp(ui, server)
