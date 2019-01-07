##################################################################
# 연구동향 분석 서비스
##################################################################

rm(list=ls())
setwd("/srv/shiny-server/lda")
options(shiny.maxRequestSize=5000*1024^2) 

# Shiny

library(shiny)
library(ggpubr)
library(NLP4kec) # Custom Library
library(readr)
library(topicmodels)
library(LDAvis)
library(servr)
library(readr)
library(tm)
library(slam)
library(dplyr)
library(ggplot2) 
library(ggpubr)


# Create Shiny app ----

topic_clustering <- function(dpath,SEED, k)
{
  #형태소 분석기 실행
  SEED <- as.numeric(SEED)
  k <- as.numeric(k)
  
  baseData <- read_csv(dpath,locale = locale(encoding = "cp949"))
  parsedData <- baseData
  content <- as.vector(unlist(baseData["Content"]))
  print(content)
  pContent<- r_parser_r(content, language = "ko")
  
  
  #분석 결과 가져오기
  parsedData["Content"] <- pContent
  idx <- which(!is.na(parsedData["Content"]))
  parsedData <- parsedData[idx,]
  
  
  #컬럼명 변경하기
  colnames(parsedData) = c("id","name","pContent","year","month","day")
  
  write.csv(parsedData,"./result/input_data.csv")
  
  ## 단어간 스페이스 하나 더 추가하기 ##
  parsedDataRe = parsedData
  parsedDataRe$pContent = gsub(" ","  ",parsedDataRe$pContent)
  
  ##################################################################
  #Text Pre-processing
  ##################################################################
  #Corpus 생성
  corp<-VCorpus(VectorSource(parsedDataRe$pContent))
  #특수문자 제거
  corp <- tm_map(corp, removePunctuation)
  #소문자로 변경
  #corp <- tm_map(corp, tolower)
  #특정 단어 삭제
  corp <- tm_map(corp, removeWords, c("전략", "연구", "평가", "마련", "조사", "관리", "보다", "분석", "구축"))
  #동의어 처리
  #for (j in seq(corp))
  #{
  #  corp[[j]] <- gsub("kei", "한국환경정책평가연구원", corp[[j]])
  #}
  ##################################################################
  
  #Document Term Matrix 생성 (단어 Length는 2로 세팅)
  dtm<-DocumentTermMatrix(corp, control=list(removeNumbers=FALSE, wordLengths=c(2,Inf)))
  ## 한글자 단어 제외하기 ##
  colnames(dtm) = trimws(colnames(dtm))
  dtm = dtm[,nchar(colnames(dtm)) > 1]
  #Sparse Terms 삭제
  dtm <- removeSparseTerms(dtm, as.numeric(0.997))
  dtm
  ##Remove low tf-idf col and row
  term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
  new_dtm <- dtm[,term_tfidf >= 0]
  
  # Get_index
  
  doc_idx <- which(term_tfidf >= 0)
  
  new_dtm <- new_dtm[row_sums(new_dtm)>0,]
  
  ############################################
  ## Running LDA
  ############################################
  
  #LDA 실행
  lda_tm <- LDA(new_dtm, control=list(seed=SEED), k)
  
  #토픽별 핵심단어 저장하기
  term_topic <- terms(lda_tm, 150)
  
  #토픽별 핵심 단어 파일로 출력하기
  filePathName = paste("./result/term_topic.csv",sep="")
  write.table(term_topic, filePathName, sep=",", row.names=FALSE)
  
  #문서별 토픽 번호 저장하기
  doc_topic <- topics(lda_tm, 1)
  doc_topic_df <- as.data.frame(doc_topic)
  doc_topic_df$rown <- as.integer(row.names(doc_topic_df))
  
  #문서별 토픽 확률값 계산하기
  doc_Prob <- posterior(lda_tm)$topics
  doc_Prob_df <- as.data.frame(doc_Prob)
  filePathName = paste("./result/doc_prob_df.csv",sep="")
  write.table(doc_Prob_df, filePathName, sep=",", row.names=FALSE)
  
  #최대 확률값 찾기
  doc_Prob_df$maxProb = apply(doc_Prob_df, 1, max)
  filePathName = paste("./result/doc_prob_df.csv",sep="")
  write.table(doc_Prob_df, filePathName, sep=",", row.names=FALSE)
  
  #문서별 토픽번호 및 확률값 추출하기
  doc_Prob_df$rown = doc_topic_df$rown
  parsedData$rown = as.numeric(row.names(parsedData))
  id_topic <- merge(doc_topic_df, doc_Prob_df, by="rown")
  id_topic <- merge(id_topic, parsedData, by="rown", all.y = TRUE)
  id_topic <- subset(id_topic,select=c("rown","id","name","doc_topic","pContent","maxProb"))
  
  #문서별 토픽 번호 및 확률값 출력하기
  filePathName = paste("./result/id_topic.txt",sep="")
  write.table(id_topic, filePathName, sep="\t",row.names=FALSE,quote=FALSE)
  
  #단어별 토픽 확률값 출력하기
  posterior(lda_tm)$terms
  filePathName = paste("./result/lda_tm.csv",sep="")
  write.table(posterior(lda_tm)$terms, filePathName, sep=",", row.names=FALSE)
  
  #########################################
  ## Make visualization
  #########################################
  
  # phi는 각 단어별 토픽에 포함될 확률값 입니다.
  phi <- posterior(lda_tm)$terms %>% as.matrix
  # theta는 각 문서별 토픽에 포함될 확률값 입니다.
  theta <- posterior(lda_tm)$topics %>% as.matrix
  # vocab는 전체 단어 리스트 입니다.
  vocab <- colnames(phi)
  
  # 각 문서별 문서 길이를 구합니다.
  doc_length <- vector()
  doc_topic_df<-as.data.frame(doc_topic)
  
  for( i in as.numeric(row.names(doc_topic_df))){
    temp <- corp[[i]]$content
    doc_length <- c(doc_length, nchar(temp[1]))
  }
  
  # 각 단어별 빈도수를 구합니다.
  new_dtm_m <- as.matrix(new_dtm)
  freq_matrix <- data.frame(ST = colnames(new_dtm_m),
                            Freq = colSums(new_dtm_m))
  
  
  # 위에서 구한 값들을 파라메터 값으로 넘겨서 시각화를 하기 위한 데이터를 만들어 줍니다.
  json_lda <- createJSON(phi = phi, theta = theta,
                         vocab = vocab,
                         doc.length = doc_length,
                         term.frequency = freq_matrix$Freq)
  #mds.method = jsPCA
  #mds.method = canberraPCA)
  
  serVis(json_lda, out.dir='../lda_result', open.browser=FALSE)
  
}


# Define server logic to read selected file ----
server <- function(input, output) {
  
  #print(input$seed)
  #print(input$nTopics)
  
  observeEvent(input$do, {
    output$ldavis <- renderText("Processing...")
    output$doc_topics <- renderText("Processing...")
    
    print("why")
    
    topic_clustering(input$file1$datapath, input$seed,input$nTopics)
    id_topic_mat <- read.delim("./result/id_topic.txt",quote="",check.names=F)
	#id_topic_mat[,"doc_topic"] <- id_topic_mat[,"doc_topic"]
    url <- a("Link", href="../lda_result/index.html",target="_blank")

    
    output$ldavis <- renderUI({
     tagList("\n\n LDA result :", url)
    })
    
 
    output$doc_topics <- renderTable({
      fields <- c("name","doc_topic")
      return (id_topic_mat[,fields])
    })
    
    
    output$topicdist <- renderPlot({
      doc_topics <- as.vector(unlist(id_topic_mat[,"doc_topic"]))
      doc_topics <- table(doc_topics) 
      doc_topics <- doc_topics / sum(doc_topics)
      doc_topics <- as.data.frame(doc_topics)
      
      pie <- ggplot(doc_topics, aes(x = "", y=Freq, fill = factor(doc_topics))) + geom_bar(width = 2, stat = "identity") + theme(axis.line = element_blank(),  plot.title = element_text(hjust=0.5)) + 
        labs(fill="class", x=NULL, y=NULL, title="Topic Distribution")
      pie + coord_polar(theta = "y", start=0)
  
    },res=170)
    #output$ldavis <- renderText({NULL})
    #output$doc_topics <- renderText({NULL})
  })
  
  output$trends <- renderPlot({
    
    # 년도별 Visualization
    id_topic_mat <- read.delim("./result/id_topic.txt",quote="",check.names=F)
    input_data_mat <- read_csv("./result/input_data.csv")
    
    fields <- c("id","doc_topic")
    
    mat1 <- id_topic_mat[,fields]
    
    fields <- c("id","year")
    mat2 <- input_data_mat[,fields]
    
    result_mat <- cbind(mat1,mat2)
 
    result_years <- as.numeric(unlist(result_mat[,"year"]))
    result_topics <- as.numeric(unlist(result_mat[,"doc_topic"]))
                                
    result_df <- data.frame(result_years,result_topics)
    
    # 주제 할당이 되지 않는 경우는 제외
    idx <- which(!is.na(result_df[,"result_topics"]))
    print(idx)
    result_df <- result_df[idx,]
    #print(result_df)
    

    graph <- ggplot(result_df, aes(x = as.character(result_years), y = as.character(result_topics))) + theme(axis.text.x = element_text(angle = 60, hjust = 1))
    graph <- graph + labs(fill="Topic") + xlab("Year") + ylab("Proportion") + geom_bar(stat="identity",aes(fill=as.factor(result_topics)))
    
    
    result_df2 <- result_df
    result_df2 <- as.matrix(result_df2 %>% group_by(result_years, result_topics) %>% tally)
    u_years <- unique(result_df2[,1])
    result_df2 <- cbind(result_df2, result_df2[,3])
    colnames(result_df2)[4] <- "p"
    
    for(i in u_years)
    {
      idx <- which(result_df2[,1] == i)
      sum_n <- sum(result_df2[idx,4])
      result_df2[idx,4] <- result_df2[idx,4] / sum_n
      
    }
    
    
    result_df2 <- as.data.frame(result_df2)  
    result_df2["Topic"] <- as.character(unlist(result_df2["result_topics"] ))
    names(result_topics) = "Topics"
    
    result_topics <- as.character(as.vector(unlist(result_topics)))
    col <- as.character(result_topics)
    graph2 <- ggplot(result_df2, aes(x=result_years, y = p, group = result_topics,colour=Topic))
    graph2 <- graph2 + geom_line() + labs(fill="Topic") + xlab("Year") + ylab("Proportion")
    #dev.off()
    
    ggarrange(graph, graph2, ncol=1,nrow=2)
    
  },res=150)
  
  
  
  output$data <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    #df <- read.csv(input$file1$datapath, header = input$header,sep = input$sep, quote = input$quote
    df <- read_csv(input$file1$datapath,locale = locale(encoding = "CP949"))
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
}



