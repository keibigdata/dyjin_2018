from bs4 import BeautifulSoup
import requests
import pandas as pd
import re
import os 
import time
from datetime import date, timedelta,datetime

class NaverNewsCrawler :
    def __init__(self):
        self.idx_result_df = pd.DataFrame()
                                    
        #디렉토리 생성
        dirname = 'result'
        if ((os.path.isdir('./' + dirname + '/')) == False):
            os.mkdir('./' + dirname + '/')
        pd.set_option("display.max_colwidth", 1000)
        
    def indexing(self, start_date=None, end_date=None, append=True):

        prev_idx_result_df = None
        
        # 인덱싱 파일 읽기
        if(append):
            input_file_name = './result/indexing.txt'
            prev_idx_result_df = pd.read_csv(input_file_name,header=None,encoding="UTF8")
            prev_idx_result_df.columns = ['date','title','news','content_url_list','file_list']
        
            # 인덱싱 파일에 덧붙힐 경우 시작날짜를 지정하지 않을경우
            if(start_date == None):
                start_date = str(prev_idx_result_df.iloc[-1,0])
        
        # 시작날짜를 지정하지 않을경우 20180302
        if(start_date == None):
            start_date = '20050101'

        # 끝 날짜를 지정하지 않을경우 어제까지 수집
        if(end_date == None):
            yesterday = date.today() - timedelta(1)
            end_date = str(yesterday)
            
        # 시작날짜 및 종료날짜 설정
        dt_index = pd.date_range(start=start_date, end = end_date)
        dt_list = dt_index.strftime("%Y%m%d").tolist()
        sec = 10
        
        # 출력 파일 지정
        output_file_name = './result/indexing.txt'
        max_page = 100

        result = pd.DataFrame()
        prev_html = ""

        # d : 날짜
        # page : 페이지
        title_list = []
        content_url_list = []
        date_list = []
        file_list = []
        news_list = []
        
        for d in dt_list:  
            dt = str(datetime.strptime(d,"%Y%m%d").date())
            time.sleep(sec)
            article_num = 0
            for page in range(1,max_page):
                url = "http://news.naver.com/main/list.nhn?mode=LS2D&sid2=252&mid=shm&sid1=102"
                params = {'date': d,'page': page}
                cur_html = requests.get(url, params=params).text
           
                # 페이지 내용의 변화가 없으면 그대로 종료
                if cur_html == prev_html:
                    break
                
                prev_html = cur_html
                
                soup = BeautifulSoup(cur_html, 'lxml')
                temp_news_list = soup.select('span.writing')
                #print(temp_news_list)
                # 링크 주소
                
                idx = 0
                
                for tag in soup.select('dt > a'):      
                    link = tag['href']
    
                    title = tag.text.strip()
                   
                    if title == "":
                        continue
                    if title == "동영상기사":   
                        print("동영상기사")
                        continue
                    if "javascript" in title:
                        continue

                    file = './result/' + str(d) + "_" + str(article_num) + ".txt"
                    news = temp_news_list[idx].text
                    #print(news)
                    idx = idx + 1
                    #print(idx)
                    article_num = article_num + 1
                    #print(article_num)
                    date_list.append(dt)
                    title_list.append(title)
                    news_list.append(news)
                    content_url_list.append(link)
                    file_list.append(file)
        
        
        self.idx_result_df['date'] = date_list
        self.idx_result_df['title'] = title_list
        self.idx_result_df['news'] = news_list
        self.idx_result_df['content_url_list'] = content_url_list
        self.idx_result_df['file_list'] = file_list
        
        self.idx_result_df = pd.concat([prev_idx_result_df,self.idx_result_df],axis=0,ignore_index=True)
        self.idx_result_df.to_csv("./result/indexing.txt",header=None,index=None)
        
    def crawling_contents(self):
        url_list = self.idx_result_df['content_url_list'].tolist()
        file_path_list = self.idx_result_df['file_list'].tolist()
        title_list = self.idx_result_df['title'].tolist()
        date_list = self.idx_result_df['date'].tolist()

        sec = 10
        for i in range(len(url_list)):
            print(i)
            # 파일이 존재하고 0바이트가 아닐경우 통과
            print(file_path_list[i])
            if(os.path.isfile(file_path_list[i]) and os.path.getsize(file_path_list[i]) != 0):
                print("통과")
                continue

            if(i % 20 == 0):
                time.sleep(sec)

            cur_html = requests.get(url_list[i]).text.strip()

            soup = BeautifulSoup(cur_html,'lxml')

            try :   
                title = soup.select('#articleTitle')[0].text.strip()

                print(title)
                date = soup.select('span.t11')[0].text.strip()
                #print(date)

                content = soup.select('div #articleBodyContents')[0].text.strip()
                exc = soup.select('div #articleBodyContents script')[0].text.strip()            
                content = content[len(exc):].strip()

                #print(content)
                result = pd.DataFrame([title,date,content])
                #print(result)
                result.to_csv(file_path_list[i],header=None,index=None)

            except:
                result = pd.DataFrame([title_list[i],date_list[i],""])
                result.to_csv(file_path_list[i],header=None,index=None)
				
				
nc = NaverNewsCrawler()
nc.indexing(None,None,True)
nc.crawling_contents()

    
