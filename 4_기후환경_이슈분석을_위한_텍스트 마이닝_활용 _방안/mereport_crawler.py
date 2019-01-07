
# coding: utf-8

# In[7]:


# Me Report crawler by Daeyong Jin

# Import library
from itertools import count
from urllib.parse import urljoin
import requests
import pandas as pd
import numpy as np
from bs4 import BeautifulSoup
import re
import os 
import time
import datetime

class MeReportCrawler :
    
        def __init__(self):
            self.idx_result_df = pd.DataFrame()
        
            #디렉토리 생성
            dirname = 'result'
            if ((os.path.isdir('./' + dirname + '/')) == False):
                os.mkdir('./' + dirname + '/')

            # 환경부 E-News 크롤링 및 파싱 (코퍼스 구축)
        
        def indexing(self, start_date=None, end_date=None, append=True):
            prev_idx_result_df = None
               
            # 인덱싱 파일 읽기
            if(append):
                input_file_name = './result/indexing.txt'
                prev_idx_result_df = pd.read_csv(input_file_name,header=None)
                prev_idx_result_df.columns = ['date','title','content_url_list','file_list']
                
                # 인덱싱 파일에 덧붙힐 경우 시작날짜를 지정하지 않을경우
                if(start_date == None):
                    start_date = str(prev_idx_result_df.iloc[-1,0])
                    
            # 시작날짜를 지정하지 않을경우 20050101
            if(start_date == None):
                start_date = '2005-01-01'

            # 끝 날짜를 지정하지 않을경우 어제까지 수집
            if(end_date == None):
                yesterday = datetime.date.today() - datetime.timedelta(1)
                end_date = str(yesterday)       
           
            print(start_date)
            print(end_date)
            
            # E-Report 페이지 리스트
            url = 'http://www.me.go.kr/home/web/board/list.do'
            params = {'maxIndexPages': 3000, 'boardMasterId': 1, 'menuId': 286, 'condition.fromDate' : str(start_date) , 'condition.toDate' : str(end_date), 'boardCategoryId' : 39 }

            first_html = requests.get(url, params=params).text
            soup = BeautifulSoup(first_html, 'html.parser')
        
            page_url_list = []
            for tag in soup.select('span.page_num a'):
                page_url_list.append('http://www.me.go.kr' + tag['href'])
            #print(page_url_list)
            
            # 글이 없거나 1페이지가 안될경우
            if(len(page_url_list) == 0) :
                page_url_list.append("http://www.me.go.kr/home/web/board/list.do?menuId=286&boardMasterId=1&boardCategoryId=39")
               

            # 페이지 별로 돌아가면서 내용 주소 크롤링
            title_list = []
            content_url_list = []
            date_list = []
            file_name_list = []

            for page_url in page_url_list:
                list_html = requests.get(page_url, params=params).text
                soup = BeautifulSoup(list_html, 'html.parser')
                for post in soup.select('td.al a'):
                    content_url_list.append('http://www.me.go.kr' + post['href'])
                    title_list.append(post.text)
                for d in soup.select('table > tbody > tr > td:nth-of-type(5)'):
                    print(d.text.strip())
                    date_list.append(d.text.strip())

            # 파일 이름 저장
            date_list = np.array(date_list)

            u_date_list = list(set(date_list))
            u_date_list.sort(reverse=True)

            for u_date in u_date_list:
                sub_date_list = date_list[date_list == u_date]
                num_list = list(range(1, sub_date_list.size + 1))

                for num in num_list:
                    u_date = str(u_date)
                    u_date = "".join(u_date.split("-"))
                    file_name = u_date + "_" + str(num)
                    file_name = "./result/" + file_name + ".txt"
                    file_name_list.append(file_name)

            self.idx_result_df['date'] = list(date_list)
            self.idx_result_df['title'] = title_list
            self.idx_result_df['content_url_list'] = content_url_list
            self.idx_result_df['file_list'] = file_name_list
            self.idx_result_df = pd.concat([prev_idx_result_df,self.idx_result_df],axis=0,ignore_index=True)
            self.idx_result_df = self.idx_result_df.drop_duplicates("file_list")
            self.idx_result_df = self.idx_result_df.sort_values(by=["date"])
            self.idx_result_df = self.idx_result_df.reset_index(drop = True)
            self.idx_result_df.to_csv("./result/indexing.txt",header=None,index=None)

            
        def crawling_report(self):
            file_path_list = self.idx_result_df['file_list'].tolist()
            title_list = self.idx_result_df['title'].tolist()
            date_list = self.idx_result_df['date'].tolist()
            url_list =  self.idx_result_df['content_url_list'].tolist()

            
            for i in range(len(url_list)):
                print(i)
                # 파일이 존재하고 0바이트가 아닐경우 통과
                if(os.path.isfile(file_path_list[i]) and os.path.getsize(file_path_list[i]) != 0):
                    print("통과")
                    continue
                
                content_url = url_list[i]
                content_html = requests.get(content_url).text
    
                try : 
                    soup = BeautifulSoup(content_html, 'lxml')
                    title = soup.select('#boardTableWrapper > header')[0].text.strip()
                    print(title)
                    date = soup.select('#boardViewListForRead_0 > ul > li > dl > dd')[0].text.strip()
                    print(date)
                    content = soup.select('#boardTableWrapper > div.view_con')[0].text.strip()
      
                    result = pd.DataFrame([title,date,content])
                    result.to_csv(file_path_list[i],header=None,index=None)
               
                except :
                    result = pd.DataFrame([title,date,""])
                    result.to_csv(file_path_list[i],header=None,index=None)
                
                

# print(result_list)

#result_df = pd.DataFrame(result,columns=['file_name','title','date','content_url'])
#result_df.to_csv("./result/indexing.txt",sep="\t")
#print("종료")


# In[ ]:


mnc = MeReportCrawler()
mnc.indexing(None,None,True)
mnc.crawling_report()

