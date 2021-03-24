# 네이버 영화 사이트 크롤링
# movie.naver.com
# 1.1 왼쪽 사이드 메뉴에서 영화랭킹 선택 후 조회순 랭킹 1~50위에 대하여 제목과 세부링크 url을 크롤링
## 최종 데이터 데이터 프레임으로 구성

# 1.2 각 영화 링크로 접근하여 네티즌 관람객 평점과 기자 평론가 평점을 크롤링하여
# 1번에서 작업한 내용과 모두 결합하여 영화랭킹.csv 파일로 저장

# 수업과제2 
# 2.1 과제 1에서 크롤링한 각 영화의 url를 사용하여 영화의 리뷰를 수집한다
# 2.2 과제 1에서 크롤링한 영화의 url를 사용하여 모든 영화의 리뷰를 수집한다
# 예 ) 리뷰.csv

# 수업과제3
# 3개를 선택해서 영화의 한줄평을 최대한 수집한다 


# 1.1

url <- 'https://movie.naver.com/movie/sdb/rank/rmovie.nhn'
selector <-'#old_content > table > tbody > tr > td.title > div > a'
base_url <- 'https://movie.naver.com/'

rank_html <- read_html(url)
rank_node <- html_nodes(rank_html,selector)
rank_text <- html_text(rank_node)
rank_text <- gsub('\r|\n|\t','',rank_text) %>% str_trim()
rank_text

length(rank_node)

rank_node[50]

rank_link <- html_attr(rank_node,'href') ; 
rank_link <-paste0(base_url,rank_link)
rank_link

rank_df <- data.frame(rank_text,rank_link)
View(rank_df)




people_selector <- '#content > div.article > div.section_group.section_group_frst > div:nth-child(5) > div:nth-child(2) > div.score_area > div.netizen_score > div > div > em'
reporter_selector <-'#content > div.article > div.section_group.section_group_frst > div:nth-child(5) > div:nth-child(2) > div.score_area > div.special_score > div > div > em'

review_url <- rank_link[49] ; review_url
review_html <- read_html(review_url) ;review_html
review_node <- html_nodes(review_html,people_selector)
review_text <- html_text(review_node) ;review_text

people_review <-c()
reporter_review <- c()
for(i in 1:50) {
  review_url <- rank_link[i]
  review_html <- read_html(review_url)
  people_review_node <- html_nodes(review_html,people_selector)
  people_review_text <- html_text(people_review_node)
  people_review <- c(people_review, people_review_text)
  reporter_review_node <-  html_nodes(review_html,reporter_selector)
  reporter_review_text <- html_text(reporter_review_node)
  reporter_review <- c(reporter_review, reporter_review_text)
}

length(people_review)
length(reporter_review)

review_df <- data.frame(people_review,reporter_review)
View(review_df)

df <- data.frame(review_df,rank_df)
View(df)

write.csv(df,'Movie Ranking.csv')



# 과제 3 -----------------------------------------
library(rvest)
library(httr)
library(RSelenium)
library(seleniumPipes)
library(stringr)

# 페이지 접속
remDr <- remoteDriver(remoteServerAddr='localhost', port =4445L, browserName='chrome')
remDr$open()
remDr$navigate(url)

# 기생충, 반지의제왕 왕의귀환, 극장판 귀멸의 칼날

# 해당 영화 페이지 이동
parasite <- subset(df$rank_link,df$rank_text=='기생충')
remDr$navigate(parasite)

# 더보기 클릭
# <a href="./point.nhn?code=161967#tab" class="link_more">더보기<span class="ico_more"></span></a>
selector <- '#content > div.article > div.section_group.section_group_frst > div:nth-child(5) > div:nth-child(2) > a'
more <- remDr$findElement(using ='css', selector )
more$clickElement()

# 1페이지 한줄평 하나 크롤링 해오기
frame <- remDr$findElements('css',"#pointAfterListIframe") # Elements
print(frame) 
remDr$switchToFrame(frame[[1]])
s <- '#_filtered_ment_0'
raw <- remDr$getPageSource()[[1]]
line_html <- read_html(raw)
line_node <- html_node(line_html, s); line_node
line_text <- html_text(line_node); line_text
line_text <- gsub('\t|\n','',line_text);line_text %>%   
  str_replace_all('[[:punct:]]','') %>% 
  str_trim()

# 1페이지 한줄평 전체 크롤링 해오기
line_text_final <- c()
for(i in 1:9) {
  s <- paste0('#_filtered_ment_',i)
  line_node <- html_node(line_html, s)
  line_text <- html_text(line_node)
  line_text <- gsub('\t|\n|\u314b','',line_text);line_text %>%   
    str_replace_all('[[:punct:]]','') %>% 
    str_trim()
  line_text_final <- c(line_text,line_text_final)
}


# 2페이지 버튼 누르기 
button <- remDr$findElement('css','#pagerTagAnchor2')
button$clickElement()

# 여러 페이지 크롤링 진행
line_text_final <- c()
frame <- remDr$findElements('css',"#pointAfterListIframe")
print(frame)
remDr$switchToFrame(frame[[1]])
for (i in 1:10) {
  button_s <- paste0('#pagerTagAnchor',i)
  button <- remDr$findElement('css',button_s)
  button$clickElement()
  raw <- remDr$getPageSource()[[1]]
  line_html <- read_html(raw)
  for(n in 1:9) {
    s <- paste0('#_filtered_ment_',n)
    line_node <- html_node(line_html, s)
    line_text <- html_text(line_node)
    line_text <- gsub('\t|\n|\u314b','',line_text);line_text %>%   
                 str_replace_all('[[:punct:]]','') %>% 
                 str_trim()
  line_text_final <- c(line_text,line_text_final)
  }
}

write.csv(line_text_final,'parasite.csv')


# 최종코드 -----------------------------------------------------
url <- 'https://movie.naver.com/movie/sdb/rank/rmovie.nhn'
remDr <- remoteDriver(remoteServerAddr='localhost', port =4445L, browserName='chrome')
remDr$open()
remDr$navigate(url)



line_text_final <- c()
movie <- c('기생충','극장판 귀멸의 칼날: 무한열차편','반지의 제왕: 두 개의 탑')
for (x in 1:length(movie)) {
  movie_link <- subset(df$rank_link,df$rank_text==movie[x])
  remDr$navigate(movie_link)
  selector <- '#content > div.article > div.section_group.section_group_frst > div:nth-child(5) > div:nth-child(2) > a'
  more <- remDr$findElement(using ='css', selector )
  more$clickElement()
  frame <- remDr$findElements('css',"#pointAfterListIframe")
  print(frame) 
  remDr$switchToFrame(frame[[1]])
  for (i in 1:10) {
    button_s <- paste0('#pagerTagAnchor',i)
    button <- remDr$findElement('css',button_s)
    button$clickElement()
    raw <- remDr$getPageSource()[[1]]
    line_html <- read_html(raw)
    for(n in 1:9) {
      s <- paste0('#_filtered_ment_',n)
      line_node <- html_node(line_html, s)
      line_text <- html_text(line_node)
      line_text <- gsub('\t|\n|\u314b','',line_text);line_text %>%   
        str_replace_all('[[:punct:]]','') %>% 
        str_trim()
      line_text_final <- c(line_text,line_text_final)
    }
  }
  name <- paste0('Final_',gsub(':| ','',movie[x]),'.csv')
  write.csv(line_text_final,name)
}
