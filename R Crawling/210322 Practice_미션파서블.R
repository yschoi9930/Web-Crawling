# 영화평점 형태소 분석

library(KoNLP)
library(stringr)
library(wordcloud2)
library(rvest)
library(httr)
library(RSelenium)
library(seleniumPipes)

# 리뷰 데이터 추출
url <- 'https://movie.naver.com/movie/sdb/rank/rmovie.nhn'
base_url <- 'https://movie.naver.com'
remDr <- remoteDriver(remoteServerAddr='localhost', port =4445L, browserName='chrome')
remDr$open()
remDr$navigate(url)

selector <-'#old_content > table > tbody > tr > td.title > div > a'
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

line_text_final <- c()
line_text_star_final<-c()
movie <- c('미션 파서블')

for (x in 1:length(movie)) {
  movie_link <- subset(rank_df$rank_link,rank_df$rank_text==movie[x])
  remDr$navigate(movie_link)
  selector <- '#content > div.article > div.section_group.section_group_frst > div:nth-child(5) > div:nth-child(2) > a'
  more <- remDr$findElement(using ='css', selector )
  more$clickElement()
  frame <- remDr$findElements('css',"#pointAfterListIframe")
  remDr$switchToFrame(frame[[1]])
  for (i in 1:100) {
    button_s <- paste0('#pagerTagAnchor',i)
    button <- remDr$findElement('css',button_s)
    button$clickElement()
    raw <- remDr$getPageSource()[[1]]
    line_html <- read_html(raw)
    for(n in 0:9) {
      s <- paste0('#_filtered_ment_',n)
      s_star <- paste0('body > div > div > div.score_result > ul > li:nth-child(',n+1,') > div.star_score > em')  
      line_node <- html_node(line_html, s)
      line_node_star <- html_node(line_html, s_star)
      line_text <- html_text(line_node)
      line_text_star <- html_text(line_node_star)
      line_text <- gsub('\t|\n|\u314b','',line_text) %>%   
        str_replace_all('[[:punct:]]','') %>% # 수정필요
        str_trim()
      line_text_final <- c(line_text,line_text_final)
      line_text_star_final<-c(line_text_star,line_text_star_final)
    }
  }
  name <- paste0('Final_',gsub(':| ','',movie[x]),'.csv')
  final_df <- data.frame(line_text_star_final,line_text_final)
  write.csv(final_df,name)
}

p_review <- read.csv('Final_미션파서블.csv')
p_review

# 데이터 정제

score <- p_review[,2]
score
text<- p_review[,3]
text

# 위 코드로 정리되지 않은 데이터 정제
text <- gsub('[[:punct:]]','',text)
text <- gsub('\u314e|\u3160|\u3147|\u3161|\u3139|\u315c|\u3137','',text)

# 형태소 분석
data_list<-list()
for (i in 1: length(text)) {
  if(class(try(ss <- SimplePos09(text[i])))=='try-error'){
    data_list[[i]] <- NA
  } else {
    data_list[[i]] <-ss
  } 
cat('\n',i)
}

# 형태소 분석 리스트 정제

split <- function(x){
  sapply(str_split(x,'/'),function(x){x[1]})  
}

data <- sapply(data_list,split)
head(data)

data_v<- unlist(data)
head(data,30)     

# dtm 생성
tbl <- table(data_v)
words<- names(sort(tbl, decreasing = TRUE))[1:300]

dtm <- matrix(0, ncol=length(words),nrow=length(text))
colnames(dtm) <- words
'head(dtm)'

words %in% data[[1]]
dtm[1,words %in% data[[1]]]<-1
dtm[1,]

for (i in 1:length(data)){
  dtm[i, words %in% data[[i]]]<-1
}

# 평점 긍정 부정 분류
score_fin <- ifelse(score>5,1,0)
score_fin %>% head()

# terms가 있는 행만 분류
d_index <- apply(dtm,1,sum) !=0
dtm_fin <- dtm[d_index,]
head(dtm_fin)
dim(dtm_fin)

score_fin <- score_fin[d_index]
length(score_fin)

# train data와 test 데이터 분류
train <- sample(1:length(score_fin),length(score_fin)*0.7)
head(dtm_fin[train,]) # train
head(dtm_fin[-train,]) # test
score_fin[train]

library(glmnet)
# alpha = 1, lasso
fit <- glmnet(dtm_fin[train,],score_fin[train],alpha=1,family='binomial',nlambda = 100)
s <- fit$lambda[length(fit$lambda)]
coef_data <- coef(fit,s=s)


predict <- predict(fit,dtm_fin[-train,],s=s)
predict

predict_2 <- ifelse(predict>0,1,0)
length(predict_2)
sum(predict_2 == score_fin[-train]) / length(predict_2) # 0.820339

# alpha = 0, ridge
fit_r <- glmnet(dtm_fin[train,],score_fin[train],alpha=0,family='binomial',nlambda = 100)
s_r <- fit_r$lambda[length(fit_r$lambda)]
coef_data_r <- coef(fit_r,s=s_r)


predict_r <- predict(fit_r,dtm_fin[-train,],s=s_r)
predict_r

predict_r2 <- ifelse(predict_r>0,1,0)
length(predict_r2)
sum(predict_r2 == score_fin[-train]) / length(predict_r2) # 0.8542373

# 계수 확인
coef_data <- as.matrix(coef_data)

sort(coef_data[,1],decreasing = TRUE)[1:30]
sort(coef_data[,1],decreasing = FALSE)[1:30]   
