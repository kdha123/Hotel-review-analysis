#패키지 다운로드
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("KoNLP")

#패키지 준비
library(wordcloud)
library(RColorBrewer)
library(KoNLP)

#워드클라우드 시험해보기
word<-c("인천광역시","강화군","옹진군")

frequency<-c(651,85,61)

wordcloud(word,frequency,colors="blue")

wordcloud(word,frequency,random.order=F,random.color=F,colors = rainbow(length(word)))

pal2<-brewer.pal(8,"Dark2")

word<-c("인천광역시","강화군","옹진군")

frequency<-c(651,85,61)

wordcloud(word,frequency,colors=pal2)

#팔레트 유형
display.brewer.all(type="div")

#저장한 폴더 읽기
pal2<-brewer.pal(8,"Dark2")

data<-read.csv(file.choose(),header = T)

head(data)

#'전국'지역 통계 삭제
data2<-data[data$행정구역.시군구.별!="전국",]

head(data2)

#'구'단위 지역 통계 삭제
x<-grep("구",data2$행정구역.시군구.별)

data3<-data2[-c(x),]

head(data3)

#순이동 인구수(전출보다 전입 인구수)가 많은 지역
data4<-data3[data3$순이동.명.>0,]

word<-data4$행정구역.시군구.별

frequency<-data4$순이동.명

wordcloud(word,frequency,colors = pal2)

#순이동 인구수(전입보다 전출 인구수)가 많은 지역
data5<-data3[data3$순이동.명<0,]

word<-data5$행정구역.시군구.별

frequency<-abs(data5$순이동.명)

wordcloud(word,frequency,colors=pal2)

#연설문 워드 클라우드 만들기
useSejongDic()

pal2<-brewer.pal(8,"Dark2")

text<-readLines(file.choose())
text

noun<-sapply(text,extractNoun,USE.NAMES = F)
noun

noun2<- unlist(noun)
noun2

word_count<-table(noun2)
word_count

head(sort(word_count,decreasing = TRUE), 10)
wordcloud(names(word_count),freq=word_count,scale=c(6,0.3),min.freq = 3,random.order = F,rot.per = .1,colors = pal2)

#단어 추가/삭제 및 두 글자 이상 단어의 빈도만 출력하기
mergeUserDic(data.frame(c("정치"),C("ncn")))

noun<-sapply(text.extractNoun.USE.NAMES=F)

noun2<-unlist(noun)
noun2<-gsub("여러분","",noun2)
noun2<-gsub("우리","",noun2)
noun2<-gsub("오늘","",noun2)
noun2<-Filter(function(x){nchar(x)>=2},noun2)

word_count<-table(noun2)
wordcloud(names(word_count),freq=word_count,scale=c(6,0.3),min.freq = 3,random.order = F,rot.per = .1,colors = pal2)

#김대중 연설문 취임사
pal2<-brewer.pal(8,"Set1")

text<-readLines(file.choose())
text

noun<-sapply(text,extractNoun,USE.NAMES = F)
noun

noun2<- unlist(noun)
noun2

word_count<-table(noun2)
word_count
wordcloud(names(word_count),freq=word_count,scale=c(6,0.3),min.freq = 3,random.order = F,rot.per = .1,colors = pal2)

noun2<-gsub("여러분","",noun2)
noun2<-gsub("우리","",noun2)
noun2<-gsub("오늘세","",noun2)
noun2<-gsub("존경하는","",noun2)
noun2<-gsub("저는","",noun2)
noun2<-Filter(function(x){nchar(x)>=2},noun2)

word_count<-table(noun2)
wordcloud(names(word_count),freq=word_count,scale=c(6,0.3),min.freq = 3,random.order = F,rot.per = .1,colors = pal2)

#공공데이터 활용
#네이버 오픈 API 활용
install.packages("RCurl")
install.packages("XML")
library(RCurl)
library(XML)

#인증기 설정
searchUrl<-"https://openapi.naver.com/v1/search/blog.xml"
Client_ID<-"j3BGeY1QytOUnxWDPTfE"
Client_Secret<-"sBRNLbS7OL"

#블로그 검색
query<-URLencode(iconv("여름 추천 요리","euc-kr","UTF-8"))

url<-paste(searchUrl,"?query=",query,"&display=20",sep="")
doc<-getURL(url,httpheader=c('Content-Type'="application/xml",'X-Naver-Client-Id'=Client_ID,'X-Naver-Client-Secret'=Client_Secret))

#블로그 내용에 대한 리스트 만들기
doc2<-htmlParse(doc,encoding="UTF-8")

text<-xpathSApply(doc2,"//item/description",xmlValue)
text

#워드 클라우드 만들기, 패키지준비시키고 세종사전 코딩
useSejongDic()

noun<-sapply(text,extractNoun,USE.NAMES = F)
noun

noun2<-unlist(noun)
noun2

word_count<-table(noun2)
word_count

palete<-brewer.pal(9,"Set1")
wordcloud(names(word_count),freq=word_count,scale=c(5,0.25),rot.per=0.25,min.freq=1,random.order=F,random.color=T,colors=palete)

warnings()


#필터링하고, 워드클라우드 만들기
noun2<-Filter(function(x){nchar(x)>=2},noun2)
noun2

noun2<-gsub('￦￦d+','',noun2)
noun2<-gsub('<b>','',noun2)
noun2<-gsub('</b>','',noun2)
noun2<-gsub('&amp','',noun2)
noun2<-gsub('&lt','',noun2)
noun2<-gsub('&gt','',noun2)
noun2<-gsub('&quot','',noun2)
noun2<-gsub("",'',noun2)
noun2<-gsub('￦','',noun2)
noun2<-gsub('','',noun2)
noun2<-gsub('-','',noun2)
noun2

word_count<-table(noun2)
head(sort(word_count,decreasing = TRUE),30)
palete<-brewer.pal(9,"Set1")
wordcloud(names(word_count),freq=word_count,scale=c(5,0.25),rot.per=0.25,min.freq=1,random.order=F,random.color=T,colors=palete)

#텍스트마이닝한것 파이차트로 만들기
a<-head(sort(word_count,decreasing = TRUE),30)
pie(a,col=rainbow(10),radius = 1)

pct<-round(a/sum(a)*100,1)
names(a)

lab<-paste(names(a),"\n",pct,"%")
pie(a,main="여름추천요리",col=topo.colors(10),cex=0.8,labels=lab)

#네이버 뉴스 API실습과제(R스크립트, 워드클라우드 필터링 전후 저장한것, 파이차트로 만든것)
searchUrl<-"https://openapi.naver.com/v1/search/news.xml"
Client_ID<-"j3BGeY1QytOUnxWDPTfE"
Client_Secret<-"sBRNLbS7OL"

query<-URLencode(iconv("인공지능","euc-kr","UTF-8"))
url<-paste(searchUrl,"?query=",query,"&display=20",sep="")
doc<-getURL(url,httpheader=c('Content-Type'="application/xml",'X-Naver-Client-Id'=Client_ID,'X-Naver-Client-Secret'=Client_Secret))
doc2<-htmlParse(doc,encoding="UTF-8")

text<-xpathSApply(doc2,"//item/description",xmlValue)
text

noun<-sapply(text,extractNoun,USE.NAMES = F)
noun
noun2<-unlist(noun)
noun2
word_count<-table(noun2)
word_count

palete<-brewer.pal(9,"Set1")
wordcloud(names(word_count),freq=word_count,scale=c(5,0.25),rot.per=0.25,min.freq=1,random.order=F,random.color=T,colors=palete)

noun2<-Filter(function(x){nchar(x)>=2},noun2)
noun2
noun2<-gsub('￦￦d+','',noun2)
noun2<-gsub('<b>','',noun2)
noun2<-gsub('</b>','',noun2)
noun2<-gsub('&amp','',noun2)
noun2<-gsub('&lt','',noun2)
noun2<-gsub('&gt','',noun2)
noun2<-gsub('&quot','',noun2)
noun2<-gsub("",'',noun2)
noun2<-gsub('￦','',noun2)
noun2<-gsub('','',noun2)
noun2<-gsub('-','',noun2)
noun2<-gsub("매력",'',noun2)
noun2<-gsub("안전",'',noun2)
noun2<-gsub("계산",'',noun2)
noun2

word_count<-table(noun2)
head(sort(word_count,decreasing = TRUE),30)
palete<-brewer.pal(9,"Set1")
wordcloud(names(word_count),freq=word_count,scale=c(5,0.25),rot.per=0.25,min.freq=2,random.order=F,random.color=T,colors=palete)

a<-head(sort(word_count,decreasing = TRUE),30)
pie(a,col=topo.colors(10),radius = 1)

pct<-round(a/sum(a)*100,1)
names(a)

lab<-paste(names(a),"\n",pct,"%")
pie(a,main="인공지능",col=topo.colors(10),cex=0.8,labels=lab)

#웹 스크래핑
install.packages("XML")
library(XML)

#웹 스크래핑 상품명 출
url<- "http://www.coupang.com/np/search?component=&q=%EC%97%AC%EC%84%B1%ED%81%AC%EB%A1%9C%EC%8A%A4%EB%B0%B1"

web_page<-readLines(url)
doc<-htmlParse(web_page,encoding = "UTF-8")

prod_name<-xpathSApply(doc,"//ul[@id='productList']//div[@class='name']",xmlValue)
prod_name

prod_name<-gsub('￦n','',prod_name)
prod_name<-gsub('','',prod_name)
prod_name

#추출한 상품 가격 출력
price<-xpathSApply(doc,"//ul[@id='productList']//strong[@class='price-value']",xmlValue)
price
#데이터 프레임 만들기
df<-data.frame(상품명=prod_name,가격=price)
df

df$상품명<-format(df$상품명,justify="left")
df

#여러 페이지 웹 스크래핑
url<- "http://www.coupang.com/np/search?q=여성크로스백&channel=user&component=&eventCategory=SRP&trcid=&traid=&sorter=scoreDesc&minPrice=&maxPrice=&priceRange=&filterType=&listSize=36&filter=&isPriceRange=false&brand=&offerCondition=&rating=0&page="

df.products<- NULL

for(page in 1:10) {
url2<-paste(url,page,sep="") 
web_page<-readLines(url2)
doc<-htmlParse(web_page,encoding = "UTF-8")

prod_name<-xpathSApply(doc,"//ul[@id='productList']//div[@class='name']",xmlValue)
prod_name<-gsub('￦n','',prod_name)
prod_name<-gsub('','',prod_name)
prod_name<-gsub('\t','',prod_name)

price<-xpathSApply(doc,"//ul[@id='productList']//strong[@class='price-value']",xmlValue)

df<-data.frame(상품명=prod_name,가격=price)
df.products<-rbind(df.products,df)
}

df.products

df.products$상품명<-format(df.products$상품명,justify="left")
df.products

#12장 네트워크 분석 
install.packages("igraph")
library(igraph)

g_star<-graph(edges=NULL,n=NULL,directed=FALSE)

g_star<-g_star+vertex("A",shape="circle",size=40,color="green")
g_star<-g_star+vertices("B","C","D","E","F",shape="circle",size=30)
g_star<- g_star+edge("A","B")
g_star<-g_star+edges("A","C","A","D","A","E","A","F")
plot(g_star)

vcount(g_star)
ecount(g_star)

#Y자형 네트워크 만들기
g_Y<-graph(edges = NULL,n=NULL,directed = FALSE)
g_Y<-g_Y+vertices("A","B","C","D","E","F",shape="circle",size=30)
g_Y<-g_Y+edge("A","B","A","C","A","D","D","E","E","F")
plot(g_Y)

#원형 네트워크 만들기
g_ring<-graph(edges = NULL,n=NULL,directed = FALSE)
g_ring<-g_ring+vertex("A",shape="square",size=40,color="green")
g_ring<-g_ring+vertices("B","C","D","E","F",shape="circle",size=30)
g_ring<-g_ring+edges("A","B","B","C","C","D","D","E","E","F","F","A")
plot(g_ring)

#연결정도 파악하기
degree(g_star,normalized = FALSE)
degree(g_star,normalized = TRUE)
tmax<-centr_degree_tmax(g_star)
centralization.degree(g_star,normalized = FALSE)$centralization/tmax

degree(g_Y,normalized = FALSE)
degree(g_Y,normalized = TRUE)
tmax<-centr_degree_tmax(g_Y)
centralization.degree(g_Y,normalized = FALSE)$centralization/tmax

degree(g_ring,normalized = FALSE)
degree(g_ring,normalized = TRUE)
tmax<-centr_degree_tmax(g_ring)
centralization.degree(g_ring,normalized = FALSE)$centralization/tmax

#근접정도 파악하기
closeness(g_star,normalized = FALSE)
closeness(g_star,normalized = TRUE)
tmax<-centralization.closeness.tmax(g_star)
centralization.closeness(g_star,normalized = FALSE)$centralization/tmax

closeness(g_Y,normalized = FALSE)
closeness(g_Y,normalized = TRUE)
tmax<-centralization.closeness.tmax(g_Y)
centralization.closeness(g_Y,normalized = FALSE)$centralization/tmax

closeness(g_ring,normalized = FALSE)
closeness(g_ring,normalized = TRUE)
tmax<-centralization.closeness.tmax(g_ring)
centralization.closeness(g_ring,normalized = FALSE)$centralization/tmax

#중개 정도
betweenness(g_star,normalized = FALSE)
betweenness(g_star,normalized = TRUE)
tmax<-centralization.closeness.tmax(g_star)
centralization.betweenness(g_star,normalized = FALSE)$centralization/tmax

betweenness(g_Y,normalized = FALSE)
betweenness(g_Y,normalized = TRUE)
tmax<-centralization.closeness.tmax(g_Y)
centralization.betweenness(g_Y,normalized = FALSE)$centralization/tmax

betweenness(g_ring,normalized = FALSE)
betweenness(g_ring,normalized = TRUE)
tmax<-centralization.closeness.tmax(g_ring)
centralization.betweenness(g_ring,normalized = FALSE)$centralization/tmax
