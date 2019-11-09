# 산점도 그래프
math<-c(95,65,80,92,60,75,88,100,75,68)
scie<-c(90,70,80,95,65,70,85,95,70,60)
plot(math,scie)

# 산점도 행렬
pairs(iris[1:4], main = "Anderson's Iris Data -- 3 species", pch = 21, bg = c("red","green3","blue")[unclass(iris$Species)])

# 히스토그램과 박스플롯
height<-c(182,160,165,170,163,160,181,166,159,145,175)
hist(height)
boxplot(height)

# reshape 활용
install.packages("reshape")
library(reshape)
install.packages("reshape2")
library(reshape2)
data(airquality)
colnames(airquality)<-tolower(colnames(airquality))
head(airquality)
names(airquality)<-tolower(names((airquality))) # 변수명을 소문자로 변환하는 것
T<- melt(airquality,id = c("month","day"),na.rm = TRUE)
T # melt id에 있는 변수를 기준으로 나머지 변수를 variable이란 이름의 데이터로 만든다. 또한 원래 변숫값을 이 value에 저장해 모든 데이터를 변환한다.
cast(T,day~month~variable) # 행을 day, 열을 month로 각 변수들을 새롭게 배열
b<- acast(T,month~variable,mean)
b # 각 변수들의 month 평균
d<- cast(T,month~variable,mean,margins = c("grand_row","grand_col"))
d # margin 관련 옵션으로, 행과 열에 대한 합계를 산출하는 기능
e<-cast(T,day~month,mean,subset=variable=="ozone")
e #subset 기능을 이용해 특정 변수(ozone)만을 처리하도록 한다.
f<- cast(T,month~variable,range)
f # range 기능은 min은 x1 이라는 변수를 max는 x2라는 변수명을 끝에 붙여준다.

# sqldf를 이용한 분석
install.packages("sqldf")
library(sqldf)
data(iris)
sqldf("select*from iris") # ""안에 sql 조회할 내용을 표현한다.
sqldf("select*from iris limit 10") # 10행까지만 조회
sqldf("select*from iris where species like 'se%'") # se로 시작되는 붓꽃의 종류

# plyr 
set.seed(1) # 난수를 생성할 때마다 같은 값의 난수들을 생성
d<-data.frame(year = rep(2012:2014, each = 6),count = round(runif(9,0,20)))
d
# 2012~2014년 각각 6개씩 0~20사이 정수 9개를 저장한다.
install.packages("plyr")
library(plyr)
ddply(data, variables, fun=NULL) # 데이터 프레임을 분할하고 함수를 적용한 뒤 결과를 데이터 프레임으로 반환한다.
ddply(d,"year",function(x){mean.count=mean(x$count)
sd.count=sd(x$count)
cv=sd.count/mean.count
data.frame(cv.count=cv)})
ddply(d,.(year),summarise,mean.cout=mean(count)) # summarise():데이터의 요약 정보를 새로운 변수에 만드는 함수
ddply(d,.(year),transform,total.count=sum(count)) # trasform():연산 결과를 데이터 프레임의 새로운 칼럼에 저장하는 함수

#데이터 테이블
DT<-data.table(x=c("b","b","b","a","c"),v=rnorm(5))
DT