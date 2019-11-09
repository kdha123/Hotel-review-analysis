# 백터 
x<-c(1,10,20)
y<-c("사과","배","복숭아")
xy<-c(x,y)
xy

# 행렬
m<-matrix(c(1,2,3,4,5,6),ncol=2)
m
# 행렬 생성 함수:matrix(data,nrow,ncol,byrow=FALSE,dimnames=NULL)

# 리스트
x<-list(name="kimdonghyeon",heigh = 182)
x

# 데이터 프레임
a1<-c(100,200,300)
b1<-c("a","b","c")
c1<-c(FALSE,FALSE,FALSE)
D<-data.frame(incom=a1,car=b1,marriage=c1)
D

# 배열(행렬은 2차원이고 배열은 다차원 데이터)
array(1:12,dim = c(3,4))
(x<-array(1:12,dim=c(2,2,3)))

# 외부 데이터 불러오기
read.csv("D:\\DATA\\traffic_death.csv",header = T)
read.table("D:\\DATA\\traffic_death.csv",header = T,Sep = ";")

#R의 기초함수
# 수열 생성하기
rep(1,3)
rep(2:5,3)
seq(1,3)
seq(1,11,by=2)
seq(1,11,length=7)

# 기초적인 행렬계산
a<-c(2,7,3)
t(a)
z<-a%*%t(a)
z

# 역행렬 계산
A<-matrix(c(23,41,12,35,67,1,24,53,7),nrow=3)
A
5*A
solve(A)

# 기초적인 대표값 및 분산 계산
c<-1:10
mean(c)
var(c)
sd(c)

# 기초적인 변환 및 상관계수 공분산
sum(c)
median(C)
log(C)

a<-1:10
b<-log(C)
cov(a,c)
cor(a,c)
summary(a)

# R 데이터 핸들링
# 백터형 변수
b<-c("a","b","c")
b[2]
b[-3]
b[c(1,2)]

# 반복구문과 조건문
a<-c()
for(i in 1:9)
  {a[i] = i*i}
a

x<-1
while(x<5) 
{x = x +1
print(x)}

ifelse(x>0,"양수","양수아님")

#사용자 정의 함수
foruse<- function(a)
{isum<-0
for(i in 1:a){
  isum = isum+i
}
print(isum)}
foruse(3)

# 기타 유용한 기능들
number<-1:5
alphabet<-c("a","b","c")
paste(number, alphabet)
paste(number,alphabet,sep = "to the")

country<-c("korea","Japan")
substr(country,1,2)

as.data.frame(x) #데이터 프레임 형식으로 변환
as.list(x) #리스트 형식으로 변환
as.matrix(x) #행렬 형식으로 변환
as.vector(x) #벡터 형식으로 변환
as.factor(x) #factor 형식으로 변환
as.integer(3.14) # 실수형 벡터를 정수형 벡터로 변환
as.numeric("foo") #문자형 데이터를 수치형 벡터로 강제 변환
as.numeric(FALSE) #논리값인 TRUE와 FALSE를 수치형으로 변환할 때는 0,1로 바꾼다.
as.logical(0.45)
as.Date("2019-05-08")
as.Date("05/08/2019",format="%m/%d/%Y")
format(Sys.Date())
