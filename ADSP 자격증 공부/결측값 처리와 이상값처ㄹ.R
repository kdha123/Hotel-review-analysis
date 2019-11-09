# 데이터 기초 통계
data(iris)
str(iris)
summary(iris)
cov(iris[,1:4])#각 변수 간의 공분산을 확인한다.
cor(iris[,1:4])#각 변수 간의 상관계수을 확인한다.

#결측값 처리
y<-c(1,2,3,NA)
is.na(y) # 결측값 여부를 확인
mydata[mydata$v1==99,"v1"]<-NA #mydata 데이터의 v1 변수 내에 99를 결측값 처리한다.
#결측값으로 인한 문제를 해결하는 방법으로 해당 값을 제외하는 방법
x<-c(1,2,NA,3)
mean(x)
mean(x,na.rm=T)
french_fries[!complete.cases(french_fries),]# french_fries 데이터중에 complete.cases함수를 이요 결측값확인

#Amelia 패키지를 이용한 결측값 imputation
install.packages("Amelia")
library(Amelia)
data("freetrade")
head(freetrade)
a.out<-amelia(freetrade,m=5,ts="year",cs='country') #m=imputation데이터셋 수
hist(a.out$imputations[[3]]$tariff,col="grey",border = "white") # 3번째 imputation 데이터셋 적용된 tariff변수를 히스토그램으로 나타낸다.
save(a.out,file="imputations.RData") # imputation된 데이터셋 저장
write.amelia(obj=a.out,file.stem="outdata") # write.amelia 명령어를 사용하여 그 자신의 파일에 전가된 데이터셋 각각을 저장
missmap(a.out) # 결측값 처리하기 전의 그래프
freetrade$tariff<-a.out$imputations[[5]]$tariff # imputation 값을 데이터셋 사용
missmap(freetrade)

# 이상값 검색
x<- rnorm(100)
boxplot(x)
x<-c(x,19,28,30)
outwith=boxplot(x)

#outlier 패키지 이용
install.packages("outliers")
library(outliers)
set.seed(1234)
y=rnorm(100)
outlier(y) #평균과 가장 차이가 많이 나는 값 출력
outlier(y,opposite =TRUE) # 반대 방향으로 가장 차이가 많이 나는 값 출력
dim(y)<-c(20,5)
outlier(y) # 각 열의 평균과 가장 차이가 많은 값을 각 열별로 출력
outlier(y,opposite = TRUE)
boxplot(y)
