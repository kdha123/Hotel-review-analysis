# 다차원 척도법
loc<-cmdscale(eurodist)
loc
x<-loc[,1] # 2차원 값을 각각 x,y좌표에 배정
y<-loc[,2]
plot(x,y,type="n",main="eurodist") # type=n 그래프 유형으로 아무것도 그리지 않음.
text(x,y,rownames(loc),cex=.8,col="red") # text()그래프에 문자열을 그리는데 사용
abline(v=0,h=0) # abline()그래프에 직선을 추가할 때 사용

# 주성분 분석
library(datasets)
data("USArrests")
head(USArrests)
fit <- princomp(USArrests,cor=TRUE) # 주성분분석 함수 princomp(), cor=TRUE 상관계쑤 행렬을 사용
summary(fit) # 첫번째 주성분 분석하나가 전체 분산의 62%를 설명하고 있다.
loadings(fit) # 주성분들의 로딩 벡터를 나타낸다. Y1=-0.536Murder-0.583Assault-0.278UrbanPop-0.543Rape
plot(fit,type="lines")
biplot(fit)

# 시계열 분석
Nile # 나일강의 연간 유입량
plot(Nile)
# 비정상시계열 자료이기 때문에 diff()함수로 차분을 한다.
Nile.diff1<-diff(Nile,differences = 1)
plot(Nile.diff1)
# 차분한 결과로 아직 평균이 일정하지 않아 차분을 2번한다.
Nile.diff2<-diff(Nile,differences = 2)
plot(Nile.diff2)

# 영국 내의 월별 폐질환 사망자에 관한 시열자료
plot(ldeaths)
# 연도별로 계절성을 띠고 있다. decompose() 함수를 사용하면 시계열 자료를 4가지 요인으로 분해할 수 있다.
ldeaths.decompose<-decompose(ldeaths)
ldeaths.decompose$seasonal
plot(ldeaths.decompose)
# 계절성을 띠는 시계열 자료는 계절요인을 추정해 그 값을 원시계열 자료에서 뺴면 적절하게 조정할 수 있다.
ldeaths.decompose.adj<-ldeaths-ldeaths.decompose$seasonal
plot(ldeaths.decompose.adj)

# ARIMA 모형을 적합한 후에 최종 모형을 결정하기 acf함수를 사용하여 2차 차분한 나일강 시계열 자료 활용
acf(Nile.diff2,lag.max = 20)
acf(Nile.diff2,lag.max = 20,plot=FALSE)
# lag =1,8을 제외하고 모두 신뢰구간에 있는 것을 알 수 있다.
# 다음으로 부분자기상관함수 pacf()를 이용
pacf(Nile.diff2,lag.max = 20)
pacf(Nile.diff2,lag.max = 20,plot = FALSE)

install.packages("forecast")
library(forecast)
auto.arima(Nile)
# ARIMA(1,1,1) 모형으로 결정
# ARIMA 모형을 이용한 예측
Nile.arima<- arima(Nile,order=c(1,1,1)) # 데이터에 모형을 적합한 후 forecast 함수를 이용하여 미래의 수치값을 예측
Nile.arima
Nile.forecast<-forecast(Nile.arima,h=50) 
Nile.forecast
