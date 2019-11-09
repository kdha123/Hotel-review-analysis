# 기술통계
data("iris")
mean(iris$Sepal.Length)

# 회귀분석
set.seed(2)
x=runif(10,0,11) # 평균은 0, 표준편차 11 연속균등분포로부터 난수 10개를 추출
y = 2+3*x+rnorm(10,0,0.2) # rnorm(10,0,0.2) 평균0, 표준편차2, 정규분포로부터 난수 10개를 뽑는다.
dfrm<- data.frame(x,y) # x,y로 데이터 프레임 생성한다.
dfrm
lm(y~x,data=dfrm) #lm(선형회귀)(종속변수~독립변수, 함수를 적용할 데이터)

# 다중선형회귀
set.seed(2)
u<-runif(10,0,11)
v<-runif(10,11,20)
w<-runif(10,1,30)
y=3+0.1*u+2*v-3*w+rnorm(10,0,0.1)
dfrm1<- data.frame(y,u,v,w)
dfrm1
m<-lm(y~u+v+w,dfrm1)
m # 따라서 도출된 회귀식은 y=2.81806+0.08733u+2.01396v-2.99561w

# 회귀모델의 적합성 검증(모델평가)
summary(m) # summary 함수를 통해서 결정계수, F통계량, 잔차의 표준오차 등 주요 통계량정보를 출력할 수 있다.

# 식이요법 방법을 적용한 닭
install.packages("MASS")
library(MASS)
chick<-ChickWeight[ChickWeight$Diet==1,] # 식이요법 방법 1을 적용한 데이터만 조회
chick
chick<-ChickWeight[ChickWeight$Chick==1,] # 1번 닭만 조회
chick
lm(weight~Time,chick)
# 회귀식은 weight = 7.988Time+24.465
m1<-lm(weight~Time,chick)
summary(m1)

# 모델 진단 그래프
data("cars")
m<-lm(dist~speed,cars)
summary(m)
plot(m) 
# Residuals vs Fitted는 y축은 잔차를 보여준다.
# Normal Q-Q는 잔차가 정규분포를 잘 따르고 있는지를 확인하는 그래프이다.
# Scale-Location은 y축이 표준화 잔차를 나타낸다. 기울기 0직선이 가장 이상적이다.
# Residuals vs Leverage는 우측 상단에 Cook's Distance가 표시되어 있다. 이것은 일반적으로 1값이넘거가면 관측치를 영향점으로 판별한다.

# 설명변수를 선택하는 방법(Step함수로)
x1<-c(7,1,11,11,7,11,3,1,2,21,1,11,10)
x2<-c(26,29,56,31,52,55,71,31,54,47,40,66,68)
x3<-c(6,15,8,8,6,9,17,22,18,4,23,9,8)
x4<-c(60,52,20,47,33,22,6,44,22,26,34,12,12)
y<-c(72.5,74.3,104.3,87.6,95.9,109.2,102.7,72.5,93.1,115.9,83.8,113.3,109.4)
df<-data.frame(x1,x2,x3,x4,y)
head(df)
# 다시 회귀분석
a<- lm(y~x1+x2+x3+x4,data=df)
summary(a)
# 유의수준이 가장높은x3을 제거하고 다시 회귀분석을 한다.
a<- lm(y~x1+x2+x4,data=df)
summary(a)
# 결정계수가 높아졌으므로 다시 유의수준이 높은 x4를 제거하고 회귀분석한다.
a<- lm(y~x1+x2,data=df)
summary(a)
# 최종 회귀식은 49.98608+1.44616x1+0.70991x2로 추정된다.

# 전진 선택법
step(lm(y~1,df),scope = list(lower=~1,upper=~x1+x2+x3+x4),direction = "forward")
# step(lm(종속변수~설명변수, 데이터셋),scope = list(lower=~1,upper=~설명변수),direction = "변수선택방법")
# scope분석할 때 고려할 변수의 범위를 정한다. '1'은 상수항을 의미
# direction은 변수선택 방법이다. 가능한 옵션은 'forward','backward','both'가 있다.
# 변수제거 없는 회귀분석부터 시작하며 AIC가 감소기준으로 종속변수에 가장 큰 영향을 주는 독립변수부터 포함시키며 더 이상 추가될 독립변수가
# 없을 때 중단하고 분석을 시작한다.