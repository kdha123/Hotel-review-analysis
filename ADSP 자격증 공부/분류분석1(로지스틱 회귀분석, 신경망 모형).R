x<-c(27.2,27.7,28.3,28.4,29.9) 
male<-c(2,17,26,19,27)
female<-c(25,7,4,8,1)
total<- male+female
pmale<-male/total
pmale
# 온도를 독립변수로 수컷 비율을 종속변수로 하여 단순선형회귀식을 추정하시오
z<-lm(pmale~x)
summary(z) # 추정회귀식은 -6.9020+0.2673*온도
p<- coefficients(z)[1]+coefficients(z)[2]*x
p # 온도에 따른 수컷 비율 예측 결과값이 1보다 큰 값이 존재한다.

# 로직변환된 값을 종속변수로 한 단순선형회귀식을 추정하시오.
logit<-log(pmale/(1-pmale))
z1<-lm(logit~x)
summary(z1)
# 예측값 수컷비 예측값 출력
logit2<- coefficients(z1)[1]+coefficients(z1)[2]*x
remalehat<- exp(logit2)/(1+exp(logit2))
remalehat # 로짓변환하여 온도별 예측 확률값은 0~1사이에 값을 갖는다.

#최대 우도추정법을 사용하여 회귀식 추정
logit<-glm(pmale~x,family = "binomial", weights = total)
summary(logit) # 추정회귀식은 수컷비율= -61.3183+2.2110*x 즉,27.3도에서 암컷과 수컷을 구분짓는 경계값이 된다.
exp(-61.3183)*exp(2.2111*27)
exp(-61.3183)*exp(2.2111*28)
# 즉 28도에서 예측값은 27도보다 exp(2.2111)=9.125배이다. 따라서 28도에서 암컷에서 수컷으로 부화될 가능성은 0.2*9.125=1.825가 된다고 한다.

colnames(iris)<-tolower(colnames(iris))
a<-subset(iris,species =="setosa"|species=="versicolor")
# 로지스틱 회귀를 하기위해 범주가 2개인 setosa=1과 versicolor=2만 추출한다.
# 로지스틱이 적용될 때보다 큰 숫자인 versicolor일 때 오즈를 모형화하므로 해석에 유의해야한다.
a$species<-factor(a$species) # 2개 레벨을 가진 새로운 factor형(범주형)

b<-glm(species~sepal.length,data=a,family = binomial)
summary(b)
# sepal.length p-value 유의수준보다 낮아 매우 유의한 변수이다.
coef(b)
exp(coef(b))["sepal.length"] # 로지스틱 회귀계수 값은 약 170개가 된다.
fitted(b)[c(1:3,98:100)]
# 로지스틱 회귀모델은 0 또는 1로 값을 예측하는 모델이다. 적합된 값을 통해 0.5이하면 'setosa',0.5 이상이면 'versicolor' 예측값을 의미
predict(b,newdata = a[c(1,50,51,100),],type = "response") # 종류를 예측할 수 있다.
cdplot(species~sepal.length,data=a) # 길이가 길어짐에 따라 versicolor의 확률이 증가함을 보여준다.

# 다항 로지스틱 회귀분석(32종류의 자동차에 대해 11개 변수값을 측정한 자료)
attach(mtcars)
str(mtcars)

# vs(0:flat engine, 1:straight engine)을 반응변수로 mpg+am(Transmission:automatic=0,manual=1)을 예측변수로 하는 로지스틱 회귀모형 추정
glm.vs<-glm(vs~mpg+am,data = mtcars,family = "binomial")
summary(glm.vs)
# 해석 : am이 주어질 때 mpg 값이 한 단위 증가할 때 vs=1 오즈가 exp(0.6809)=1.98 증가한다.
# mpg이 주어질 때 오즈에 대한 am의 효과는 exp(-3.0073)=0.05배, 즉 변속기가 수동인 경우 자동에 비해 vs=1의 오즈가 95% 감소한다.
anova(glm.vs,test = "Chisq")

# 로지스틱 회귀분석에도 상관계수와 유사한 개념으로 확인할 수 있다
install.packages("pscl")
library(pscl)
pR2(glm.vs) # 상관계수값이 0.69인 것으로 보아 모델이 데이터셋의 분산의 약 69.1%를 설명하고 있다.

# iris 자료에 대한 신경망 모형 분석
install.packages("nnet")
library(nnet)
data<-iris
Scale<-data.frame(lapply(data[,1:4],function(x) scale(x)))
Scale$species<-data$species
index<-sample(1:nrow(Scale),round(0.75*nrow(Scale)),replace = FALSE)
# 신경망 모형 적합 전 scaling(정규화)가 필요하다.
train<-Scale[index,]
test<-Scale[-index,] # train, test데이터 분리
model<-nnet(species~.,size=2,decay=5e-04,data=train)
# summary 함수를 이용하여 신경망 모델의 결과를 확인할 수 있다
summary(model)
# 노드의 수와 가중치의 수를 보여준 후 가중치의 값을 보여준다.
# weights의 수 = (input nod+1)*hidden nod+(hidden nod+1)*output nod

# 신경망 모델의 검증
predict.model<-predict(model,test[,1:4],type="class")
predict.model
# 오분류표(모델성능검증표)를 만들기 위해서 table 함수를 이용하여 표를 만든다.
actual<-test$species
confusion.matrix<- table(actual,predict.model)
confusion.matrix
# 정확도검증
accuracy<-sum(diag(confusion.matrix))/sum(confusion.matrix)
accuracy
sum(predict.model==actual)/NROW(predict.model)
# 92.1% 정확도를 나타낸다.