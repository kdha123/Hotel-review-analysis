# 앙상블 모형
# 배깅
install.packages("adabag")
library(adabag)
library(rpart)
data("iris")
set.seed(1)
train<- c(sample(1:50,25),sample(51:100,25), sample(101:150,25)) # species에서 각각 25개 sampling한다.
iris.bagging<-bagging(Species~.,data = iris[train, ],mfinal = 10,control = rpart.control(maxdepth = 1)) # 배깅은 복원추출이 중요
iris.bagging # 10번 반복 결과 보여준다.
iris.bagging$importance # 가장 큰 기여를 하는 변수는 petal.length 그리고 petal.width이다.
barplot(iris.bagging$importance[order(iris.bagging$importance,decreasing = TRUE)],ylim = c(0,100),main = "Variabled Relative Importance")
table(iris.bagging$class, iris$Species[train],dnn = c("PredictedClass","Observed Class"))
1-sum(iris.bagging$class==iris$Species[train])/length(iris$Species[train]) # error는 1.3%

# 부스팅
iris.adaboost<-boosting(Species~.,data = iris[train,],mfinal = 10, control=rpart.control(maxdepth = 1))
iris.adaboost
# importance에서 약간의 차이가 있고 weight은 배깅과 다르다.
barplot(iris.adaboost$importance[order(iris.adaboost$importance,decreasing = TRUE)],ylim = c(0,100),main = "Variabled Relative Importance",col = "red")
table(iris.adaboost$class, iris$Species[train],dnn = c("PredictedClass","Observed Class"))
# 결과는 배깅과 같다.

# 랜덤포레스트
install.packages("randomForest")
library(randomForest)
data("stagec")
stegec3<-stagec[complete.cases(stagec),]
# complete.cases() 함수로 결측치가 있는지 확인
# 데이터 분할 방법
# nrow(stagec3) : stagec3의 행 개수 그러므로 1,2라는 범주를 샘플링
# replace=TRUE 복원추출, prob=(0.7,0.3): 범주1을 70%, 범주2를 30% 뽑음
set.seed(1234)
ind<-sample(2,nrow(stegec3),replace = TRUE,prob = c(0.7,0.3))
# 랜덤 포레스트는 데이터의 일부분을 복원추출로 꺼내고 해당 데이터에 대해서만 의사결정나무를 만든다.
train<- stegec3[ind==1,]
test<-stegec3[ind==2,]
rf<- randomForest(ploidy~.,data=train,ntree = 100,proximity = T, importance = T)
# proximity 분석 대상 자료 간의 유사도를 산정한 값
table(predict(rf),train$ploidy)
print(rf) # confusion matrix OOB : 모델 훈련에 사용되지 않은 데이터를 사용한 에러 추정치 3.92%
varImpPlot(rf)
# 정확성은 g2가 제일 높고 불순도 개선 기여도 역시 g2가 높다.

#SVM 서포트 벡터 머신
library(e1071)
s<-sample(150,100) # 150중 100 샘플링
col<-c("Petal.Length","Petal.Width","Species") # 컬럼명 지정
iris_train<-iris[s,col]
iris_test<-iris[-s,col]
# linear kernel 방식으로 modeling
iris_svm<-svm(Species~.,data=iris_train,cost=1,kernel="linear")
# cost: 커널 파라미터, kernel : 커널 함수
plot(iris_svm,iris_train[,col])
# svm train vs test 결과예측
p<-predict(iris_svm,iris_test[,col],type="class")
plot(p)
table(p,iris_test[,3])

# 나이브 베이즈 분류모형
colnames(iris)<-tolower(colnames(iris))
m<-naiveBayes(species~.,data = iris)
m
table(predict(m,iris[,-5]),iris[,5],dnn=list('predicted','actual'))

# ROC그래프
# infert data를 신경망 모형과 의상결정 모형을 통해 모형평가를 한다.
set.seed(1234)
str(infert)
infert<-infert[sample(nrow(infert )),]
infert<-infert[,c("age","parity","induced","spontaneous","case")]
traindata<-infert[1:(nrow(infert)*0.7),]
testdata<- infert[(nrow(infert)*0.7+1):nrow(infert),]
install.packages("neuralnet")
library(neuralnet)
net.infert<-neuralnet(case~age+parity+induced+spontaneous,data=traindata,hidden=3,err.fct = "ce",linear.output = F,likelihood = T)
n_test<-subset(testdata,select= -case)
nn_pred<-compute(net.infert,n_test) #model estimate value
testdata$net_pred<-nn_pred$net.result
head(testdata)

# decision tree
install.packages("C50")
library(C50)
traindata$case<-factor(traindata$case) # decision tree only use categorical variable for target
dt.infert<-C5.0(case~age+parity+induced+spontaneous,data=traindata)
testdata$dt_pred<-predict(dt.infert,testdata, type = "prob")
head(testdata)
install.packages("Epi")
library(Epi)
dtree_ROC<-ROC(form = case~dt_pred,data=testdata,plot="ROC")
neural_ROC<-ROC(form = case~net_pred,data=testdata,plot="ROC")
# AUC(area under the curve : 곡선 밑에 구역)이 신경망 0.723이고 의사결정나무가 0.661이기때문에 신경망 모형의 분류분석 모옇이 더 높은 성과를 보여줌
# 이 두개의 그래프를 겹치게 하려면 par(new=TRUE)를 이용한다.

# 신경망 모형의 향상도 곡선
n_lift<-performance(n_r,"lift","rpp")
plot(n_lift,col='red')
abline(v=0.2) # 신경망 모형의 경우 상위 20%의 집단에 대하여 랜덤 모델과 비교할 때 2배의 성과 향상을 보인다.
