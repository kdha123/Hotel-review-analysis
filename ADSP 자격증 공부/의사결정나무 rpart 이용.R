# 의사결정나무
colnames(iris)<-tolower(colnames(iris))
install.packages("rpart")
library(rpart)
k<-rpart(species~.,data=iris)
k
plot(k,compress=T,margin = 3)
text(k,cex=1.0)
# rpart.plot() 함수를 통해 다양한 시각화 제공
# type=4를 지정해 모든 노드에 레이블, extra=2를 지정해 각 노드에서의 관측값과 각 노드에서 올바르게 예측한 데이터의 비율 출력
install.packages("rpart.plot")
library(rpart.plot)
prp(k,type=4,extra=2,digits = 3) # Petal.Length<2.45인 경우에 붓꽃의 종이 setosa로 예측되며, 이에 해당하는 데이터는 50개 모두이다.
# 예측 predict()를 통해 수행하고 comfusionMatrix 함수를 이용하여 정확성을 평가한다.
head(predict(k,newdata=iris,type = "class"))
printcp(k) # error제일 낮은 부분 찾기
plotcp(k)

install.packages("caret")
install.packages("e1071")
library(caret)
library(e1071)
rpartpred<- predict(k,iris,type = "class")
confusionMatrix(rpartpred,iris$species) #predict 함수를 사용해서 iris 셋의 species를 예측한 후, confusionMatrix함수를 사용해서 모델의 정확성을 평가


