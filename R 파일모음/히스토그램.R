data(women)
women

weight<-women$weight
plot(weight)

height<-women$height
plot(height,weight,xlab="키",ylab="몸무게",pch="?",col="blue",bg="green",cex=1.5)

data(quakes)
head(quakes)

mag<-quakes$mag
mag

colors<-c("red","orange","yellow","green","blue","navy","violet")
hist(mag,main="지진 발생 강도의 분포",xlab="지진강도",ylab="상대도수",col=topo.colors(7),breaks=seq(4,6.5,by=0.5),freq=FALSE)

lines(density(mag))

min(mag)
max(mag)
median(mag)
quantile(mag,c(0.25,0.5,0.75))
boxplot(mag,main="지진 발생 강도의 분포",xlab="지진",ylab="발생건수",col="skyblue")
