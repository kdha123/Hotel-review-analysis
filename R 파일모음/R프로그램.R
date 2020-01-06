#파이차트
x<-c(9,15,20,6)
label<-c("영업1팀","영업2팀","영업3팀","영업4팀")
pie(x,labels=label,main="부서명 영업 실적")
pie(x,init.angle=90,labels=label,main="부서별 영업 실적")

pct<-round(x/sum(x)*100)
label<-paste(label,pct)
label<-paste(label,"%",sep="")
pie(x,labels=label,init.angle=90,col=rainbow(length(x)),main="부서별 영업 실적")
pie(x,labels=label,init.angle=90,col=heat.colors(length(x)),main="부서별 영업 실적")
pie(x,labels=label,init.angle=90,col=terrain.colors(length(x)),main="부서별 영업 실적")
pie(x,labels=label,init.angle=90,col=topo.colors(length(x)),main="부서별 영업 실적")
pie(x,labels=label,init.angle=90,col=cm.colors(length(x)),main="부서별 영업 실적")
pie(x,labels=label,init.angle=90,col=c("blue","green","pink","red"),main="부서별 영업 실적")

#3D 파이차트
library(plotrix)
pie3D(x,labels=label,explode=0,labelcex=0.9,main="부서별 영업 실적")

#바차트
height<-c(9,15,20,6)
name<-c("영업1팀","영업2팀","영업3팀","영업4팀")
barplot(height,names.arg = name,main="부서별 영업 실적",col=rainbow(length(height)))
barplot(height,names.arg = name,main="부서별 영업 실적",col=rainbow(length(height)),xlab="부서",ylab="영업 실적(억 원)",ylim=c(0,25))
text(x=bp,y=height,labels=round(height,0),pos=3)
barplot(height,names.arg = name,main="부서별 영업 실적",col=rainbow(length(height)),xlab="영업 실적(억 원)",ylab="부서",xlim=c(0,25),horiz=TRUE,width=50)

#그룹화된 바 차트
height1<-c(4,18,5,8)
height2<-c(9,15,20,6)
height<-rbind(height1,height2)
name<-c("영업1팀","영업2팀","영업3팀","영업4팀")
legend_lbl<-c("2014년","2015년")
barplot(height,main="부서별 영업 실적",names.arg=name,xlab="부서",ylab="영업 실적(억 원)",col=c("blue","skyblue"),legend.text=legend_lbl,ylim=c(0,35))
barplot(height,main="부서별 영업 실적",names.arg=name,xlab="영업 실적(억 원)",ylab="부서",col=c("blue","skyblue"),legend.text=legend_lbl,ylim=c(0,40),xlim=c(0,40),beside=TRUE,horiz=TRUE,width=2.8)
