View(winequality.white)
attach(winequality.white)
summary(winequality.white[1:12,])


pairs(winequality.white[1:12,])

winequality.white.pca<-princomp(winequality.white[1:12,])
summary(winequality.white.pca)
loadings(winequality.white.pca)

winequality.white.pca<-princomp(winequality.white[1:12,],cor=TRUE)
summary(winequality.white.pca)
loadings(winequality.white.pca)

predict(winequality.white.pca)

plot(winequality.white.pca)





##### ggscree.R #####
library(ggplot2)
ggscreeplot<-function(mydata,cor=F,maxcomp=10) {
  my.pc<-princomp(mydata, cor=cor)
  k<-min(dim(mydata),maxcomp)
  x<-c(0:k)
  y<-my.pc$sdev[1:k]*my.pc$sdev[1:k]
  y<-c(0,y)
  z<-100*cumsum(y)/sum(my.pc$sdev*my.pc$sdev)
  
  p<-ggplot(mapping=aes(x,z))
  p<-p+xlab("number of dimensions")+ylab("cumulative percentage of total variance")
  p<-p+ggtitle("Scree plot of variances")
  p<-p+geom_segment(aes(x=0,y=100,xend=k,yend=100),colour="orange",linetype="dashed",size=1)
  p<-p+ylim(c(0,100))+scale_y_continuous(breaks=c(0,20,40,60,80,100))
  p<-p+geom_text(aes(label=x),colour="blue",nudge_y=2)
  p+geom_line(colour="red",size=1)
}


ggscreeplot(winequality.white[1:12,],cor=TRUE)



library(MASS)

biplot(winequality.white.pca)



########################

View(winequality.red)
attach(winequality.red)
summary(winequality.red[1:12,])


pairs(winequality.red[1:12,])

winequality.red.pca<-princomp(winequality.red[1:12,])
summary(winequality.red.pca)
loadings(winequality.red.pca)

winequality.red.pca<-princomp(winequality.red[1:12,],cor=TRUE)
summary(winequality.red.pca)
loadings(winequality.red.pca)

predict(winequality.red.pca)

plot(winequality.red.pca)



##### ggscree.R #####
library(ggplot2)
ggscreeplot<-function(mydata,cor=F,maxcomp=10) {
  my.pc<-princomp(mydata, cor=cor)
  k<-min(dim(mydata),maxcomp)
  x<-c(0:k)
  y<-my.pc$sdev[1:k]*my.pc$sdev[1:k]
  y<-c(0,y)
  z<-100*cumsum(y)/sum(my.pc$sdev*my.pc$sdev)
  
  p<-ggplot(mapping=aes(x,z))
  p<-p+xlab("number of dimensions")+ylab("cumulative percentage of total variance")
  p<-p+ggtitle("Scree plot of variances")
  p<-p+geom_segment(aes(x=0,y=100,xend=k,yend=100),colour="orange",linetype="dashed",size=1)
  p<-p+ylim(c(0,100))+scale_y_continuous(breaks=c(0,20,40,60,80,100))
  p<-p+geom_text(aes(label=x),colour="blue",nudge_y=2)
  p+geom_line(colour="red",size=1)
}


ggscreeplot(winequality.red[1:12,],cor=TRUE)



library(MASS)


biplot(winequality.red.pca)









