install.packages("reshape2")
install.packages ("scales")
install.packages("ggplot2")
library (ggplot2)
library(reshape)
library (scales)

dncvsdkd<-read.csv ("F:/dncvsdkd.csv", sep=',', header = T)

corm1<-melt(dncvsdkd)

ggplot(data = corm1, aes(x=X, y=variable, fill=value))+ geom_tile()+scale_fill_gradient2(low = muted("green"), mid = "white",high = muted("red"), midpoint = 0, space = "Lab",na.value = "grey50", guide = "colourbar")+ theme(axis.text.x=element_text(angle=90, hjust=1))+xlab("DKD")+ylab("DNC")+ labs(fill = "Correlation")

corm1$g<-as.character(corm1$X)
corm1$g<-factor(corm1$X,levels=unique(corm1$X))

corm1$b<-as.character(corm1$variable)
corm1$b<-factor(corm1$variable,levels=unique(corm1$variable))
ggplot(data = corm1, aes(x=g, y=b, fill=value))+ geom_tile()+scale_fill_gradient2(limits=c(-1, 1),low = muted("red"), mid = "white",high = muted("green"), midpoint = 0, space = "Lab",na.value = "grey50", guide = "colourbar")+ theme(axis.text.x=element_text(angle=90, hjust=1))+xlab("DKD")+ylab("DNC")+ labs(fill = "Correlation")

################################ndkd vs dnc##############################
ndkdvsdnc<-read.csv ("F:/ndkdvsdnc.csv", sep=',', header = T)

corm2<-melt(ndkdvsdnc)

ggplot(data = corm2, aes(x=X, y=variable, fill=value))+ geom_tile()+scale_fill_gradient2(low = muted("green"), mid = "white",high = muted("red"), midpoint = 0, space = "Lab",na.value = "grey50", guide = "colourbar")+ theme(axis.text.x=element_text(angle=90, hjust=1))+xlab("NDKD")+ylab("DNC")+ labs(fill = "Correlation")

corm2$g<-as.character(corm2$X)
corm2$g<-factor(corm2$X,levels=unique(corm2$X))

corm2$b<-as.character(corm2$variable)
corm2$b<-factor(corm2$variable,levels=unique(corm2$variable))
ggplot(data = corm2, aes(x=g, y=b, fill=value))+ geom_tile()+scale_fill_gradient2(limits=c(-1, 1),low = muted("red"), mid = "white",high = muted("green"), midpoint = 0, space = "Lab",na.value = "grey50", guide = "colourbar")+ theme(axis.text.x=element_text(angle=90, hjust=1))+xlab("NDKD")+ylab("DNC")+ labs(fill = "Correlation")


