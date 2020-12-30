install.packages("reshape2")
install.packages ("scales")
library (ggplot2)
library(reshape)
library (scales)


write.csv( mat35, file="f:/mat.csv")
mat35.m=melt(mat35)
ggplot(data = mat35.m, aes(x=X1, y=X2, fill=value))+ geom_tile()+scale_fill_gradient2(low = muted("green"), mid = "white",high = muted("red"), midpoint = 0, space = "Lab",na.value = "grey50", guide = "colourbar")+ theme(axis.text.x=element_text(angle=90, hjust=1))+xlab("Girls")+ylab("Boys")


p<-ggplot(data = mat35.m, aes(x=X1, y=X2, fill=value)) + geom_tile()+scale_fill_gradient2(low = muted("red"), mid = "white",high = muted("green"), midpoint = 0, space = "Lab",na.value = "grey50", guide = "colourbar")+ theme(axis.text.x=element_text(angle=90, hjust=1))
p+labs(x="Girls")+labs(y="Boys")+ labs(fill = "Correlation")
?muted

###############for my correlation data##########################

vit_data<-read.csv ("F:/vitbs.csv", sep=',', header = T)

save(vit_data, file="vitb.Rdata")
load("vitb.Rdata")

corm<-melt(vit_data)

ggplot(data = corm, aes(x=X, y=variable, fill=value))+ geom_tile()+scale_fill_gradient2(low = muted("green"), mid = "white",high = muted("red"), midpoint = 0, space = "Lab",na.value = "grey50", guide = "colourbar")+ theme(axis.text.x=element_text(angle=90, hjust=1))+xlab("Girls")+ylab("Boys")+ labs(fill = "Correlation")

corm$g<-as.character(corm$X)
corm$g<-factor(corm$X,levels=unique(corm$X))

corm$b<-as.character(corm$variable)
corm$b<-factor(corm$variable,levels=unique(corm$variable))
ggplot(data = corm, aes(x=g, y=b, fill=value))+ geom_tile()+scale_fill_gradient2(limits=c(-1, 1),low = muted("red"), mid = "white",high = muted("green"), midpoint = 0, space = "Lab",na.value = "grey50", guide = "colourbar")+ theme(axis.text.x=element_text(angle=90, hjust=1))+xlab("Girls")+ylab("Boys")+ labs(fill = "Correlation")



