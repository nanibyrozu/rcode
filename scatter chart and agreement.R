library(openxlsx)
library (BlandAltmanLeh)
install.packages ("agRee")
library(agRee)

library(psych)

ad=read.xlsx("F:\\E\\NSS\\MAHARASTRA.xlsx",sheet=1)
ad1 <- na.omit(ad)


mx<-max(ad1$NIN_WT)
my<-max(ad1$AWW_WT)

x1<-ad1$NIN_WT
y1<-ad1$AWW_WT-1.0
dev.off()

plot(x = ad$NIN_WT,y = ad$AWW_WT,
     xlab = "NIN_Weight (kg)",
     ylab = "AWW_Weight (kg)",
     xlim = c(0,25),
     ylim = c(0,25),		 
     main = "Agreement between the Weights tabken by NIN and AWW"
)
abline(coef = c(0,1), lwd=2)
abline(coef=c(.5,1),col="red", lwd=2)
abline(coef=c(-.5,1),col="red",lwd=2)
text(15, 2, "Red line indicate ±0.5kg difference", srt=0.1, col = "blue")



plot(x = ad$NIN_WT,y = ad$AWW_WT,
     xlab = "NIN_Weight (kg)",
     ylab = "AWW_Weight (kg)",
     xlim = c(0,25),
     ylim = c(0,25),		 
     main = "Agreement between the Weights tabken by NIN and AWW"
)
abline(coef = c(0,1), lwd=2)
abline(coef=c(.1,1),col="red", lwd=2)
abline(coef=c(-.1,1),col="red",lwd=2)
text(15, 2, "Red line indicate ±0.1kg difference", srt=0.1, col = "blue")



plot(x = ad$NIN_WT,y = ad$AWW_WT,
     xlab = "NIN_Weight (kg)",
     ylab = "AWW_Weight (kg)",
     xlim = c(0,25),
     ylim = c(0,25),		 
     main = "Agreement between the Weights tabken by NIN and AWW"
)
abline(coef = c(0,1), lwd=2)
abline(coef=c(.2,1),col="red", lwd=2)
abline(coef=c(-.2,1),col="red",lwd=2)
text(15, 2, "Red line indicate ±0.2kg difference", srt=0.1, col = "blue")




plot(x = ad$NIN_WT,y = ad$AWW_WT,
     xlab = "NIN_Weight (kg)",
     ylab = "AWW_Weight (kg)",
     xlim = c(0,25),
     ylim = c(0,25),		 
     main = "Agreement between the Weights tabken by NIN and AWW"
)
abline(coef = c(0,1), lwd=2)
abline(coef=c(1,1),col="red", lwd=2)
abline(coef=c(-1,1),col="red",lwd=2)
text(15, 2, "Red line indicate ±1kg difference", srt=0.1, col = "blue")



scatter.with.hist(ad$NIN_WT,ad$AWW_WT)

scatter.hist(ad$NIN_WT,ad$AWW_WT, xlab="NIN_WT", ylab="AWW_WT", title="Aggrement between NIN-WT and AWW-WT")

scatter.hist(ad$NIN_HT,ad$AWW_HT, xlab="NIN_HT", ylab="AWW_HT", title="Aggrement between NIN-HT and AWW-HT")



a<-ad1$NIN_WT
b<-ad1$AWW_WT
bland.altman.plot(ad1$NIN_WT,ad1$AWW_WT, xlab="mean", ylab="difference")
bland.altman.plot(a,b, xlab="mean weight", ylab="difference in weights", main="Bland-Altman Plot for Weight",conf.int=.95)

#gree.plot(ad1$NIN_Wt,ad1$AWW_Wt)

mx<-max(ad1$NIN_HT)
my<-max(ad1$AWW_HT)


plot(x = ad$NIN_HT,y = ad$AWW_HT,
     xlab = "NIN_Height (cm)",
     ylab = "AWW_Height (cm)",
     xlim = c(0,120),
     ylim = c(0,120),		 
     main = "Agreement between the Heights tabken by NIN and AWW"
)
abline(coef = c(0,1),col="black",lwd=2)
abline(coef=c(.2,1),col="red", lwd=2)
abline(coef=c(-.2,1),col="red",lwd=2)
text(75, 15, "Red line indicate ±0.2cm difference", srt=0.1, col = "blue")





plot(x = ad$NIN_HT,y = ad$AWW_HT,
     xlab = "NIN_Height (cm)",
     ylab = "AWW_Height (cm)",
     xlim = c(0,120),
     ylim = c(0,120),		 
     main = "Agreement between the Heights tabken by NIN and AWW"
)
abline(coef = c(0,1),col="black",lwd=2)
abline(coef=c(.5,1),col="red", lwd=2)
abline(coef=c(-.5,1),col="red",lwd=2)
text(75, 15, "Red line indicate ±0.5cm difference", srt=0.1, col = "blue")



plot(x = ad$NIN_HT,y = ad$AWW_HT,
     xlab = "NIN_Height (cm)",
     ylab = "AWW_Height (cm)",
     xlim = c(0,120),
     ylim = c(0,120),		 
     main = "Agreement between the Heights tabken by NIN and AWW"
)
abline(coef = c(0,1),col="black",lwd=2)
abline(coef=c(1,1),col="red", lwd=2)
abline(coef=c(-1,1),col="red",lwd=2)
text(75, 15, "Red line indicate ±1cm difference", srt=0.1, col = "blue")







a<-ad1$NIN_HT
b<-ad1$AWW_HT

bland.altman.plot(a,b, xlab="mean height", ylab="difference in height", main="Bland-Altman Plot for Height",conf.int=.95)



addmargins(table(ad$wt_0))
addmargins(table(ad$wt_100))
addmargins (table(ad$wt_200))
addmargins (table (ad$wt_500))
addmargins (table (ad$wt_1kg))

prop.table(table(ad$wt_0))
prop.table(table(ad$wt_100))
prop.table (table(ad$wt_200))
prop.table (table (ad$wt_500))
prop.table (table (ad$wt_1kg))



addmargins(table(ad$ht_0))
addmargins (table(ad$ht_p2cm))
addmargins (table (ad$ht_p5cm))
addmargins (table (ad$ht_1cm))

prop.table(table(ad$ht_0))
prop.table (table(ad$ht_p2cm))
prop.table (table (ad$ht_p5cm))
prop.table (table (ad$ht_1cm))



#################
DATA <- as.data.frame(cbind(a,b,c))
subset <-  DATA[complete.cases(DATA), ] 
##############################



da=read.xlsx("F:\\E\\NSS\\data2.xlsx",sheet=1)


mx<-max(da$CDFNORM)
my<-max(da$Abramowitz)


plot(x = da$CDFNORM,y = da$Proposed,
     xlab = "CDFNORM",
     ylab = "Proposed Algorithm",
     xlim = c(0,1),
     ylim = c(0,1),		 
     main = "Agreement between the CDFNORM funtion and Proposed Algorithm"
)
abline(coef = c(0,1),col="black",lwd=2)
abline(coef=c(0.01,1),col="black", lty=2, lwd=1)
abline(coef=c(-0.01,1),col="black",lty=2,lwd=1)
text(0.5, 0.1, "Dotted line indicate ±0.01 difference", srt=0.1, col = "black")

a<-da$CDFNORM
b<-da$Abramowitz

bland.altman.plot(a,b, xlab="mean ", ylab="difference", main="Bland-Altman Plot",conf.int=.95)

########################

table(ad$Weight_difference)
table(ad$Height_difference)
prop.table(table(ad$Weight_difference))
prop.table(table(ad$Height_difference))

