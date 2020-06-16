library(ggplot2)
library(dplyr) # easier data wrangling 
library(viridis) # colour blind friendly palette, works in B&W also
library(Interpol.T) #  will generate a large dataset on initial load
library(lubridate) # for easy date manipulation
library(ggExtra) # because remembering ggplot theme options is beyond me
library(tidyr) 




##################nani  chart##############
data2<-read.csv("D:\\Work2020\\2020\\table3.csv",sep=',',header = T)
bmi<-ordered (data2$BMI, levels=c(1,2,3,4), labels=c("CED   (<18.5)",	"Normal (18.5-22.9)",	"Overweight(23-27.49)",	"Obese (>=27.5)"))
ffm<-ordered (data2$FFM, levels=c(1,2), labels =c("Low FFM", "Normal")) 
whr<-ordered (data2$WHR, levels=c(1,2), labels=c("Normal",	"Abdominal adiposity"))
htn<-data2$HTN
dm<-data2$DM

df2<-data.frame(bmi, ffm, whr, htn)
df2

p2 <-ggplot(df2,aes(bmi,ffm,fill=htn))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="HTN (%)",option ="C")
p2
p2 <-p2 + facet_grid(whr)

p2
p2 <-p2 + labs(title= "Prevalence of Hypertension", x="BMI", y="FFM")
p2

p2 <-p2 + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()#ggExtra
p2

##############################################
df3<-data.frame(bmi, ffm, whr, dm)
df3

p3 <-ggplot(df3,aes(bmi,ffm,fill=dm))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="Diabetes (%)",option ="C")
p3
p3 <-p3 + facet_grid(whr)

p3
p3 <-p3 + labs(title= "Prevalence of Diabetes", x="BMI", y="FFM")
p3

p3 <-p3 + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()#ggExtra
p3

##################
Data4<-read.csv("D:\\Work2020\\2020\\table2.csv",sep=',',header = T)
bmi<-ordered (data4$BMI, levels=c(1,2,3,4), labels=c("CED   (<18.5)",	"Normal (18.5-24.9)",	"Overweight(25-29.9)",	"Obese (>=30)"))
ffm<-ordered (data4$FFM, levels=c(1,2), labels =c("Low FFM", "Normal")) 
whr<-ordered (data4$WHR, levels=c(1,2), labels=c("Normal",	"Abdominal adiposity"))
htn<-data4$HTN
dm<-data4$DM

df4<-data.frame(bmi, ffm, whr, htn)
df4

p4 <-ggplot(df4,aes(bmi,ffm,fill=htn))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="HTN (%)",option ="C")
p4
p4 <-p4 + facet_grid(whr)

p4
p4 <-p4 + labs(title= "Prevalence of Hypertension", x="BMI", y="FFM")
p4

p4 <-p4 + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()#ggExtra
p4

##############################################
df5<-data.frame(bmi, ffm, whr, dm)
df5

p5 <-ggplot(df5,aes(bmi,ffm,fill=dm))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="Diabetes (%)",option ="C")
p5
p5 <-p5 + facet_grid(whr)

p5
p5 <-p5 + labs(title= "Prevalence of Diabetes", x="BMI", y="FFM")
p5

p5 <-p5 + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()#ggExtra
p5

#####################280520############################
data5<-read.csv("D:\\Work2020\\2020\\table5.csv",sep=',',header = T)
names(data5)
bmi<-ordered (data5$BMI, levels=c(1,2,3,4), labels=c("CED   (<18.5)",	"Normal (18.5-22.9)",	"Overweight(23-27.49)",	"Obese (>=27.5)"))
fp<-ordered (data5$FATP, levels=c(1,2,3), labels =c("T1 (<27.5)",	"T2 (27.5-35.4)",	"T3 (>=35.4)")) 
whr<-ordered (data5$WHR, levels=c(1,2), labels=c("Normal",	"Abdominal adiposity"))
htn<-data5$HTN
dm<-data5$DM
tc<-data5$High_TC


df5<-data.frame(bmi, fp, whr, htn)
df5

p5 <-ggplot(df5,aes(bmi,fp,fill=htn))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="HTN (%)",option ="C")
p5
p5 <-p5 + facet_grid(whr)

p5
p5 <-p5 + labs(title= "Prevalence of Hypertension", x="BMI", y="Fat Percent")
p5

p5 <-p5 + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()#ggExtra
p5


#########for male ############
data6<-data5[data5$Sex=='1',]
names(data6)
bmi<-ordered (data6$BMI, levels=c(1,2,3,4), labels=c("CED   (<18.5)",	"Normal (18.5-22.9)",	"Overweight(23-27.49)",	"Obese (>=27.5)"))
fp<-ordered (data6$FATP, levels=c(1,2,3), labels =c("T1 (<27.5)",	"T2 (27.5-35.4)",	"T3 (>=35.4)")) 
whr<-ordered (data6$WHR, levels=c(1,2), labels=c("Normal",	"Abdominal adiposity"))
htn<-data6$HTN
dm<-data6$DM

df6<-data.frame(bmi, fp, whr, htn)
df6

p6 <-ggplot(df6,aes(bmi,fp,fill=htn))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="HTN (%)",option ="C")
p6
p6 <-p6 + facet_grid(whr)

p6
p6 <-p6 + labs(title= "Prevalence of Hypertension-Male", x="BMI", y="Fat Percent")
p6

p6 <-p6 + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()#ggExtra
p6


#########female###########
data6<-data5[data5$Sex=='2',]
names(data6)
bmi<-ordered (data6$BMI, levels=c(1,2,3,4), labels=c("CED   (<18.5)",	"Normal (18.5-22.9)",	"Overweight(23-27.49)",	"Obese (>=27.5)"))
fp<-ordered (data6$FATP, levels=c(1,2,3), labels =c("T1 (<27.5)",	"T2 (27.5-35.4)",	"T3 (>=35.4)")) 
whr<-ordered (data6$WHR, levels=c(1,2), labels=c("Normal",	"Abdominal adiposity"))
htn<-data6$HTN
dm<-data6$DM

df6<-data.frame(bmi, fp, whr, htn)
df6

p6 <-ggplot(df6,aes(bmi,fp,fill=htn))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="HTN (%)",option ="C")
p6
p6 <-p6 + facet_grid(whr)

p6
p6 <-p6 + labs(title= "Prevalence of Hypertension-Female", x="BMI", y="Fat Percent")
p6

p6 <-p6 + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()#ggExtra
p6




#################################################
data5<-read.csv("D:\\Work2020\\2020\\table5.csv",sep=',',header = T)
names(data5)

bmi<-ordered (data5$BMI, levels=c(1,2,3,4), labels=c("CED   (<18.5)",	"Normal (18.5-22.9)",	"Overweight(23-27.49)",	"Obese (>=27.5)"))
fp<-ordered (data5$FATP, levels=c(1,2,3), labels =c("T1 (<27.5)",	"T2 (27.5-35.4)",	"T3 (>=35.4)")) 
whr<-ordered (data5$WHR, levels=c(1,2), labels=c("Normal",	"Abdominal adiposity"))
htn<-data5$HTN
dm<-data5$DM
max(dm)
df5<-data.frame(bmi, fp, whr, dm)
df5

p5 <-ggplot(df5,aes(bmi,fp,fill=dm))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="Diabetes (%)",option ="C")
p5
p5 <-p5 + facet_grid(whr)

p5
p5 <-p5 + labs(title= "Prevalence of Diabetes", x="BMI", y="Fat Percent")
p5

p5 <-p5 + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()#ggExtra
p5
####################################################################

#########for male ############
data6<-data5[data5$Sex=='1',]
names(data6)
bmi<-ordered (data6$BMI, levels=c(1,2,3,4), labels=c("CED   (<18.5)",	"Normal (18.5-22.9)",	"Overweight(23-27.49)",	"Obese (>=27.5)"))
fp<-ordered (data6$FATP, levels=c(1,2,3), labels =c("T1 (<27.5)",	"T2 (27.5-35.4)",	"T3 (>=35.4)")) 
whr<-ordered (data6$WHR, levels=c(1,2), labels=c("Normal",	"Abdominal adiposity"))
htn<-data6$HTN
dm<-data6$DM

df6<-data.frame(bmi, fp, whr, dm)
df6

p6 <-ggplot(df6,aes(bmi,fp,fill=dm))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="Diabetes (%)",option ="C")
p6
p6 <-p6 + facet_grid(whr)

p6
p6 <-p6 + labs(title= "Prevalence of diabetes-Male", x="BMI", y="Fat Percent")
p6

p6 <-p6 + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()#ggExtra
p6


#########female###########
data6<-data5[data5$Sex=='2',]
names(data6)
bmi<-ordered (data6$BMI, levels=c(1,2,3,4), labels=c("CED   (<18.5)",	"Normal (18.5-22.9)",	"Overweight(23-27.49)",	"Obese (>=27.5)"))
fp<-ordered (data6$FATP, levels=c(1,2,3), labels =c("T1 (<27.5)",	"T2 (27.5-35.4)",	"T3 (>=35.4)")) 
whr<-ordered (data6$WHR, levels=c(1,2), labels=c("Normal",	"Abdominal adiposity"))
htn<-data6$HTN
dm<-data6$DM

df6<-data.frame(bmi, fp, whr, dm)
df6

p6 <-ggplot(df6,aes(bmi,fp,fill=dm))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="Diabetes (%)",option ="C")
p6
p6 <-p6 + facet_grid(whr)

p6
p6 <-p6 + labs(title= "Prevalence of diabetes-Female", x="BMI", y="Fat Percent")
p6

p6 <-p6 + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()#ggExtra
p6
