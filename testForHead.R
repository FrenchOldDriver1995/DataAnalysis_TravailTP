d = read.csv(file = "~/Desktop/dataAnalysisCode/test3.csv",header = FALSE, sep = ",")
#header设置为false这样就能把时间轴读入进去
# names(d)  #V1,V2,V3...
library(ggplot2)
d2=d[-c(1,4,5),-c(39)] #4,5是空白行，39是空的备注行，1是信息栏？怎么在headerfalse
# d3=d[-c(4,5),-c(39)]
d4=na.omit(d2) # ==d2
time2=0:28
#dv=as.vector(d2,mode="any")




#直接plot里d2$V2等返回的是当前数的排序index，而不是本身的值
#Affichage des lignes

#Delta_TP9
plot(time2,as.vector(d2$V2,mode="any"),ylim=c(-1,1.5),ylab="Delta_TP9",col=2,type="b")
points(time2,d2$V2)
#Delta_AF7
plot(time2,as.vector(d2$V3,mode="any"),ylim=c(-1,1.5),ylab="Delta_AF7",col=2,type="b")
points(time2,d2$V3)
#Delta_AF8
plot(time2,as.vector(d2$V4,mode="any"),ylim=c(-1.5,2),ylab="Delta_AF8",col=2,type="b")
points(time2,d2$V4)
#Delta_TP10
plot(time2,as.vector(d2$V5,mode="any"),ylim=c(-1.5,2),ylab="Delta_TP10",col=2,type="b")
points(time2,d2$V5)
#Theta_TP9
lines(time2,as.vector(d2$V6,mode="any"),ylab="Theta_TP9",col=3,type="b")
points(time2,d2$V6,pch=4)
#Theta_AF7
lines(time2,as.vector(d2$V7,mode="any"),ylab="Theta_AF7",col=3,type="b") #Theta_AF7
points(time2,d2$V7)
#Theta_AF8
lines(time2,as.vector(d2$V8,mode="any"),ylab="Theta_AF8",type="b",col=3)
points(time2,d2$V8,col=8)
#Theta_TP10
lines(time2,as.vector(d2$V9,mode="any"),ylab="Theta_TP10",type="b",col=3)
points(time2,d2$V9,col=9)
#Alpha_TP9
lines(time2, as.vector(d2$V10,mode="any"),ylab="ALPHA_TP9",type="b",col=4) #ALpha_TP9
points(time2,d2$V10,col=6)
#Alpha_AF7
lines(time2, as.vector(d2$V11,mode="any") ,ylab="ALPHA_AF7",type="b",col=4) #Alpha_AF7
points(time2,d2$V11)
#Alpha_AF8
lines(time2,as.vector(d2$V12,mode="any"),ylab="ALPHA_AF8",type="b",col=4) 
points(time2,d2$V12)
#Alpha_TP10
lines(time2,as.vector(d2$V13,mode="any"),ylab="Alpha_TP10",type="b",col=4)
points(time2,d2$V13,col=13)
#Beta_TP9
lines(time2,as.vector(d2$V14,mode="any"),ylab="Beta_TP9",type="b",col=5)
points(time2,d2$V14,col=8)
#Beta_AF7
lines(time2,as.vector(d2$V15,mode="any"),ylab="Beta_AF7",type="b",col=5)
points(time2,d2$V15,col=15)
#Beta_AF8
lines(time2,as.vector(d2$V16,mode="any"),ylab="Beta_AF8",type="b",col=5)
points(time2,d2$V16,col=16)
#Beta_TP10
lines(time2,as.vector(d2$V17,mode="any"),ylab="Beta_TP10",type="b",col=5)
points(time2,d2$V17,col=17)
#Gamma_TP9
lines(time2,as.vector(d2$V18,mode="any"),ylab="Gamma_TP9",type="b",col=6)
points(time2,d2$V18,col=18)
#Gamma_AF7
lines(time2,as.vector(d2$V19,mode="any"),ylab="Gamma_AF7",type="b",col=6)
points(time2,d2$V19,col=19)
#Gamma_AF8
lines(time2,as.vector(d2$V20,mode="any"),ylab="Gamma_AF8",type="b",col=6)
points(time2,d2$V20,col=20)
#Gamma_TP10
lines(time2,as.vector(d2$V21,mode="any"),ylab="Gamma_TP10",type="b",col=6)
points(time2,d2$V21,col=21)
#Raw_TP9
lines(time2,as.vector(d2$V22,mode="any"),ylab = "Raw_TP9",type="b",col=22)
points(time2,d2$V22,col=22)
#RawAF7
lines(time2,as.vector(d2$V23,mode="any"),ylab="Raw_AF7",type="b",col=23)
points(time2,d2$V23,col=23)
#Raw_AF8
lines(time2,as.vector(d2$V24,mode="any"),ylab="Raw_AF8",type="b",col=24)
points(time2,d2$V24,col=24)
#Raw_TP10
lines(time2,as.vector(d2$V25,mode="any"),ylab="Raw_TP10",type="b",col=25)
points(time2,d2$V25,col=25)
#AUX_RIGHT
lines(time2,as.vector(d2$V26,mode="any"),ylab="AUX_TIGHT",type="b",col=26,pch="--")
points(time2,d2$V26,col=26,pch="*")
#Accelerometer_X
lines(time2,as.vector(d2$V27,mode="any"),ylab="Accelerometer_X",type="b",col=27)
points(time2,d2$V27,col=27,pch=5)
#Accelerometer_Y
lines(time2,as.vector(d2$V28,mode="any"),ylab="Accelerometer_Y",type="b",col=28)
points(time2,d2$V28,col=28,pch=6)
#Accelerometer_Z
lines(time2,as.vector(d2$V29,mode="any"),ylab="Accelerometer_Z",type="b",col=29)
points(time2,d2$V29,col=29,pch=7)
#Gyro_X
lines(time2,as.vector(d2$V30,mode="any"),ylab="Gyro_X",type="b",col=1)
points(time2,d2$V30,col=1,pch=2)
#Gyro_Y
lines(time2,as.vector(d2$V31,mode="any"),ylab="Gyro_Y",type="b",col=2)
points(time2,d2$V31,col=2,pch=3)
#Gyro_Z
lines(time2,as.vector(d2$V32,mode="any"),ylab="Gyro_Z",type="b",col=3)
points(time2,d2$V32,col=3,pch=4)



#commentaire
#lty line type
#inset, change location
#cex, change format
#box.lty  ,box.lwd 改变框的样式，颜色等

legend('bottomleft',legend = c('Delta_TP9','Theta_TP9','Alpha_TP9','Beta_TP9','Gamma_TP9'),fill=c(2,3,4,5,6),cex=0.7,inset = .02)
legend('topright',legend = c('Delta_AF7','Theta_AF7','Alpha_AF7','Beta_AF7','Gamma_AF7'),fill=c(2,3,4,5,6),cex=0.7,inset = .02)
legend('bottomleft',legend = c('Delta_AF8','Theta_AF8','Alpha_AF8','Beta_AF8','Gamma_AF8'),fill=c(2,3,4,5,6),cex=0.7,inset = .02)
legend('bottomleft',legend = c('Delta_TP10','Theta_TP10','Alpha_TP10','Beta_TP10','Gamma_TP10'),fill=c(2,3,4,5,6),cex=0.7,inset = .02)




