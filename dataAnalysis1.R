data <- read.delim("~/Downloads/data1TP1.txt")
##1Tracer les données
  par(mfrow=c(2,3)) #设置2行3列的分割
  plot(data[,c(1)],data[,c(6)])
  plot(data[,c(2)],data[,c(6)])
  plot(data[,c(3)],data[,c(6)])
  plot(data[,c(4)],data[,c(6)])
  plot(data[,c(5)],data[,c(6)])
##2r de Pearson
  #计算协方差 convariance
  cov1 <- cov(data[,c(1)],data[,c(6)])
  cov2 <- cov(data[,c(2)],data[,c(6)])
  cov3 <- cov(data[,c(3)],data[,c(6)])
  cov4 <- cov(data[,c(4)],data[,c(6)])
  cov5 <- cov(data[,c(5)],data[,c(6)])

  #计算标准差，即方差开平方 écart type
  s1 <- sd(data[,c(1)])
  s2 <- sd(data[,c(2)])
  s3 <- sd(data[,c(3)])
  s4 <- sd(data[,c(4)])
  s5 <- sd(data[,c(5)])
  s6 <- sd(data[,c(6)])

  #计算相关系数 coefficient de Pearson
  ro1 <- cov1/(s1*s6)
  ro2 <- cov2/(s2*s6)
  ro3 <- cov3/(s3*s6)
  ro4 <- cov4/(s4*s6)
  ro5 <- cov5/(s5*s6)
  #verify
  res1<-cor(data[,c(1)],data[,c(6)]) 
  res2<-cor(data[,c(2)],data[,c(6)])
  res3<-cor(data[,c(3)],data[,c(6)])
  min_ro<-min(ro1,ro2,ro3,ro4,ro5)#minimum
##3 r de Spearman
  a<-data[,c(1)]
  b<-data[,c(2)]
  c<-data[,c(3)]
  d<-data[,c(4)]
  e<-data[,c(5)]
  y<-data[,c(6)]
  
  #spearman coefficient
  sro1<-1-6*sum((rank(a)-rank(y))^2)/(15^3-15)
  sro2<-1-6*sum((rank(b)-rank(y))^2)/(15^3-15)
  sro3<-1-6*sum((rank(c)-rank(y))^2)/(15^3-15)
  sro4<-1-6*sum((rank(d)-rank(y))^2)/(15^3-15)
  sro5<-1-6*sum((rank(e)-rank(y))^2)/(15^3-15)
  #verify
  sro_verify<-cor(a,y,method="spearman")
  sro_verify2<-cor(b,y,method="spearman")
  sro_verify3<-cor(c,y,method="spearman")
  sro_verify4<-cor(d,y,method="spearman")
  sro_verify5<-cor(e,y,method="spearman")
  
  
  
  