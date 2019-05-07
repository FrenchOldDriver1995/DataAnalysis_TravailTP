da <- read.delim("~/Downloads/data2TP1.txt")
a1<-da[,c(1)]#mars
a2<-da[,c(2)]#aix
moyen<-mean(a1)
moyen2<mean(a2)

#moyenne théorique
t<-abs(moyen-19)/(sd(a1)/sqrt(length(a1)))

t2<-abs(moyen-moyen2)/sqrt(var(a1)/length(a1)+var(a2)/length(a2))

#Test Non Paramétrique
n=1528+106+117+381
ratio1=n*9/16
ratio2=n*3/16
ratio3=n*3/16
ratio4=n*1/16
KhiDeux=(1528-ratio1)^2/ratio1+(106-ratio2)^2/ratio2+(117-ratio3)^2/ratio3+(381-ratio4)^2/ratio4
#dans le table Khi Deux, avec df=3, alpha=5%, le result est 7.81, de ce fait, on peut contredire le ratio n'est pas vrai
form=matrix(c(29,5,46,40,32,8,18,22,0),nrow=3,byrow=TRUE)
color=matrix(c(20,60,29,51,12,28),nrow=3,byrow=TRUE)
kd1=chisq.test(form)
kd2=chisq.test(color)
#on trouve que kd1 est beaucoup plus petit que kd2 , c'est à dire que le form est plus dépendant/important pour détecter un mélanome
