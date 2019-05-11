A <- read.delim("~/Downloads/data1TP2.txt") #Point

statut=A[,c(1)] # ==A$Stature
Poids=A[,c(2)]# ==A$Poids
Taille=A[,c(3)] #==A$Taille
library("plot3D")
library("scatterplot3d")
library("plot3D")
library("plot3Drgl")
#2 façons de montres les nuages de points en dimension 3
scatterplot3d(statut,Poids,Taille)

scatter3D(statut,Poids,Taille)
#scatter3D(A$Stature,A$Poids,A$Taille)
#axes principaux x=statut, y=poids, z=taille

V=cov(A) #pour le matrix covariance

B=scale(A,center = T, scale = F) #cov(B)==V   center=T这句话可以省略

eiV=eigen(V)
v_propre=eiV$values #valeur_propre
v_vecteur=eiV$vectors #valeur_vecteur
C=B%*%-v_vecteur
C2=princomp(A)$scores #主成分分析 ACP
print(v_vecteur)

scatter3D(x=c(0,-300*v_vecteur[1,1]),y=c(0,-300*v_vecteur[2,1]),z=c(0,-300*v_vecteur[3,1]),add = TRUE,type = 'l')
plotrgl()

library(devtools)
#install_github('andreacirilloac/updateR')
library(updateR)

#exercice7
plot(C[,1],C[,2])

