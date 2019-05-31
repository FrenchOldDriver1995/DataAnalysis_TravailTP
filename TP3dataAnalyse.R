#TP3
#point a
x=runif(100,0,1)
y=runif(100,0,1)
plot(x,y,xlim=c(-4,10),ylim=c(-4,10),col=1,pch=1)
a=data.frame(x,y)

#point b
mamtrix
x=rnorm(100,4,1)
y=rnorm(100,0,1)
points(x,y,col=2,pch=2)
b=data.frame(x,y)

#point c
x=rnorm(100,0.5,sqrt(2))
y=rnorm(100,6,sqrt(2))
points(x,y,col=3,pch=3)
c=data.frame(x,y)
nuage=rbind(a,b,c)

plot(nuage[,1],nuage[,2])
points(mean(nuage[,1]),mean(nuage[,2]),col=3)
km.out=kmeans(nuage,2,nstart = 20)#可直接用于kmeans
plot(x,col=(km.out$cluster+1), pch=20,cex=2)

myfunc = function (nuage, k){
  count = 1
  nr = nrow(nuage)
  c = diag(nr)
  MA = matrix(numeric(0), nr, nr)
  for(i in 1:nr){
    for(j in i:nr){
      MA[j, i]  =sqrt((nuage[i,1] - nuage[j,1])^2) + (nuage[i,2] - nuage[j,2])^2
    }
  }
  while( count<= nr-k){
    cntmin = MA[2,1]
    pi=2
    pj=1
    for(i in 2:(nr-count+1)){
      
      for(j in 1:(i-1)){
        if(MA[i, j] < cntmin){
          cntmin = MA[i, j]
          pi = i
          pj = j
        }
      }
    }
    C[pj, ] = C[pj,] + C[pi, ]
    
    for( i in 1: nr-count+1){
      nitmp = nuage[C[i, ]==1]
      gitmp = c(mean(nitmp$X), mean(nitmp$Y))
      dtmp = sqrt((gtmp[1] - gitmp[1])^2 + (gtmp[2] - gitmp[2])^2)
      if( i <pj){
        MA[pj, i] = dtmp
      }else{
        MA[i, pj] = dtmp
      }
    }
    
    C=C[-pi,]
    ntmp = nuage[C[pj,]==1,]
    gtmp = c(mean(ntmp$X), mean(ntmp$Y))
    MA = MA[-pi,]
    MA=MA[, -pi]
    count = count+1
    
  }
  return(t(C))
}

C = myfunc(nuage,3)

#Verification
D = dist(nuage, method ="euclidean")
Asc = hclust(D, method ="complete")
plot(Asc, cex=0.6, hang=-1)
cluster = cutree(Asc, 3)
