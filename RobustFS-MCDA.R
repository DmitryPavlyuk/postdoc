require(needs)
needs(igraph)
needs(QuACN)

res<-list()


adj.complete <- matrix(1,9,9)
g<-as(adj.complete,"graphNEL")
g <- randomEGraph(as.character(1:64), 0.1)
plot(g)

library(igraph)
myEdges <- combn(1:9,2)
myGraph <- graph(myEdges, directed=FALSE)
tkplot(myGraph)

g <- randomEGraph(as.character(1:64), 0.1)
plot(g)
offdiagonal(g)
res[[length(res)+1]]<-list(g=g,val=offdiagonal(g))

est<-c(4,	5,	5,	3,	4,	2,	4,	4,	4,	6,	5,	1,	3,	4,	2,	3,	3,	4,	4,	2)
estPolina<-c(3,	6,	7,	7,	6,	4,	9,	9,	8,	6,	4,	2,	4,	6,	6,	7,	4,	4,	3,	1)


odc<-c()
for (r in res){
  odc<-c(odc,offdiagonal(r$g))
  #plot(r$g)
  #print(round(offdiagonal(r$g)*10))
  #readline(prompt="Press [enter] to continue")
}
cor(est,odc)
cor(est,estPolina)
cor(estPolina,odc)




data<-read.csv("E:/Dmitry/Dropbox/science/2020/TRC/materials/MultiCriteria.csv", sep=";",dec=",", header=T)
head(data)
criteriaMinMax <- c("min","max","max","min","min")
names(criteriaMinMax) <- colnames(data)[4:8]
criteriaMinMax
needs(MCDA)
plotRadarPerformanceTable(data[,4:8], criteriaMinMax, overlay=FALSE, bw=TRUE, lwd =5)

pT<-data[,4:8]
rownames(pT)<-data[,3]
par(mfrow=c(2,3))
for (i in 1:dim(pT)[2]){
yaxis <- range(pT[,i])*c(0.99,1.05)
if (criteriaMinMax[i] =="min")
oPT <- pT[order(pT[,i],decreasing=FALSE),]
else
oPT <- pT[order(pT[,i],decreasing=TRUE),]
name <-paste(colnames(pT)[i]," (",criteriaMinMax[i],")", sep="")
barplot(oPT[,i], main=name, names.arg = rownames(oPT),
density = i*10, ylim = yaxis, xpd=FALSE)
}


needs(tidyverse)

normalizationTypes <- c("percentageOfMax","percentageOfMax","percentageOfMax","percentageOfMax","percentageOfMax")
names(normalizationTypes) <- colnames(pT)
nPT <- normalizePerformanceTable(pT,normalizationTypes)

w <- c(-0,1,1,-0,-0)
names(w) <- colnames(pT)
ws<-weightedSum(nPT,w)
tib<-as_tibble(rank(-ws))%>%mutate(alternative=names(rank(-ws)))

tib%>%arrange(value)%>%print(n=36)
