mydata<-read.csv("c://crime.csv",header = TRUE)
mydata
z<-mydata[,-c(1,1)]
#Normalization 
z<-mydata[,-c(1,1)]
m<-apply(z,2,mean)
s<-apply(z,2,sd)
z<-scale(z,m,s)
#Calculating Euclidean distance 
distance<-dist(z,method = "euclidean")
#calculating dendogram with Complete linkage 
hc.c<-hclust(distance,method = "complete")
plot(hc.c)
plot(hc.c,hang = -1)
#calculating dendogram with average linkage 
hc.a<-hclust(distance,method = "average")
plot(hc.a)
plot(hc.a,hang = -1)
#calculating dendogram with single linkage 
hc.s<-hclust(distance,method = "single")
plot(hc.s)
plot(hc.s,hang = -1)
#calculating dendogram with centroid linkage 
hc.z<-hclust(distance,method = "centroid")
plot(hc.z)
plot(hc.z,hang = -1)

#clustring membership
member.c<-cutree(hc.c,3)
member.a<-cutree(hc.a,3)
table(member.c,member.a)
