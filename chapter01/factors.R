x<-c("a","b","c","c","b")

y<-factor(x)
y

y<-as.factor(x)
y

y<-factor(x,levels=c("c","a","b"))
y

y<-relevel(y,ref="a")
y

#reordering all levels
y<-factor(y,levels=c("a","b","c"))
y

#renaming data
levels(y)<-c("q","r","s")
y
