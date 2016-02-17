#script para hacer figura de selección en favor de aelo dominante

# first source selat function

s<-0.1
w<-c(1,(1-s),(1-s))
e1<-selnat(0.1,w,50)

s<-0.25
w<-c(1,(1-s),(1-s))
e25<-selnat(0.1,w,50)

s<-0.5
w<-c(1,(1-s),(1-s))
e5<-selnat(0.1,w,50)

s<-0.75
w<-c(1,(1-s),(1-s))
e75<-selnat(0.1,w,50)

plot(e1,xlim=c(0,50),ylim=c(0,1),type="l",xlab="Tiempo",ylab="Proporción A")
lines(e25)
lines(e5)
lines(e75)

#### Selección a favor del recesivo

s<-0.1
w<-c((1-s),(1-s),1)
e1<-selnat(0.9,w,50)

s<-0.25
w<-c((1-s),(1-s),1)
e25<-selnat(0.9,w,50)

s<-0.5
w<-c((1-s),(1-s),1)
e5<-selnat(0.9,w,50)

s<-0.75
w<-c((1-s),(1-s),1)
e75<-selnat(0.9,w,50)

plot(e1,xlim=c(0,50),ylim=c(0,1),type="l",xlab="Tiempo",ylab="Proporción A")
lines(e25)
lines(e5)
lines(e75)

## selección a favor del heterocigoto

s<-0.3
w<-c((1-s),1,(1-s))
e1<-selnat(0.1,w,50)

s<-0.3
w<-c((1-s)1,(1-s))
e25<-selnat(0.25,w,50)

s<-0.3
w<-c((1-s),1,(1-s))
e5<-selnat(0.75,w,50)

s<-0.3
w<-c((1-s),1,(1-s))
e75<-selnat(0.9,w,50)

plot(e1,xlim=c(0,50),ylim=c(0,1),type="l",xlab="Tiempo",ylab="Proporción A")
lines(e25)
lines(e5)
lines(e75)
