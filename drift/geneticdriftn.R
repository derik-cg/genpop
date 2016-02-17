#simulation of genetic drift using markov chains

#set population size
n<-1
#set number of simulations
t<-10
#set the vector of initial frequencies with 1 in the middle
p<-matrix(0,nrow=t,ncol=(2*n+1))
p[1,(n+1)]<-1

#create the transition matrix
T<-matrix(0,nrow=(2*n+1),ncol=(2*n+1))
for (i in 0:(2*n))
{
  for (j in (0:2*n))
  {
    T[(j+1),(i+1)]<-choose((2*n),i)*(j/(2*n))^i*(1-j/(2*n))^(2*n-i)
  }
}
#now iterate over time to produce a matrix of population states in time
for (i in 2:t)
{
  p[i,]<
    -p[i-1,]%*%T
}
#now plot the result
require(lattice)
require(latticeExtra)
cloud(p,panel.3d.cloud=panel.3dbars, xlim=c(0,10), ylim=c(0,4),
      xbase=0.5,ybase=0.5,
      screen=list(z=-80, x=-60))
#persp(p,theta=60,phi=10)
p2<-data.frame(rho=rep(1:3,each=10),prop=c(p[,1],p[,2],p[,3]),time=factor(rep(1:10,3)))
barchart(rho~prop|time,data=p2,layout=c(10,1),main="Deriva génica en el tiempo")
################
#### sumulation with aleles
##############
rm(list=ls())
#set up for number of individuals
n<-10 # use even numbers #rows
#set up for number of generations
t<-50 #layers
#set up the number of populations
c<-10 # number of repetitions # columns

#use haploid genetic system

# first set up a population with a fixed p

#do the 3d array
pop<-array(0,dim=c(n,c,t)) ### n c t ####
ind<-rep(rep(c(0,1),each=(n/2)),c)
pop[,,1]<-matrix(ind,nrow = n,ncol = c)

# then randomly choose alleles to make up the next generation
for (i in 2:t) #layers
{
  for (j in 1:c) #columns
  {
    if (sum(colSums(pop[,,(i-1)])==c(n*c,0)))
    {
      pop<-pop[,,1:(i-1)]
      break
    }
    else
    {
      pop[,j,i]<-sample(pop[,j,(i-1)],size=n,replace=T)
    }  
  }
}
if (length(dim(p))==2)
{
  p<-apply(pop,1,FUN=sum)
  plot(p,type="l",xlab = "time")
}
#plot the rest only if there are layers
matsums<-apply(pop,c(2,3),sum)
plot(matsums[1,],type="l",xlab="time",ylab="Number of alleles",ylim=c(0,max(matsums)))
for (i in 2:c)
{
  lines(matsums[i,])
}