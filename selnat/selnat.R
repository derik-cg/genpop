## Evoluci�n por selecci�n natural de la frecuencia al�lica en un 
##  sistema de un locus y dos alelos
## esta funci�n sigue a J. roughgarden en su libro "theory of 
## population genetics and evolutionary biology. An introductioin.
## Codigo por Derik Castillo Guajardo

#primero se declara la fucnci�n
#p es la proporci�n de alelos inicial
#w es un vector de tres entradas con wAA, wAa, waa en ese orden
#t esel tiempo, numero de veces que se itera la ecuaci�n
selnat<-function(p,w,t)
{
  e<-rep(0,t+1) #inicializar la salida
  e[1]<-p
  for (i in 2:(t+1)) # iterar con ciclo for
  {
    e[i]<-(w[1]*e[i-1]^2+w[2]*e[i-1]*(1-e[i-1]))/(w[1]*e[i-1]^2+2*w[2]*e[i-1]*(1-e[i-1])+w[3]*(1-e[i-1])^2)
  }
  return(e)
  }
