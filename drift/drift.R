# Deriva Génica según Roughgarden
# tomada de "Theory of population genetics and evolutionary
# biology an introduction"

#Numero de iteraciones
iter<-5

# ecuaciones para la iteracion
rhoiter<-function(rho)
{
  #state es el estado de la poblacion
  # es un vector de tres entradas con la fracción de la población
  #en cada genotipo 
  aa<-rho[1]+1/4*rho[2] ## funcion para sacar aa
  Aa<-1/2*rho[2]
  AA<-1/4*rho[2]+rho[3]
  return(c(aa,Aa,AA))
}

#condiciones iniciales
#Fracciones de la población con cada genotipo
# aa,Aa,AA
rho=matrix(0,nrow=(iter+1),ncol=3)
rho[1,]<-c(0,1,0)
#plot the result
for (i in 1:(iter+1))
{
  barplot(rho[i,])
  Sys.sleep(2)
}
