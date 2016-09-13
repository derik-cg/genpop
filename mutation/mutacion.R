##########################################
#### primera parte
#### mutacion de 1 via. una simulación
##########################################

#Codigo para la simulaición de la mutación
#mutación de una via

#parámetros
p0<-0.9 #frecuencia alélica inicial
u<-10^-6 #tasa de mutación

#hacer un ciclo para la iteración numérica
#primero hacer el vector de los tiempos

tiempo<-seq(from=0,to=10^7,length.out = 100)
frec<-p0 #inicializar el vector de los resultados
for(i in 1:(length(tiempo)-1))
{
  frec[i+1]<-frec[1]*(1-u)^tiempo[i+1]
}

#graficar el resultado
plot(tiempo,frec,type="l",ylab="Frecuencia")

##############################################
##### mutación de dos vias
##### multiples simulaciones
##############################################
#este código es para hacer múltiples lineas en la misma figura
#parámetros
p0<-0.9 #frecuencia alélica inicial
u<-c(10^-3,10^-5,10^-6,10^-8,2.5*10^-8) #tasa de mutación
#hacer un ciclo para la iteración numérica
#primero hacer el vector de los tiempos

tiempo<-seq(from=0,to=10^7,length.out = 100)
frec<-array(p0,dim=c(100,5)) #inicializar el vector de los resultados

for(i in 2:(length(tiempo)-1))
{
  frec[i+1,]<-frec[1,]*(1-u)^tiempo[i+1]
}

#graficar el resultado
plot(tiempo,frec[,2],type="l",ylab = "Frecuencia")
for(i in 2:5)
{
  lines(tiempo,frec[,i],lty=i)
}

###########################################
#### balance mutación selección
###########################################

#Para ver la dinámica, escribir por separado ambas
#ecuaciones. selección y mutación
p0<-0.1 #Condición inicial
s<-0.016666#presión de selección
wAA<- 1-s #adecuación de homocigotos dominante
wAa<- 1-s #adecuación de heterocigoto
waa<-1    #adecuación de homocigoto recesivo
v<-10^-3 # tasa de mutación a->A

#deltap=deltapsel+deltapmut
p<-rep(NA,500);p[1]<-p0 #inicializar vector de salida
for (i in 2:500)
{
  dp<-((p[i-1]*wAA+(1-p[i-1])*wAa)/
    (p[i-1]^2*wAA+2*p[i-1]*(1-p[i-1])*wAa+
       (1-p[i-1])^2*waa))*p[i-1]-p[i-1]+
    (v*(1-p[i-1]))
  p[i]<-p[i-1]+dp
}
plot(p,type="l",ylim=c(0,0.1),ylab="Frecuencia")

###########################################
#### mutación de dos vias
#### una simulación
############################################

p0<-0.001 #frecuencia inicial
v<-10^-3 #Tasa de mutación A->a
u<-2*10^-3 #tasa de mutación a->A

p<-rep(NA,5000);p[1]<-p0 #inicializar vector de salida
for (i in 2:5000)
{
  p[i]<-(1-u)*p[i-1]+v*(1-p[i-1])
}
plot(p,type="l",ylab="Frecuencia")
