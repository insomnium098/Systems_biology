
###Cargando grind
source("grind.R")

####Crear carpetas donde iran las imagenes
dir.create("P2")
dir.create("P3")
dir.create("P4")
dir.create("P5")

#####Crear ruta de las carpetas
working_directory <- getwd()


###Definiendo Modelo
model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    R = 1/(1+A^n) # Repressor
    ###Modificando Allolactasa de acuerdo al manual
    ###Se comentara la formula original
    #dA = M*L - delta*A - v*M*A # Allolactose OLD
    ##Se agrega la division entre h+A
    dA = M*L - delta*A - ((v*M*A) / (h+A))
    dM = c0 + c*(1-R) - d*M # mRNA
    return(list(c(dA, dM)))
  })
}

###Definiendo Parametros por default
p <- c(L=1,c=1,c0=0.05,d=1,delta=0.2,n=5,v=1, h = 2)
s <- c(A=0,M=0.5)
plane(xmax=4)
#low <- newton(s,plot=T)
#mid <- newton(c(A=0.8,M=0.2),plot=T)
#hig <- newton(c(A=2,M=1),plot=T)
#continue(mid,x="L",y="A",xmax=2,ymax=4)

####Punto 2

###Correr el modelo en 50 unidades de tiempo
pdf(paste0(working_directory,"/P2/original_values.pdf"))
run(50)
title ("Original Values")
dev.off()

#####Variar la concentracion de A
s <- c(A=0.2,M=0.5)
pdf(paste0(working_directory,"/P3/A_0_2.pdf"))
#pdf("A_0_2.pdf")
run(50)
title ("A=0.2")
dev.off()

s <- c(A=0.4,M=0.5)
pdf(paste0(working_directory,"/P3/A_0_4.pdf"))
#pdf("A_0_4.pdf")
run(50)
title ("A=0.4")
dev.off()

###Punto 4: Phase-plane with nullclines
plane(add=T)
###Add rajectory
run(traject = T)

###Phase portrait
plane(tstep=0.2,portrait=T)

###La funcion newton() encuentra un estado estable cerca del estado inicial
##Definieno de nuevo los parametros iniciales
s <- c(A=0,M=0.5)

###Plot con vector field 
pdf(paste0(working_directory,"/P4/Punto_4.pdf"))
#pdf("Punto_4.pdf")
plane(xmax=4, vector = TRUE, tstep=0.2, portrait = T)
low <- newton(s,plot=T)
mid <- newton(c(A=0.8,M=0.2),plot=T)
hig <- newton(c(A=2,M=1),plot=T)
dev.off()


####Punto 5
###Vary the amount of Lactose in the environment
### parameter L. What happens to the nullclines?
###What does this imply?

###La lactosa original es 1
pdf(paste0(working_directory,"/P5/Lactose_original_1.pdf"))

#pdf("Lactose_original_1.pdf")
p <- c(L=1,c=1,c0=0.05,d=1,delta=0.2,n=5,v=1, h = 2)
s <- c(A=0,M=0.5)
plane(xmax=4,vector = TRUE, tstep=0.2, portrait = T)
low <- newton(s,plot=T)
mid <- newton(c(A=0.8,M=0.2),plot=T)
hig <- newton(c(A=2,M=1),plot=T)
title("Lactose = 1")
dev.off()

###Lactose 0.5
Lactose <- 0.5
pdf(paste0(working_directory,"/P5/Lactose_0.5.pdf"))

#pdf("Lactose_0.5.pdf")
p <- c(L=Lactose,c=1,c0=0.05,d=1,delta=0.2,n=5,v=1, h = 2)
s <- c(A=0,M=0.5)
plane(xmax=4,vector = TRUE, tstep=0.2, portrait = T)
low <- newton(s,plot=T)
mid <- newton(c(A=0.8,M=0.2),plot=T)
hig <- newton(c(A=2,M=1),plot=T)
title("Lactose = 0.5")
dev.off()

###Lactose 1.5
Lactose <- 1.5
pdf(paste0(working_directory,"/P5/Lactose_1.5.pdf"))
#pdf("Lactose_1.5.pdf")
p <- c(L=Lactose,c=1,c0=0.05,d=1,delta=0.2,n=5,v=1, h = 2)
s <- c(A=0,M=0.5)
plane(xmax=4,vector = TRUE, tstep=0.2, portrait = T)
low <- newton(s,plot=T)
mid <- newton(c(A=0.8,M=0.2),plot=T)
hig <- newton(c(A=2,M=1),plot=T)
title("Lactose = 1.5")
dev.off()



