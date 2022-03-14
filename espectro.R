
######################################################################
#             Modelo Lineal Funcional Generalizado
######################################################################
###################li####################################
# Cargamos las librerias y los datos
#######################################################
library(fda.usc)
setwd("D:/PRACTRICAS_PRE_PROFESIONALES_EPN/Pracitcas_con_Software_R/Informacion/Muestra 1")

esp <- read.table("Espectro-1.txt",header=T,dec = "," )

nivel <- c(0.5, 1.48, 2.44, 3.85, 6.10, 9.09, 16.67, 37.50, 50, 83.33, 100 )

nivel.b <- nivel > 10

espectros <- t(esp[,-1])

colnames(espectros) <- esp[,1]

f.espectros<-fdata(espectros,names=list("Nivel de contaminación","Espectros","Length"))
str(f.espectros)
######clasificación

# GLM empleando la respuesta binaria
y <- nivel.b
ab = f.espectros
##################
#Primer modelo
##################
ldata1 = list(df=data.frame(y), ab = ab)
res.glm = fregre.glm(y ~ ab, data = ldata1, family = binomial())

# Predicción
newesp <- read.table("muestra.csv",header=T,sep = ";")
newespectros <- t(newesp[,-1])
colnames(newespectros) <- newesp[,1]
f.newespectros<-fdata(newespectros,names=list("Nivel de contaminación","Nuevos Espectros","Length"))
str(f.newespectros)
ldatanew = list(ab = f.newespectros) # Nuevos datos para realizar su clasificación
res.pr = predict.fregre.glm(res.glm,ldatanew)# Se ha equivocado en tres observaciones
round(res.pr, 3)
memb<-res.pr > 0.5

pdf('espectro.pdf') 
matplot(t(f.newespectros$data),type="l",lty=1,main="Espectro",ylab="Nivel",xlab="length",col=memb+1)
ifelse(test = memb,etiqueta  <- "Alto",etiqueta <- "Bajo")
legend("topright", etiqueta,lwd=3,bty="n", title="Nivel de contaminacion",text.col=c(1,2),lty=1,col=c(1,2))
dev.off()


  

