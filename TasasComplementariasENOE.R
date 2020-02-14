#clear workspace
rm(list = ls(all = TRUE)) 

################# Cargamos las biblioecas necesarias ####################3
library(foreign)
library(questionr)
library(survey)

setwd("/home/milo/Documentos/R/ENOE/datos")
###################Cargado de Bases#######################################

sdemt319 <- read.dbf ("SDEMT319.dbf")
coe1t319 <- read.dbf ("COE1T319.dbf")
coe2t319 <- read.dbf ("COE2T319.dbf")

folio <- paste (sdemt319$CD_A, sdemt319$ENT, sdemt319$CON, sdemt319$V_SEL, sdemt319$N_HOG, 
                sdemt319$H_MUD, sdemt319$N_REN)
x<-data.frame (sdemt319, folio)
folio<- paste(coe1t319$CD_A, coe1t319$ENT, coe1t319$CON, coe1t319$V_SEL, coe1t319$N_HOG, 
              coe1t319$H_MUD, coe1t319$N_REN)
x1<-data.frame(coe1t319, folio)
folio<- paste(coe2t319$CD_A, coe2t319$ENT, coe2t319$CON, coe2t319$V_SEL, coe2t319$N_HOG, 
              coe2t319$H_MUD, coe2t319$N_REN)
x2<-data.frame(coe2t319, folio)
total <- merge(x,x1,by="folio")
enoetotal <-merge(total, x2, by="folio")

################   DATA FRAME #######
#Uso data frame con la ENOE.
#En R el uso de data frame es la creaciÃ³n de un marco de datos
#Uno de los usos mas frecuentes es la generacion de "sub-bases"
#Es decir por ejemplo si vamos a trabajar con la ENOE y ya ubicamos las variables
#que vamos a utilizar, entonces el data frame o marco de datos
#nos permite crear una nueva base con las variables que se van a usar 
#En este caso vamos a utilizar las siguientes variables: 

#SDEMT319: R_DEF(Entrevista Completa), C_RES(Residentes Definitivos), EDA(Mayores de 15 años), CLASE2(Población ocupada), MH_FIL2(Trab. Princ) 
#COE2T319: P4A (Subsector, Clasificador del SCIAN)

#Además del Factor de expansion (FAC)

################# indicadores seleccionados #####################
#############seleccionamos los cuestionarios terminados#####
enoetotal$R_DEF.x<- as.numeric (as.character (enoetotal$R_DEF.x)) ### la variable se convierte en numerico
enoetotal$C_RES<- as.numeric (as.character (enoetotal$C_RES)) ### la variable se convierte en numerico
enoetotal1 <- subset (enoetotal, (enoetotal$R_DEF.x == 0) & (enoetotal$C_RES== 1 |  enoetotal$C_RES==3))
########################### seleccionamos poblacion 15 aÃ±os o mas ############3
enoetotal1$EDA <- as.numeric(as.character(enoetotal1$EDA))
enoetotal1 <- subset (enoetotal1, EDA >= 15 & EDA <= 98) ###

########################## Incorporamos el esquema de muestreo #############

mydesign<-svydesign(id=~UPM, strata=~EST_D, weight=~FAC, data=enoetotal1, nest=TRUE)
options(survey.lonely.psu="adjust")

########################## Tasa de desocupaci?n#############
# Tasa de Desocupaci?n, considera a la poblaci?n que se encuentra sin trabajar, pero que est? buscando trabajo


tasaDesocupacion<-function(data){
  c2_jo<-wtd.table (data$CLASE2, weights=data$FAC)
  
  print(((c2_jo[2]/(c2_jo[1]+c2_jo[2]))*100))
}

# Generamos una variable "ocupado"
enoetotal1$ocupado<-0
enoetotal1$ocupado[enoetotal1$CLASE2==2]<-1

# Generamos la variable pea
enoetotal1$pea<-0
enoetotal1$pea[enoetotal1$CLASE2==1 | enoetotal1$CLASE2==2]<-1

(myratio <- svyratio(~ocupado, ~pea, mydesign))

##### Tasa de Participaci?n ######
## Tasa de Participaci?n, representa a la poblaci?n econ?micamente activa (PEA) respecto a la de 15 y m?s a?os de edad.
#Obtiene la variable CLASE1(PEA y PNEA) de enoetotal1, a la cual asigna etiquetas a los 
#niveles: 1=PEA y 2=PNEA

tasaParticipacion<- function(data){
  c1<-wtd.table(data$CLASE1, weights=data$FAC)
  
  ((c1[1]/(c1[1]+c1[2]))*100)
}


##### Tasa de Ocupaci?n Parcial y Desocupaci?n
# Tasa de Ocupaci?n Parcial y Desocupaci?n, considera a la poblaci?n desocupada y la ocupada que trabaj? menos de 15 horas a la semana
tasaOcupacionParcial<- function(data){
  c1<-wtd.table(data$CLASE1, weights=data$FAC)
  c2_jo<-wtd.table (data$CLASE2, weights=data$FAC)
  topd<-wtd.table(data$CLASE1,data$DUR9C,weights = data$FAC)
  ((c2_jo[2]+topd[1,3])/c1[1])*100
}

##### Tasa de Presi?n General
# Tasa de Presi?n General, incluye adem?s de los desocupados, a los ocupados que buscan empleo.

tasaPresionGeneral<-function(data){
  c1<-wtd.table(data$CLASE1, weights=data$FAC)
  c2_jo<-wtd.table (data$CLASE2, weights=data$FAC)
  buscar<-wtd.table(data$BUSCAR5C,weights = data$FAC)
  
  ((c2_jo[2]+sum(buscar[2:4]))/c1[1])*100
}


##### Tasa de Trabajo Asalariado
# Tasa de Trabajo Asalariado, representa a la poblaci?n que percibe de la unidad econ?mica para la que trabaja un sueldo, salario o jornal, por las actividades realizadas.

tasaTrabajoAsalariado<-function(data){
  c1<-wtd.table(data$CLASE1, weights=data$FAC)
  c2_jo<-wtd.table (data$CLASE2, weights=data$FAC)
  remune<-wtd.table(data$REMUNE2C,weights = data$FAC)
  
  (sum(remune[2])/c2_jo[1])*100
}


##### Tasa de Subocupaci?n
# Tasa de Subocupaci?n, porcentaje de la poblaci?n ocupada que tiene la necesidad y disponibilidad de ofertar m?s tiempo de trabajo de lo que su ocupaci?n actual le permite.

tasaSubocupacion<-function(data){
  c1<-wtd.table(data$CLASE1, weights=data$FAC)
  c2_jo<-wtd.table (data$CLASE2, weights=data$FAC)
  subocu<-wtd.table(data$SUB_O,weights = data$FAC)
  
  (subocu[2]/c2_jo[1])*100
}


##### Tasa de Condiciones Cr?ticas de Ocupaci?n
# Tasa de Condiciones Cr?ticas de Ocupaci?n, incluye a las personas que se encuentran trabajando menos de 35 horas a la semana por razones ajenas a sus decisiones, m?s las que trabajan m?s de 35 horas semanales
# con ingresos mensuales inferiores al salario m?nimo y las que laboran m?s de 48 horas semanales ganando hasta dos salarios m?nimos.

# 1- Personas que se encuentran trabajando menos de 35 horas a la semana
# 2.-Trabajan m?s de 35 horas semanales con ingresos mensuales inferiores al salario m?nimo y 
# 3.- Las que laboran m?s de 48 horas semanales ganando hasta dos salarios m?nimos
TCCO<-function(data){
  c1<-wtd.table(data$CLASE1, weights=data$FAC)
  c2_jo<-wtd.table (data$CLASE2, weights=data$FAC)
  TCCO_Nom<-wtd.table(data$TCCO,weights = data$FAC)
  (sum(TCCO_Nom[2:4])/c2_jo[1])*100
} 

# 1- Personas que se encuentran trabajando menos de 35 horas a la semana(menos35hrs)
menos35hrs<-function(data){
 TCCO_Nom<-wtd.table(data$TCCO,weights = data$FAC)
(TCCO_Nom[2])
} 
# 2.-Trabajan m?s de 35 horas semanales con ingresos mensuales inferiores al salario m?nimo(mas35hrs1SM) 
mas35hrs1SM<-function(data){
  TCCO_Nom<-wtd.table(data$TCCO,weights = data$FAC)
  (TCCO_Nom[3])
}
# 3.- Las que laboran m?s de 48 horas semanales ganando hasta dos salarios m?nimos(mas48hrs2SM)
mas48hrs2SM<-function(data){
  TCCO_Nom<-wtd.table(data$TCCO,weights = data$FAC)
  (TCCO_Nom[3])
}
### Poblaci?n Econ?micamente Activa (PEA)
PEA<-function(data){
  c2_jo<-wtd.table (data$CLASE2, weights=data$FAC)
  (c2_jo[1])
} 

##### Tasa de Informalidad Laboral 
# Tasa de Informalidad Laboral 1, se refiere a la suma, sin duplicar, de los ocupados que son laboralmente vulnerables por la naturaleza de la unidad econ?mica para la que trabajan, 
# con aquellos cuyo v?nculo o dependencia laboral no es reconocido por su fuente de trabajo. As?, en esta tasa se incluye -adem?s del componente que labora en micronegocios no registrados o 
# sector informal- a otras modalidades an?logas como los ocupados por cuenta propia en la agricultura de subsistencia, as? como a trabajadores que laboran sin la protecci?n de la seguridad social 
# y cuyos servicios son utilizados por unidades econ?micas registradas.

tasaInformalidadLaboral<-function(data){
  c1<-wtd.table(data$CLASE1, weights=data$FAC)
  c2_jo<-wtd.table (data$CLASE2, weights=data$FAC)
  TIL<-wtd.table(data$EMP_PPAL,weights = data$FAC)
  (TIL[2]/c2_jo[1])*100
}


##### Tasa de Ocupaci?n en el Sector Informal
# Tasa de Ocupaci?n en el Sector Informal 1, representa a la poblaci?n ocupada que trabaja para una unidad econ?mica que opera a partir de los recursos del hogar, pero sin constituirse como empresa, 
# de modo que la actividad no tiene una situaci?n identificable e independiente de ese hogar.

tasaOcupacionSectorInf<-function(data){
  c1<-wtd.table(data$CLASE1, weights=data$FAC)
  c2_jo<-wtd.table (data$CLASE2, weights=data$FAC)
  TOSI<-wtd.table(data$TUE_PPAL,weights = data$FAC)
  (TOSI[2]/c2_jo[1])*100
}

#### Generamos las subases para cada segmento de edad

juvenil<-subset(enoetotal1,EDA >= 18 & EDA <= 29)
mayores<-subset(enoetotal1,EDA >= 30 & EDA <= 98)
##### Preparamos data frame

`Tasa de Participaci?n`<-c(tasaParticipacion(enoetotal1),tasaParticipacion(juvenil),tasaParticipacion(mayores))
`Tasa de Desocupaci?n`<-c(tasaDesocupacion(enoetotal1),tasaDesocupacion(juvenil),tasaDesocupacion(mayores))
`Tasa de Ocupaci?n Parcial y Desocupaci?n`<-c(tasaOcupacionParcial(enoetotal1),tasaOcupacionParcial(juvenil),tasaOcupacionParcial(mayores))
`Tasa de Presi?n General`<-c(tasaPresionGeneral(enoetotal1),tasaPresionGeneral(juvenil),tasaPresionGeneral(mayores))
`Tasa de Trabajo Asalariado`<-c(tasaTrabajoAsalariado(enoetotal1),tasaTrabajoAsalariado(juvenil),tasaTrabajoAsalariado(mayores))
`Tasa de Subocupaci?n`<-c(tasaSubocupacion(enoetotal1),tasaSubocupacion(juvenil),tasaSubocupacion(mayores))
`Tasa de Condiciones Cr?ticas de Ocupaci?n`<-c(TCCO(enoetotal1),TCCO(juvenil),TCCO(mayores))
`Trabajando menos de 35 horas a la semana`<-c(menos35hrs(enoetotal1),menos35hrs(juvenil),menos35hrs(mayores))
`Trabajan m?s de 35 horas semanales con ingresos mensuales inferiores al SM`<-c(mas35hrs1SM(enoetotal1),mas35hrs1SM(juvenil),mas35hrs1SM(mayores))
`Laboran m?s de 48 horas semanales ganando hasta dos SM`<-c(mas48hrs2SM(enoetotal1),mas48hrs2SM(juvenil),mas48hrs2SM(mayores))
`Poblaci?n econ?micamente activa`<-c(PEA(enoetotal1),PEA(juvenil),PEA(mayores))
`Tasa de Informalidad Laboral`<-c(tasaInformalidadLaboral(enoetotal1),tasaInformalidadLaboral(juvenil),tasaInformalidadLaboral(mayores))
`Tasa de Ocupaci?n en el Sector Informal`<-c(tasaOcupacionSectorInf(enoetotal1),tasaOcupacionSectorInf(juvenil),tasaOcupacionSectorInf(mayores))

ano<-as.numeric(paste("20",(substring(stri_extract_first_regex(cuestionario, "[0-9]+"),2,3)),sep = ""))
year<-c(ano,ano,ano)
Grupo<-c("General","18-29","30ymas")
trim<-as.numeric(substring(stri_extract_first_regex(cuestionario, "[0-9]+"),1,1))
trimestre<-c(trim,trim,trim)

datosResumen<-data.frame(year,trimestre,Grupo,`Tasa de Participaci?n`,
                         `Tasa de Desocupaci?n`,`Tasa de Ocupaci?n Parcial y Desocupaci?n`,
                         `Tasa de Presi?n General`,`Tasa de Trabajo Asalariado`,
                         `Tasa de Subocupaci?n`,`Tasa de Condiciones Cr?ticas de Ocupaci?n`,
                         `Trabajando menos de 35 horas a la semana`,
                         `Trabajan m?s de 35 horas semanales con ingresos mensuales inferiores al SM`,
                         `Laboran m?s de 48 horas semanales ganando hasta dos SM`,
                         `Poblaci?n econ?micamente activa`,
                         `Tasa de Informalidad Laboral`,`Tasa de Ocupaci?n en el Sector Informal`)



## Utilizamos un for para todos los trimestres

listofdfs <- list() #Create a list in which you intend to save your df's.
system.time(
for(i in 1:55){
  
  df<-totalFuncion(lista_enoe[i])
  listofdfs[[i]] <- df
})

## Convertimos la lista a dataframe

library (plyr)

enoe_05_18 <- ldply (listofdfs, data.frame)

detach("package:plyr", unload=TRUE)

save(enoe_05_18,file = "C:/Users/End User/Desktop/Maestr?a/Tesis/Municipal/Tasas Estatales ENOE/enoe_05_18.RData")

## Hacemos una serie de tiempo
library(ggplot2)

enoe_05_18$mes<-0
enoe_05_18$mes[enoe_05_18$trimestre==1]<-3
enoe_05_18$mes[enoe_05_18$trimestre==2]<-6
enoe_05_18$mes[enoe_05_18$trimestre==3]<-9
enoe_05_18$mes[enoe_05_18$trimestre==4]<-12

enoe_05_18$Time<-as.Date(paste(enoe_05_18$year,"/",enoe_05_18$mes,"/","1",sep = ""))

ggplot(enoe_05_18, aes(x = Time, y = Tasa.de.Desocupaci?n)) + 
  geom_line(aes(color = Grupo), size = 1) +
  scale_color_manual(values = c("#FC4E07" ,"#E7B800","#00AFBB" )) +
  theme_minimal()

# Integramos el arima census 13 a ggplot2
library(ggseas)

# Tasa de desocupaci?n
ggplot(enoe_05_18, aes(x = Time, y = Tasa.de.Desocupaci?n, colour = Grupo)) +
  geom_point() +
  stat_seas(start = c(2005, 1), frequency = 4,size=.8) +  
  scale_color_manual(values = c("#FC4E07" ,"#E7B800","#00AFBB" )) +
  theme_minimal()+labs(x = "Time", y = "Porcentaje (%)")

# Tasa.de.Presi?n.General
ggplot(enoe_05_18, aes(x = Time, y = Tasa.de.Presi?n.General, colour = Grupo)) +
  geom_point() +
  stat_seas(start = c(2005, 1), frequency = 4,size=.8) +  
  scale_color_manual(values = c("#FC4E07" ,"#E7B800","#00AFBB" )) +
  theme_minimal()+labs(x = "Time", y = "Porcentaje (%)")

# Tasa.de.Subocupaci?n
ggplot(enoe_05_18, aes(x = Time, y = Tasa.de.Subocupaci?n, colour = Grupo)) +
  geom_point() +
  stat_seas(start = c(2005, 1), frequency = 4,size=.8) +  
  scale_color_manual(values = c("#FC4E07" ,"#E7B800","#00AFBB" )) +
  theme_minimal()+labs(x = "Time", y = "Porcentaje (%)")

# Tasa.de.Informalidad.Laboral
ggplot(enoe_05_18, aes(x = Time, y = Tasa.de.Informalidad.Laboral, colour = Grupo)) +
  geom_point() +
  stat_seas(start = c(2005, 1), frequency = 4,size=.8) +  
  scale_color_manual(values = c("#FC4E07" ,"#E7B800","#00AFBB" )) +
  theme_minimal()+labs(x = "Time", y = "Porcentaje (%)")

# Tasa.de.Participaci?n
ggplot(enoe_05_18, aes(x = Time, y =Tasa.de.Participaci?n , colour = Grupo)) +
  geom_point() +
  stat_seas(start = c(2005, 1), frequency = 4,size=.8) +  
  scale_color_manual(values = c("#FC4E07" ,"#E7B800","#00AFBB" )) +
  theme_minimal()+labs(x = "Time", y = "Porcentaje (%)")

# Tasa.de.Ocupaci?n.Parcial.y.Desocupaci?n
ggplot(enoe_05_18, aes(x = Time, y = Tasa.de.Ocupaci?n.Parcial.y.Desocupaci?n, colour = Grupo)) +
  geom_point() +
  stat_seas(start = c(2005, 1), frequency = 4,size=.8) +  
  scale_color_manual(values = c("#FC4E07" ,"#E7B800","#00AFBB" )) +
  theme_minimal()+labs(x = "Time", y = "Porcentaje (%)")

# Tasa.de.Trabajo.Asalariado
ggplot(enoe_05_18, aes(x = Time, y = Tasa.de.Trabajo.Asalariado, colour = Grupo)) +
  geom_point() +
  stat_seas(start = c(2005, 1), frequency = 4,size=.8) +  
  scale_color_manual(values = c("#FC4E07" ,"#E7B800","#00AFBB" )) +
  theme_minimal()+labs(x = "Time", y = "Porcentaje (%)")

# Tasa.de.Condiciones.Cr?ticas.de.Ocupaci?n
ggplot(enoe_05_18, aes(x = Time, y = Tasa.de.Condiciones.Cr?ticas.de.Ocupaci?n , colour = Grupo)) +
  geom_point() +
  stat_seas(start = c(2005, 1), frequency = 4,size=.8) +  
  scale_color_manual(values = c("#FC4E07" ,"#E7B800","#00AFBB" )) +
  theme_minimal()+labs(x = "Time", y = "Porcentaje (%)")

# Tasa.de.Ocupaci?n.en.el.Sector.Informal
ggplot(enoe_05_18, aes(x = Time, y = Tasa.de.Ocupaci?n.en.el.Sector.Informal, colour = Grupo)) +
  geom_point() +
  stat_seas(start = c(2005, 1), frequency = 4,size=.8) +  
  scale_color_manual(values = c("#FC4E07" ,"#E7B800","#00AFBB" )) +
  theme_minimal()+labs(x = "Time", y = "Porcentaje (%)")