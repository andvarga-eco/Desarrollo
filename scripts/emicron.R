library(dplyr)
library(readr)
library(ggplot2)

# Cargar los datos
id<- read_csv("datos/EMICRON 2023/Módulo de identificación.csv")
carac<- read_csv("datos/EMICRON 2023/Módulo de características del micronegocio.csv")
personal<- read_csv("datos/EMICRON 2023/Módulo de personal ocupado.csv")
ingreso<- read_csv("datos/EMICRON 2023/Módulo de ventas o ingresos.csv")

# Identificar unidades económicas de patrono/empleador. No autoempleo

micro<-id%>%filter(P3033==1)

# Seleccionar variables de interés

carac<-carac%>%select(c(DIRECTORIO,SECUENCIA_P,SECUENCIA_ENCUESTA,P1633,P1055,))
personal<-personal%>%select(c(DIRECTORIO,SECUENCIA_P,SECUENCIA_ENCUESTA,TIPO,P3080,P3084))
ingreso<-ingreso%>%select(c(DIRECTORIO,SECUENCIA_P,SECUENCIA_ENCUESTA,VENTAS_MES_ANTERIOR))

# Pegar las bases de datos y crear nuevas variables

micro<-micro%>%left_join(carac,by=c("DIRECTORIO","SECUENCIA_P","SECUENCIA_ENCUESTA"))
micro<-micro%>%left_join(ingreso,by=c("DIRECTORIO","SECUENCIA_P","SECUENCIA_ENCUESTA"))
micro<-micro%>%mutate(empleo=P3032_1+P3032_2+P3032_3+1,
                      vpe=VENTAS_MES_ANTERIOR/empleo)

# Análisis comparativo entre firmas formales e informales

micro%>%group_by(P1633)%>%summarise(weighted.mean(empleo,F_EXP,na.rm=TRUE),
                                    weighted.mean(VENTAS_MES_ANTERIOR,F_EXP,na.rm=TRUE),
                                    weighted.mean(vpe,F_EXP,na.rm=TRUE))

ggplot(micro,aes(x=empleo,group=factor(P1633),fill=factor(P1633)))+
  geom_density(alpha=0.4)

ggplot(micro,aes(x=log(VENTAS_MES_ANTERIOR),group=factor(P1633),fill=factor(P1633)))+
  geom_density(alpha=0.4)

ggplot(micro,aes(x=log(vpe),group=factor(P1633),fill=factor(P1633)))+
  geom_density(alpha=0.4)

# Crear base de datos para análisis de margen intensivo

margen<-micro%>%left_join(personal,by=c("DIRECTORIO","SECUENCIA_P","SECUENCIA_ENCUESTA"))
margen<-margen%>%group_by(id=consecutive_id(DIRECTORIO,SECUENCIA_P,SECUENCIA_ENCUESTA),DIRECTORIO,SECUENCIA_P,SECUENCIA_ENCUESTA)
margen<-margen%>%arrange(id)

# Definición de firma informal y empleo informal

margen<-margen%>%mutate(inf=ifelse(P1633==2,1,0),
                        infemp=ifelse(P3080==1,0,1))

# Proporción de trabajadores informales por tipo de firma

margen%>%group_by(inf)%>%filter(TIPO==1)%>%summarise(weighted.mean(infemp,F_EXP,na.rm=TRUE))

# Modelo de probabilidad lineal

m1<-lm(infemp~inf+empleo+log(vpe+1)+factor(GRUPOS4),data=margen)
summary(m1)
