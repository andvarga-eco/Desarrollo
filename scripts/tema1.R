here::i_am("scripts/tema1.R")
library(here)
here()
library(dplyr)
library(ggplot2)
library(readxl)
library(forcats)

# Ingreso per cápita 
gni_2020_2022 <- read_excel(here("datos","gni_2020-2022.xlsx"))

gni2020<-gni_2020_2022%>%subset(select=c(`Country Name`,`Country Code`,`YR2020`))
gni2020<-gni2020%>%mutate(YR2020=as.numeric(YR2020))%>%arrange(YR2020)%>%mutate(ccode=`Country Code`%>%fct_reorder(YR2020,.desc=FALSE))
gni2020<-gni2020%>%mutate(rank=seq(from=1,to=nrow(gni2020),by=1), col=as.factor(ifelse(ccode=="COL",1,0)))
gni2020%>%na.omit()%>%ggplot(aes(x=rank,y=YR2020,fill=col))+geom_bar(stat="identity")+
  scale_fill_manual(values = c( "1"="#336600", "0"="darkgray" ),guide="none")+theme_minimal()+
  labs(title="Ingreso per cápita 2020", caption="WDI, Banco Mundial")+xlab("Ranking de paises")+ylab("USD PPP 2017")

gni2020%>%na.omit()%>%ggplot(aes(x=YR2020))+geom_histogram(bins =50)

gni2020%>%na.omit()%>%ggplot(aes(x=YR2020))+geom_bar()+scale_x_binned(n.breaks=20)+
  labs(title="Ingreso per cápita 2020", caption="WDI, Banco Mundial")+xlab("Rango de ingresos")+ylab("# Países")+theme_minimal()

# Pobreza

pov<-read_excel(here("datos","poverty_1921.xlsx"))
pov<-pov%>%mutate(p_215=as.numeric(p_215),p_365=as.numeric(p_365),p_685=as.numeric(p_685))
pov20<-pov%>%filter(Time==2020)%>%na.omit()

pov20<-pov20%>%left_join(gni2020,by="Country Code")

ggplot(pov20,aes(x=YR2020,y=p_215))+geom_point()
ggplot(pov20,aes(x=YR2020))+geom_point(aes(y=p_215,colour="US$2.15"))+geom_point(aes(y=p_365,colour="US$3.65"))+
  geom_point(aes(y=p_685,colour="US$6.85"))+
  scale_color_manual("",breaks=c("US$2.15","US$3.65","US$6.85"),values=c("US$2.15"="black","US$3.65"="#CC0000","US$6.85"="#009999"))+
  labs(title="Incidencia de pobreza 2020", caption="WDI, Banco Mundial")+xlab("Ingreso Percápita (US$ PPP, 2017)")+ylab("% Población en pobreza")+theme_minimal()

       