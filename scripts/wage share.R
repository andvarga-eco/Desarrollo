library(haven)
here::i_am("scripts/wage share.R")
library(here)
here()
library(dplyr)
library(ggplot2)

ws<-read_dta(here("datos","pwt1001-labor-detail.dta"))

col<-ws%>%filter(countrycode=="COL")

ggplot(col,aes(x=year))+geom_line(aes(y=comp_sh, colour="Empleados"))+
  geom_line(aes(y=lab_sh2,colour="Empleados + Mixto"))+
scale_color_manual("",breaks=c("Empleados","Empleados + Mixto"),
                   values=c("Empleados"="black",
                            "Empleados + Mixto"="#CC0000"))+
  labs(title="Participación del salario: Colombia", caption="PWT 10.1")+
  ylab("Proporción en el ingreso")+xlab("")+
  theme_minimal()+theme(legend.position="bottom")

us<-ws%>%filter(countrycode=="USA")

ggplot(us,aes(x=year))+geom_line(aes(y=comp_sh, colour="Empleados"))+
  geom_line(aes(y=lab_sh2,colour="Empleados + Mixto"))+
  scale_color_manual("",breaks=c("Empleados","Empleados + Mixto"),
                     values=c("Empleados"="black",
                              "Empleados + Mixto"="#CC0000"))+
  labs(title="Participación del salario: EE.UU", caption="PWT 10.1")+
  ylab("Proporción en el ingreso")+xlab("")+
  theme_minimal()+theme(legend.position="bottom")
