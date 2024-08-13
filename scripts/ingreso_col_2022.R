here::i_am("scripts/ingreso_col_2022.R")
library(here)
here()
library(haven)
library(dplyr)
library(ggplot2)
library(dineq)
library(DescTools)
library(stats)

per<-read_dta(here("datos","PERSONAS.dta"))
hog<-read_dta(here("datos","HOGARES.dta"))

per<-per%>%select(c(directorio,secuencia_p,p6040,p6050,impa,isa,ie,imdi,iof1,iof2,iof3h,iof3i,iof6,
                    impaes,isaes,iees,imdies,iof1es,iof2es,iof3hes,iof3ies,iof6es,ingtotob,ingtotes,ingtot,fex_c))
hog<-hog%>%select(c(directorio,secuencia_p,npersug,ingtotug,ingtotugarr,ingpcug,li,lp,pobre,indigente,npobres,nindigentes))

comb<-per%>%left_join(hog,by=c("directorio","secuencia_p"))

# Densidad ingreso por unidad de gasto

mean(comb$ingpcug)
weighted.mean(comb$ingpcug, comb$fex_c)
quantile(comb$ingpcug,probs=c(0.25,0.5,0.75,0.9))
Quantile(comb$ingpcug,weights=comb$fex_c,probs=c(0.25,0.5,0.75,0.9))



ingpdf.plot<-ggplot(comb,aes(x=ingpcug, weight=fex_c))+geom_density(fill="gray")+coord_cartesian(xlim = c(0, 2000000))+theme_classic()
ingpdf.plot  

ingcdf.plot<-ggplot(comb,aes(x=ingpcug, weight=fex_c))+stat_ecdf(geom = "step",pad = FALSE)+coord_cartesian(xlim = c(0,2000000 ))
ingcdf.plot  

ingpdf.plot+geom_vline(xintercept=weighted.mean(comb$ingpcug,comb$fex_c),linetype="dashed", colour="blue")+
  geom_vline(xintercept=median(comb$ingpcug),linetype="dashed",colour="red")+
  geom_vline(xintercept=mean(comb$lp),linetype="dashed",colour="green")+
  geom_vline(xintercept=mean(comb$li),linetype="dashed",colour="yellow")

ingcdf.plot+geom_vline(xintercept=weighted.mean(comb$ingpcug,comb$fex_c),linetype="dashed", colour="blue")+
  geom_vline(xintercept=median(comb$ingpcug),linetype="dashed",colour="red")+
  geom_vline(xintercept=mean(comb$lp),linetype="dashed",colour="green")+
  geom_vline(xintercept=mean(comb$li),linetype="dashed",colour="yellow")


ggplot(comb,aes(x=ingpcug, weight=fex_c))+geom_histogram(fill="gray",binwidth=100000,color="white")+coord_cartesian(xlim = c(0, 5000000))+
  theme_classic()+
  geom_vline(xintercept=weighted.mean(comb$ingpcug,comb$fex_c),linetype="dashed", colour="blue")+
  geom_vline(xintercept=median(comb$ingpcug),linetype="dashed",colour="red")+
  geom_vline(xintercept=mean(comb$lp),linetype="dashed",colour="green")+
  geom_vline(xintercept=mean(comb$li),linetype="dashed",colour="yellow")


# Composición del ingreso por hogar

comb[is.na(comb)]<-0
compy<-comb%>%group_by(directorio, secuencia_p)%>%summarise(ylab=sum(impa, na.rm=TRUE)+sum(impaes,na.rm=TRUE)+sum(isa,na.rm=TRUE)+sum(isaes,rm=TRUE)+sum(imdi,na.rm=TRUE)+
                                                             sum(imdies,na.rm=TRUE),
                                                            yk=sum(iof1,na.rm=TRUE)+sum(iof1es,na.rm=TRUE)+sum(iof6,na.rm=TRUE)+sum(iof6es),
                                                            ytr=sum(iof3i,na.rm=TRUE)+sum(iof3ies,na.rm=TRUE),
                                                            ingtotugarr=mean(ingtotugarr),ingpcug=mean(ingpcug),
                                                            pobre=mean(pobre), fex_c=mean(fex_c))%>%ungroup()
compy<-compy%>%mutate(yresto=ingtotugarr-ylab+1-yk-ytr,
                      ylabp=ylab/ingtotugarr,
                      ykp=yk/ingtotugarr,
                      ytrp=ytr/ingtotugarr,
                      yrestop=yresto/ingtotugarr)
compy<-compy%>%filter(ingtotugarr>0)

compyt<-compy%>%group_by(pobre)%>%summarise(ylab=weighted.mean(ylabp,fex_c),yk=weighted.mean(ykp,fex_c),
                                            ytr=weighted.mean(ytrp,fex_c),yresto=weighted.mean(yrestop,fex_c))

# Curva de lorenz

lorenz<-comb[order(comb$ingpcug),]
g10<-round(nrow(lorenz)/10)
lorenz["grupo"]<-0

    lorenz[1:g10,'grupo']<-1
    for(i in 1:9){
      f<-(g10*i)+1
      l<-(i+1)*g10
      g<-i+1
    lorenz[f:l,'grupo']<-i+1
    }
    lorenz[865963,14]<-10
  
lorenzs<-lorenz%>%group_by(grupo)%>%summarise(ingd=sum(ingpcug))%>%ungroup()%>%mutate(ingdc=cumsum(ingd))
lorenzs<-lorenzs%>%mutate(indgp=ingd/sum(ingd), ingdcp=ingdc/sum(ingd))
new_row<-c(grupo=0,ingd=0,ingdc=0,ingp=0,ingdcp=0)
lorenzs<-rbind(lorenzs,new_row)
lorenzs<-lorenzs[order(lorenzs$grupo),]
plot(lorenzs$grupo,lorenzs$ingdcp,type="line",col="blue",lwd=2,xlim=c(0,10))
curve(0.1*x,add=TRUE,lwd=2)

gini.wtd(compy$ingtotugarr)
gini.wtd(compy$ylab)
gini.wtd(comb$ingpcug)
