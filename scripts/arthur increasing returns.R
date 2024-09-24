library(dplyr)
library(ggplot2)
#parametros

ar<-10
r<-3
br<-4
as<-4
bs<-10
s<-3

#Regla de elección. Cada tipo de agente compara beneficios de cada tecnología y elige la mejor

##En cada momento de elección entra aleatoriamente un agente R(0) o S(1)

simul<-numeric(100)
for (k in 1:100){
N<-round(runif(200,min=0,max=1))
df<-matrix(,200,5)
df<-data.frame(N,df)
names(df)[2]<-"BA"
names(df)[3]<-"BB"
names(df)[4]<-"choice"
names(df)[5]<-"na"
names(df)[6]<-"nb"
#Valor inicial
na<-0
nb<-0
agent<-df[1,1]
BA<-ifelse(agent==0,ar+r*na,as+s*na)
BB<-ifelse(agent==0,br+r*nb,bs+s*nb)
choice<-ifelse(BA>BB,"A","B")
df[1,2]<-BA
df[1,3]<-BB
df[1,4]<-choice

# valores siguientes

for(i in 1:200){
  choice<-c("A","B")
  choice<-data.frame(choice)
n<-df%>%group_by(choice)%>%summarise(count=n())
choice<-left_join(choice,n,by="choice")
choice[is.na(choice)]<-0
df[i,5]<-choice[1,2]
df[i,6]<-choice[2,2]
na<-as.numeric(df[i,5])
nb<-as.numeric(df[i,6])
agent<-df[i+1,1]
BA<-ifelse(agent==0,ar+r*na,as+s*na)
BB<-ifelse(agent==0,br+r*nb,bs+s*nb)
choice<-ifelse(BA>BB,"A","B")
df[i+1,2]<-BA
df[i+1,3]<-BB
df[i+1,4]<-choice
}

df<-df%>%mutate(pa=na/(na+nb))
simul[k]<-df[200,7]
}
#plot(df[2:200,7],type="line")
