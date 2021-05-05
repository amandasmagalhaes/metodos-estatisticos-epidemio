# Análise de Sobrevivência





#Livro: Analise de Sobrevivencia Aplicada			
#Autores: Enrico Antonio Colosimo e Suely Ruiz Giolo	
#E-mail: enricoc@est.ufmg.br e  giolo@ufpr.br
#Editora: Edgar Blucher	(www.blucher.com.br)					
#Ano Publicação: 2006   Edicao: 1a.

#CRAN Task View: Survival Analysis
#http://finzi.psych.upenn.edu/views/Survival.html



# Remove todos os objetos definidos
rm(list = ls())



# Pacotes
if (!require(pacman)) install.packages('pacman')
library(pacman)
pacman::p_load(survival, km.ci)



# Exemplo da Hepatite (placebo e tratado)

tempos <- c(1,2,3,3,3,5,5,16,16,16,16,16,16,16,16,1,1,1,1,4,5,7,8,10,10,12,16,16,16)
cens <- c(0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,1,1,1,0,0,0,0,0)
grupos <- c(rep(1,15),rep(2,14))



# KAPLAN-MEIER

ekm <- survfit(Surv(tempos,cens)~grupos)
ekm$surv
summary(ekm) #IC calculado utilizando a transformação log

plot(ekm, lty=c(2,1), xlab="Tempo (semanas)", ylab="S(t) estimada")
legend(1,0.3,lty=c(2,1), c("Controle","Esteróide"), lwd=1, bty="n")
lines(ekm, lwd=1, lty=c(2,1), conf.int=T)

plot(ekm, col = c(1, 2), xlab="Tempo (semanas)", ylab="S(t) estimada")
legend(1, 0.3, col=c(1, 2), c("Controle","Esteróide"), lwd = 1, bty = 'n')
lines(ekm, lwd=1, col=c(1, 2), conf.int=T)


ekm <- survfit(Surv(tempos,cens)~grupos,conf.type="log-log")
summary(ekm) #IC calculado utilizando a transformação log-log


ekm<- survfit(Surv(tempos,cens)~grupos,conf.type="plain")
summary(ekm) #IC sem transformação (deve ser evitado)



# Plot com Intervalo de Confiança para o Grupo Esteróide  
ss <- survfit(coxph(Surv(tempos[grupos==2],cens[grupos==2])~1,method="breslow"))
summary(ss)
plot(ss)



# Teste Logrank (rho=0) Wilcoxon (rho=1)

tempos <- c(1,2,3,3,3,5,5,16,16,16,16,16,16,16,16,1,1,1,1,4,5,7,8,10,10,12,16,16,16)
cens <- c(0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,1,1,1,0,0,0,0,0)
grupos <- c(rep(1,15),rep(2,14))

survdiff(Surv(tempos,cens)~grupos,rho=0) #logrank
#O valor do teste logrank para a comparação entre os dois grupos dos dados de hepatite é T = 3,67 
#o que implica em um valor p = 0,056, indicando uma prováavel diferença entre as 
#duas curvas de sobrevivência e, portanto, entre os grupos placebo e esteróide.

survdiff(Surv(tempos,cens)~grupos,rho=1) #apr. Wilcoxon/TaroneWare



################################################################################



# Estudo experimental com camundongos conduzido no Centro de Pesquisas Renée Rachou, FioCruz, MG.
#44 camundongos foram infectados pela malária (Plasmodium berguei).
#Os camundongos foram aleatoriamente alocados em três grupos:
##Grupo 1: infectado também pela esquistossomose e imunizado.
##Grupo 2: controle.
##Grupo 3: infectado também pela esquistossomose.
#Dados disponíveis na pag. 14 (Colosimo e Giolo, 2006).

#Outro Exemplo com três grupos - logrank com 3gl

# Remove todos os objetos definidos
rm(list = ls())

tempos<-c(7,8,8,8,8,12,12,17,18,22,30,30,30,30,30,30,8,8,9,10,10,14,
          15,15,18,19,21,22,22,23,25,8,8,8,8,8,8,9,10,10,10,11,17,19)
cens<-c(rep(1,10), rep(0,6),rep(1,15),rep(1,13))
grupo<-c(rep(1,16), rep(2,15), rep(3,13))
ekm<-survfit(Surv(tempos,cens)~grupo)
summary(ekm)
plot(ekm, lty=c(1,4,2), xlab="Tempo",ylab="S(t) estimada")
legend(1,0.3,lty=c(1,4,2),c("Grupo 1","Grupo 2", "Grupo 3"),lwd=1,
       bty="n",cex=0.8)
survdiff(Surv(tempos,cens)~grupo,rho=0) # logrank
#Os grupos 1 e 2 não se diferem mas ambos são significativamente diferentes do grupo 3.
survdiff(Surv(tempos,cens)~grupo,rho=1) # tipo Tarone-Ware
survdiff(Surv(tempos[1:31],cens[1:31])~grupo[1:31],rho=0)
survdiff(Surv(tempos[17:44],cens[17:44])~grupo[17:44],rho=0)
survdiff(Surv(c(tempos[1:16],tempos[32:44]),c(cens[1:16],
                                              cens[32:44]))~c(grupo[1:16],grupo[32:44]),rho=0)



################################################################################



# Estudo de Caso: Aleitamento Materno



# Remove todos os objetos definidos
rm(list = ls())



# Pacotes
if (!require(pacman)) install.packages('pacman')
library(pacman)
pacman::p_load(dplyr, survival, km.ci)



# Banco de dados
desmame <- read.table('http://www.est.ufmg.br/~enricoc/pdf/avancados_medicina/desmame.txt', 
                    head=T)
glimpse(desmame)



# KAPLAN-MEIER
ekm<- survfit(Surv(desmame$tempo,desmame$cens)~desmame$V1)
summary(ekm)
survdiff(Surv(desmame$tempo,desmame$cens)~desmame$V4,rho=0)
plot(ekm,lty=c(1,4),mark.time=F,xlab="Tempo até o desmame (meses)",
     ylab="S(t) estimada - KM")
text(17.5,0.93,c("Dificuldades para Amamentar"), bty="o",cex=0.85)
legend(13,0.9,lty=c(4),c("Sim"),bty="n",cex=0.8)
legend(18.5,0.9,lty=c(1),c("Não"),bty="n",cex=0.8)



fit<-coxph(Surv(tempo,cens)~V1+V3+V4+V6,data=desmame,x = T,method="breslow")
summary(fit)
names(fit)



# Teste de Schoenfeld de Adequação do Modelo

resid(fit,type="scaledsch")
cox.zph(fit, transform="identity")     ## g(t) = t
par(mfrow=c(2,2))
plot(cox.zph(fit))




# Técnicas Gráficas de Adequação do Modelo

par(mfrow=c(2,2))
fit1<-coxph(Surv(tempo[V1==0],cens[V1==0])~1,data=desmame,x=T,method="breslow")
ss<- survfit(fit1)
s0<-round(ss$surv,digits=5)
H0<- -log(s0)
plot(ss$time,log(H0),xlim=range(c(0,20)),xlab="Tempos",
     ylab=expression(log(Lambda[0]*(t))), bty="n",type="s")
fit2<-coxph(Surv(tempo[V1==1],cens[V1==1])~1,data=desmame,x=T,method="breslow")
ss<- survfit(fit2)
s0<-round(ss$surv,digits=5)
H0<- -log(s0)
lines(ss$time,log(H0),type="s",lty=2)
legend(10,-3,lty=c(2,1),c("V1 = 1 (Não)","V1 = 0 (Sim)"),lwd=1,bty="n",cex=0.5)
title("V1: Experiência Amamentação")

fit1<-coxph(Surv(tempo[V3==0],cens[V3==0])~1,data=desmame,x=T,method="breslow")
ss<- survfit(fit1)
s0<-round(ss$surv,digits=5)
H0<- -log(s0)
plot(ss$time,log(H0),xlim=range(c(0,20)),xlab="Tempos",
     ylab=expression(log(Lambda[0]*(t))), bty="n",type="s")
fit2<-coxph(Surv(tempo[V3==1],cens[V3==1])~1,data=desmame,x=T,method="breslow")
ss<- survfit(fit2)
s0<-round(ss$surv,digits=5)
H0<- -log(s0)
lines(ss$time,log(H0),type="s",lty=2)
legend(10,-3,lty=c(2,1),c("V3 = 1 (Não)","V3 = 0 (Sim)"),lwd=1,bty="n",cex=0.5)
title("V3: Conceito de Amamentação")



# Gráfico Censura

labels<-c('1','2','3','4','5','6')
ano<-c(2001,2002,2003,2004,2006,2008)
ranges<-c(6,3,4,5,4,2)
dotchart(ano, labels=labels, xlab='ano', pch=20,xlim=c(min(ano), max(ano+ranges)))
segments(ano,1:4,ano+ranges,1:4)
segments(ano,1:6,ano+ranges,1:6)
point(ano+ranges)
# Error: could not find function "point"
?points
x<-c(1,2,3,4)
y<-c(2001,2002,2003,2004)
points(x,y,pch=23)
#http://stackoverflow.com/questions/9962259/adding-spread-data-to-dotplots-in-r




