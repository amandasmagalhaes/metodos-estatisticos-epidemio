# Testes: Qui-quadrado e Mantel-Haenszel
# Modelo de Regressão Logística





# Pacotes
if (!require(pacman)) install.packages('pacman')
library(pacman)
pacman::p_load(dplyr, epitools, rms)



# Dados (Exemplo: AZT vs Placebo)
dados <- matrix(c(16, 1, 121, 144), nc = 2)
dimnames(dados) <- list(Grupo = c( "Placebo","AZT"), 
                        Óbito = c("Sim", "Não"))
dados

# Proporção (Exemplo: AZT vs Placebo)
round(prop.table(dados, 1), 2)

barplot(prop.table(dados, 1), beside = T, ylim = c(0,1.0),
        ylab = 'Probabilidade óbito', xlab = 'óbito', legend = T,
        args.legend = list(x = 'topleft'))



# Teste qui-quadrado (Exemplo: AZT vs Placebo)
chisq.test(dados, correct = F)

# Teste qui-quadrado com correção de continuidade (Exemplo: AZT vs Placebo)
chisq.test(dados)



# Testes que devem ser utilizados quando a condição E>5 não for atendida

#Teste Exato de Fisher (Exemplo: AZT vs Placebo)
fisher.test(dados)    

# Simulação Monte Carlo (Exemplo: AZT vs Placebo)
chisq.test(dados, correct = F, simulate.p.value = T, B = 2000)



# Razão de Chances (RC) e IC95% (OR) - Pacote epitools (Exemplo: AZT vs Placebo)
(dados[1, 1] * dados [2, 2]) / (dados[1, 2] * dados[2, 1])

oddsratio.wald(dados)



# Risco Relativo (RR) e IC95% (RR) (Exemplo: AZT vs Placebo)
dados
p11<-(dados[1,1]/(sum(dados[1,])))
p21<-(dados[2,1]/(sum(dados[2,])))
RR<-p11/p21
RR
vf1<-((1-p11)/(sum(dados[1,])*p11)) + ((1-p21)/(sum(dados[2,])*p21))
dpf1<-sqrt(vf1)
z<-qnorm(0.975)
li<-exp(log(RR)-z*dpf1)
li # limite inferior do IC
ls<-exp(log(RR)+z*dpf1)
ls # limite superior do IC





# Tabelas r por s (Exemplo: Diferença de idade vs sexo do primeiro filho)

#Dados (Exemplo: Diferença de idade vs sexo do primeiro filho)
dados <- matrix(c(14, 29, 117, 84, 37, 20), nc = 3)
dimnames(dados) <- list(Sexo = c('Menino', 'Menina'),
                        Dif.Idade = c('-9 a -1', '0 a 5', '5 a 15'))
dados

# Proporção (Exemplo: Diferença de idade vs sexo do primeiro filho)
round(prop.table(dados, 1), 2)

barplot(prop.table(dados,1),beside=T, ylim= c(0,1),xlab="Difernça de idade",
        ylab="probabilidade",legend=T)

barplot(c(dados[1,1]/sum(dados[,1]), dados[1,2]/sum(dados[,2]), dados[1,3]/sum(dados[,3])), 
        names.arg=c("-9 a -1", "0 a 5", "5 a 15"), xlab="Difernça de idade",
        ylab="proporção menino", col=c("pink", "green", "blue"))



# Teste qui-quadrado (Exemplo: Diferença de idade vs sexo do primeiro filho)
chisq.test(dados, correct = F)

names(chisq.test(dados, correct = F))



# Resíduos
chisq.test(dados, correct = F)$residuals

# Resíduos padronizados (mais indicado que os resíduos)
chisq.test(dados, correct = F)$stdres 

#Os resíduos indicam que tem mais meninas em casais cuja diferença de idade é 
#negativa (mãe mais velha do que o pai) do que a hipótese de independência prediz.
#Já que a diferença de idades está ordenada, podemos realizar um teste de tendência. 
#Ou seja, será que probabilidade de ocorrer menino aumenta com a diferença
#de idade entre Pai e Mãe (idade do pai - idade da mãe)?



# Teste de Tendência Linear
dados
escore<-c(-5,2.5,10)
fb1<-(sum(dados[1,]*escore))/sum(dados[1,])
fb2<-(sum(dados[2,]*escore))/sum(dados[2,])
esp<-(c(sum(dados[,1]),sum(dados[,2]),sum(dados[,3])))/sum(dados)
mua<-sum(escore*esp)
va<-sum((escore-mua)^2*esp)
vbf1<-((sum(dados) - sum(dados[1,]))/(sum(dados[1,])*(sum(dados)-1)))*va
QS = ((fb1-mua)^2)/vbf1

# QS
gl<-nrow(dados)-1
1-pchisq(QS,gl)

#O teste confirma a tendência linear, de aumento da probabilidade de nascer menino, 
#como primeiro filho, a medida que aumentamos a diferença de idade entre pai e mãe.



# Mantel Haenszel (QMH) em tabelas 2 x 2, ORMH e IC(ORMH)
#Exemplo Vendas Produto X - Paradoxo de Simpson

#Cidade A
dados1<-matrix(c(60,320,140,480),nc=2)
dados1
chisq.test(dados1,correct=F) #Teste qui-quadrado
(dados1[1,1]*dados1[2,2])/(dados1[1,2]*dados1[2,1]) #OR

#Cidade B
dados2<-matrix(c(640,180,160,20),nc=2)
dados2
chisq.test(dados2,correct=F) #Teste qui-quadrado
(dados2[1,1]*dados2[2,2])/(dados2[1,2]*dados2[2,1]) #OR

#Combinado (Cidade A + Cidade B)
dados3<-matrix(c(700,500,300,500),nc=2)
dados3
chisq.test(dados3,correct=F) #Teste qui-quadrado
(dados3[1,1]*dados3[2,2])/(dados3[1,2]*dados3[2,1]) #OR



# Teste de Mantel-Haenszel
tab<-array(c(60,320,140,480,640,180,160,20),dim=c(2,2,2))
tab
mantelhaen.test(tab, correct=T)

# Outro exemplo de sala de aula
tab1<-array(c(29,14,16,31,37,24,8,21),dim=c(2,2,2))
tab1
mantelhaen.test(tab1, correct=T)



# Regressão Logística Dicotômica
#Exemplo - sala de aula: Doença Cardíaca

#Vamos utilizar os dados brutos
#n=100 Coorte de 100 indivíduos #quebrando os empates de idade.
#Y (resposta): 0/1 (incidência de DC)
#X: idade
idade<-round(c(runif(10,20,29),runif(15,30,34),runif(12,35,39),
               runif(15,40,44),runif(13,45,49),runif(8,50,54),runif(17,55,59),
               runif(10,60,69)))
idade
y<-c(rep(1,1),rep(0,9),rep(1,2),rep(0,13),rep(1,3),rep(0,9),
     rep(1,5),rep(0,10),rep(1,6),rep(0,7),rep(1,5),rep(0,3),
     rep(1,13),rep(0,4),rep(1,8),rep(0,2))
y
sum(y)

# Gráfico de Dispersão
plot(idade,y,xlab="idade",ylab="ocorrência de DC")


# Ajuste de um Modelo Linear ---> Inadequado
outlm<-lm(y~idade)
summary(outlm)
par(mfrow=c(2,2))
plot(outlm)
summary(lm(abs(residuals(outlm))~fitted(outlm))) # teste de homocedasticidade
shapiro.test(outlm$resid) # teste de normalidade

# Indicação de violação de homocedásticidade e normalidade
par(mfrow=c(1,1))
plot(idade,y,abline(outlm$coefficients[1], outlm$coefficients[2]),
     xlab="idade", ylab="ocorrência de DC")


# Vamos agrupar as idades por faixa etária
resim<-c(1,2,3,5,6,5,13,8)
resnao<-c(9,13,9,10,7,3,4,2)
idade<-c(25,32,38,43,47,53,57,65)
dados<-cbind(resim, resnao,idade)
dados
dados<-as.data.frame(dados)

# *Outra Forma de Entrar com os Dados
x<-c(rep(25,10),rep(32,15),rep(38,12),
     rep(43,15),rep(47,13),rep(53,8),rep(57,17),
     rep(65,10))
y<-c(rep(1,1),rep(0,9),rep(1,2),rep(0,13),
     rep(1,3),rep(0,9),rep(1,5),rep(0,10),
     rep(1,6),rep(0,7),rep(1,5),rep(0,3),
     rep(1,13),rep(0,4),rep(1,8),rep(0,2))
dados1<-cbind(y,x)
dados1<-as.data.frame(dados1)
dados1


# Gráfico dos Dados Agrupado
theta<-resim/(resim+resnao)
dados2<-cbind(resim, resnao,idade,theta)
dados2
plot(idade,theta,ylim=range(0,0.9),xlab="idade",ylab="E(Y|x)",pch=16)
abline(outlm$coefficients[1], outlm$coefficients[2])

#O ajuste de regressão linear, a princípio, não parece mal. Entretanto,
#vemos que não é adequado para idades pequena e grande.



# Ajustando o Modelo de Regressão Logística Simples
ajust<-glm(as.matrix(dados[,c(1,2)])~idade,family=binomial, data=dados)

#Forma similar mostrando que o modelo logístico é o default
ajust<-glm(as.matrix(dados[,c(1,2)])~idade,family=binomial(link="logit"),data=dados)
summary(ajust)

anova(ajust,test="Chisq") # utilizando um teste alternativo.


# Fazendo o mesmo ajuste com os dados brutos (não agrupados)

ajust1<-glm(y~x,family="binomial",data=dados1)
summary(ajust1)
anova(ajust1,test="Chisq")



# Gráfico com os dados, ajuste logístico (vermelho) e ajuste linear
idade<-c(25,32,38,43,47,53,57,65)
plot(idade,theta,ylim=range(0,0.9),xlab="idade",ylab="E(Y|x)",pch=16)
idade<-20:70
modajust<-(exp(-5.123+0.1058*idade))/(1+ exp(-5.123+0.1058*idade))
modajust
lines(idade,modajust,col=2)
abline(outlm$coefficients[1], outlm$coefficients[2])
legend(55,0.35, c("Linear","Logístico"), lty=c(1,1), col=c("black","red"))



# Testes de Adequação do Modelo - Qui-quadrado e Desvio
ajust$fitted.values
ajust$y
ajust$residuals
dev<-residuals(ajust,type='deviance')
dev
QL<-sum(dev^2)
QL

# Valor-p para o teste de adequação do Desvio
p1<-1-pchisq(QL,ajust$df.residual)
p1

rpears<-residuals(ajust,type='pearson')
rpears
QP<-sum(rpears^2)
QP



# Valor-p para o teste de adequação do Qui-quadrado
p2<-1-pchisq(QP,ajust$df.residual)
p2
theta<-resim/(resim+resnao)

# Intervalo de 95% de confiança para RC
exp( ajust$coefficients[2])
LS<-exp(ajust$coefficients[2] + 1.96* 0.02337)
LI<-exp(ajust$coefficients[2] - 1.96* 0.02337)
LI
LS



# Exemplo 1 - Capítulo 7 - pg. 114 - sala de aula
#Y:0/1 (DC), X1: sexo, X2: ecg

resim<-c(4,8,9,21)
resnao<-c(11,10,9,6)
sexo<-c(0,0,1,1)
ecg<-c(0,1,0,1)
dados<-cbind(resim, resnao,sexo,ecg)
dados
dados<-as.data.frame(dados)

ajust<-glm(as.matrix(dados[,c(1,2)])~sexo+ecg,family=binomial(link="logit"),data=dados)
summary(ajust)
anova(ajust,test="Chisq")
names(ajust)
ajust$fitted.values
ajust$y
ajust$residuals
dev<-residuals(ajust,type='deviance')
dev
QL<-sum(dev^2)
QL
p1<-1-pchisq(QL,1)
p1
rpears<-residuals(ajust,type='pearson')
rpears
QP<-sum(rpears^2)
QP
p2<-1-pchisq(QP,1)
p2

ajust1<-
  glm(as.matrix(dados[,c(1,2)])~sexo+ecg+sexo*ecg,family=binomial(link="logit"),data=dados)
summary(ajust1)
ajust1
anova(ajust1, test = "Chisq")

# Gráfico de Interação
plot(c(0,1),ajust$y[1:2],type="l",ylim=range(0,0.9),xlab="ECG",
     ylab="E(Y|x)",pch=16)
lines(c(0,1),ajust$y[3:4],type="b")

# Intervalo de 95% de confiança para RC de sexo
exp( ajust$coefficients[2])
LS<-exp(ajust$coefficients[2] + 1.96* 0.02337)
LI<-exp(ajust$coefficients[2] - 1.96* 0.02337)
LI
LS



# Função utilizada para obter o Teste de Hosmer e Lemeshow
#Teste de Hosmer e Lemeshow

hosmerlem = function(y, yhat, g=10) {
  cutyhat = cut(yhat,
                breaks = quantile(yhat, probs=seq(0,
                                                  1, 1/g)), include.lowest=TRUE)
  obs = xtabs(cbind(1 - y, y) ~ cutyhat)
  expect = xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq = sum((obs - expect)^2/expect)
  P = 1 - pchisq(chisq, g - 2)
  return(list(chisq=chisq,p.value=P))
}



#Tópicos Especiais: Construção do Nomograma
# Apresentação dos Resultados
# Referência: Harrell (2015)
# Necessário: pacote Design/rms
# Exemplo de interpretação do Nomograma em
# http://www.prostate-cancer.org/education/riskases/Tisman_UsingNomograms1.html
require(rms)
ajust<-lrm(dc~factor(sexo)+factor(ecg)+idade)
ddist <- datadist(idade, sexo, ecg)
options(datadist='ddist')
nomogram(ajust,fun=plogis,funlabel="probabilidade",
         fun.at=c(.01,.05,.1,.25,.5,.75,.90,.95,.99), xfrac=.45)



# Tópicos Especiais: Q-QPLOT com envelope simulado (código: envel.bino)
# Utilizado para verificar a adequação do modelo

fit.model<-ajust1
par(mfrow=c(1,1))
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
w <- fit.model$weights
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
td <- resid(fit.model,type="deviance")/sqrt(1-h)
e <- matrix(0,n,100)

for(i in 1:100){
  dif <- runif(n) - fitted(fit.model)
  dif[dif >= 0 ] <- 0
  dif[dif<0] <- 1
  nresp <- dif
  fit <- glm(nresp ~ X, family=binomial)
  w <- fit$weights
  W <- diag(w)
  H <- solve(t(X)%*%W%*%X)
  H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
  h <- diag(H)
  e[,i] <- sort(resid(fit,type="deviance")/sqrt(1-h))}

e1 <- numeric(n)
e2 <- numeric(n)

for(i in 1:n){
  eo <- sort(e[i,])
  e1[i] <- eo[5]
  e2[i] <- eo[95]}

med <- apply(e,1,mean)
faixa <- range(td,e1,e2)
par(pty="s")
qqnorm(td,xlab="Percentis da N(0,1)",
       ylab="Componente da deviance", ylim=faixa, pch=16)
par(new=T)

qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1)
par(new=T)
qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1)
par(new=T)
qqnorm(med,axes=F,xlab="", ylab="", type="l",ylim=faixa,lty=2)



# Tópicos Especiais: Gráfico de Interação dos Slides

# Sem Interação
plot(c(0,12), c(0,20), type = "n", xlab="escolaridade", ylab="P(morte)")
abline(12,-1,col=2)
abline(20,-1,col=4)

# Com interação
plot(c(0,12), c(0,20), type = "n", xlab="escolaridade", ylab="P(morte)")
abline(16,-1.5,col=2)
abline(12,-0.8,col=4)




