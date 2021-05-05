# Projeto Regressão Logística


# Reabsorção radicular externa após reimplantes de dentes permanentes

#O objetivo do estudo foi identificar a associação do desfecho, RRE, 
#com a idade no momento do trauma e com variáveis clínicas relacionadas ao 
#manejo e tratamento emergencial do dente avulsionado.



# Remove todos os objetos definidos
rm(list = ls()) 



# Pacotes
if (!require(pacman)) install.packages('pacman')
library(pacman)
pacman::p_load(dplyr, psych, car, ggplot2, janitor, mfx, caret, stargazer, faraway, ResourceSelection, hnp, rms)



# Banco de dados
dados <- read.table('http://www.est.ufmg.br/~enricoc/pdf/avancados_medicina/ProjetoEnrico.txt', 
                    head=T, dec=',')
View(dados)
glimpse(dados)



# Verificar dados ausentes
is.null(dados)



# Excluir variáveis que não serão utilizadas
dados <- subset(dados, select = -c(Indice1, Splintd, NumRegistro))
glimpse(dados)



# Alterar classes das variáveis

#dados em forma de texto (character) ou como fator/categóricas (factor)
#números inteiros (integer), números decimais (numeric ou double) ou números complexos (complex)

dados$Idade11 = as.factor(dados$Idade11)
dados$Idde16 = as.factor(dados$Idde16)
dados$Pereob15 = as.factor(dados$Pereob15)
dados$Meio3 = as.factor(dados$Meio3)
dados$TempoTERd = as.numeric(dados$TempoTERd)
dados$Ind1gbin = as.factor(dados$Ind1gbin)

glimpse(dados)



# Recodificar variável resposta em 0 e 1
dados$Ind1gbin_cat <- Recode(dados$Ind1gbin, '1 = 0; 2 = 1', as.factor=T)




# Análise Descritiva

data.frame(table(dados$Idade11)) %>%
  mutate(Rel_Freq = Freq/sum(Freq)) %>%
  rename(Idade11 = Var1)

data.frame(table(dados$Idde16)) %>%
  mutate(Rel_Freq = Freq/sum(Freq)) %>%
  rename(Idde16 = Var1)

data.frame(table(dados$Pereob15)) %>%
  mutate(Rel_Freq = Freq/sum(Freq)) %>%
  rename(Pereob15 = Var1)

data.frame(table(dados$Meio3)) %>%
  mutate(Rel_Freq = Freq/sum(Freq)) %>%
  rename(Meio3 = Var1)

describe(dados$TempoTERd)
summary(dados$TempoTERd)
ggplot(dados, aes(x = '', y = TempoTERd))+
  geom_boxplot(fill = 'gray', colour = 'black',
               outlier.colour = 'red', outlier.size = 2)+
  labs(x = '', y = 'Tempo TER',
       subtitle = 'Medido em dias',
       title = 'Tempo decorrido entre o reimplante dentário
       e a realização do TER (Tratamento Endodontico Radical)')+
  theme_classic()


data.frame(table(dados$Ind1gbin_cat)) %>%
  mutate(Rel_Freq = Freq/sum(Freq)) %>%
  rename(Ind1gbin_cat = Var1)



# Colinearidade?



# Análise Univariada (Frequência e Teste qui-quadrado)

dados %>%
  tabyl(Idade11, Ind1gbin_cat) %>%
  adorn_totals(c('row', 'col')) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns %>%
  adorn_title('combined') %>%
  knitr::kable()
chisq.test(dados$Idade11, dados$Ind1gbin_cat, correct = F)

dados %>%
  tabyl(Idde16, Ind1gbin_cat) %>%
  adorn_totals(c('row', 'col')) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns %>%
  adorn_title('combined') %>%
  knitr::kable()
chisq.test(dados$Idde16, dados$Ind1gbin_cat, correct = F)
fisher.test(dados$Idde16, dados$Ind1gbin_cat)

dados %>%
  tabyl(Pereob15, Ind1gbin_cat) %>%
  adorn_totals(c('row', 'col')) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns %>%
  adorn_title('combined') %>%
  knitr::kable()
chisq.test(dados$Pereob15, dados$Ind1gbin_cat, correct = F)
fisher.test(dados$Pereob15, dados$Ind1gbin_cat)

dados %>%
  tabyl(Meio3, Ind1gbin_cat) %>%
  adorn_totals(c('row', 'col')) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns %>%
  adorn_title('combined') %>%
  knitr::kable()
chisq.test(dados$Meio3, dados$Ind1gbin_cat, correct = F)

describeBy(dados$TempoTERd, group = dados$Ind1gbin_cat, digits = 3)
ggplot(dados, aes(x=TempoTERd, y=Ind1gbin_cat)) + 
  geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)
fisher.test(dados$TempoTERd, dados$Ind1gbin_cat)



# Análise Univariada (Regressão Logística, Razão de Chances e Intervalo de Confiança)

ajusteIdade11 <- glm(Ind1gbin_cat ~ Idade11, family = binomial(link = "logit"), data = dados)
summary(ajusteIdade11)
anova(ajusteIdade11, test = "Chisq")
#Os individuos de > 11 anos têm em média 0,9% a menos 
#de reabsorção observado na consulta de inicio do Tratamento Endodontico Radical (TER)
#em comparação com os individuos de idade <= 11 anos.
#Foi observado significância estatística a p=0,009 na utilização da variável para o modelo.

logitor(Ind1gbin_cat ~ Idade11, data = dados)
exp(cbind(OR=coef(ajusteIdade11), confint(ajusteIdade11)))
#A chance de reabsorção observado na consulta de inicio do Tratamento Endodontico Radical (TER)
#em individuos de > 11 anos é cerca de 0,38 vezes a chance daqueles do grupo <= 11 anos.


ajusteIdde16 <- glm(Ind1gbin_cat ~ Idde16, family = binomial(link = "logit"), data = dados)
summary(ajusteIdde16)
anova(ajusteIdde16, test = "Chisq")
#Os individuos de > 16 anos têm em média 2,4% a menos 
#de reabsorção observado na consulta de inicio do Tratamento Endodontico Radical (TER)
#em comparação com os individuos de idade <= 16 anos.
#Foi observado significância estatística a p=0,023 na utilização da variável para o modelo.

logitor(Ind1gbin_cat ~ Idde16, data = dados)
exp(cbind(OR=coef(ajusteIdde16), confint(ajusteIdde16)))
#A chance de reabsorção observado na consulta de inicio do Tratamento Endodontico Radical (TER)
#em individuos de > 16 anos é cerca de 0,94 vezes a chance daqueles do grupo <= 16 anos.


ajustePereob15 <- glm(Ind1gbin_cat ~ Pereob15, family = binomial(link = "logit"), data = dados)
summary(ajustePereob15)
anova(ajustePereob15, test = "Chisq")
#O tempo de > 15 min têm em média 0,6% a mais 
#de reabsorção observado na consulta de inicio do Tratamento Endodontico Radical (TER)
#em comparação com os <= 15 anos.
#Não foi observado significância estatística a p=0,423 na utilização da variável para o modelo.

logitor(Ind1gbin_cat ~ Pereob15, data = dados)
exp(cbind(OR=coef(ajustePereob15), confint(ajustePereob15)))
#A chance de reabsorção observado na consulta de inicio do Tratamento Endodontico Radical (TER)
#em tempo > 15 min é cerca de 1,89 vezes a chance daqueles do grupo <= 15 min.


ajusteMeio3 <- glm(Ind1gbin_cat ~ Meio3, family = binomial(link = "logit"), data = dados)
summary(ajusteMeio3)
anova(ajusteMeio3, test = "Chisq")
#O Leite têm em média 0,08% a menos 
#de reabsorção observado na consulta de inicio do Tratamento Endodontico Radical (TER)
#em comparação com os Meios úmidos.
#O Seco  têm em média 0,14% a mais 
#de reabsorção observado na consulta de inicio do Tratamento Endodontico Radical (TER)
#em comparação com os Meios úmidos.
#Não foi observado significância estatística a p=0,867 e p=0,722 na utilização das variáveis para o modelo.

logitor(Ind1gbin_cat ~ Meio3, data = dados)
exp(cbind(OR=coef(ajusteMeio3), confint(ajusteMeio3)))
#A chance de reabsorção observado na consulta de inicio do Tratamento Endodontico Radical (TER)
#em Leite é cerca de 0,92 vezes a chance daqueles do Meios úmidos.
#A chance de reabsorção observado na consulta de inicio do Tratamento Endodontico Radical (TER)
#em Seco é cerca de 1,15 vezes a chance daqueles do Meios úmidos.


ajusteTempoTERd <- glm(Ind1gbin_cat ~ TempoTERd, family = binomial(link = "logit"), data = dados)
summary(ajusteTempoTERd)
anova(ajusteTempoTERd, test = "Chisq")
#O aumento 1 (dia) tempo decorrido entre o reimplante dentário e a realização do TER
#aumenta em média 0,004% doindice de reabsorção observado na consulta de inicio do Tratamento Endodontico Radical (TER)
#Foi observado significância estatística a p=0,000 na utilização da variável para o modelo.

logitor(Ind1gbin_cat ~ TempoTERd, data = dados)
exp(cbind(OR=coef(ajusteTempoTERd), confint(ajusteTempoTERd)))
#Para cada variação unitária no TempoTERd, as chances de ocorrência de Ind1gbin_cat aumentam 1,00 vezes


# Matriz de Confusão
dados$pdata <- as.factor(
  ifelse(
    predict(ajusteTempoTERd, 
            newdata = dados, 
            type = "response")
    >0.5,"1","0"))

confusionMatrix(dados$pdata, dados$Ind1gbin_cat, positive="1")



#Regressão Logística Múltipla

ajuste1 <- glm(Ind1gbin_cat ~ Idade11 + TempoTERd, family = binomial(link = "logit"), data = dados)
summary(ajuste1)
anova(ajuste1, test="Chisq")

#TempoTERd tem distribuição linear?
#Não tem interação?


stargazer(ajuste1, title="Resultados", type = "text")
#?


# OR e IC95%
logitor(Ind1gbin_cat ~ Idade11 + TempoTERd, data = dados)
exp(coef(ajuste1))
exp(cbind(OR=coef(ajuste1), confint(ajuste1)))


#VIF
vif(ajuste1)



# Teste Hosmer e Lemeshow
hl=hoslem.test(dados$Ind1gbin_cat,fitted(ajuste1),g=10)
hl



# Q-QPLOT com envelope simulado

fit.model <- ajuste1

par(mfrow = c(1, 1))
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
w <- fit.model$weights
W <- diag(w)
H <- solve(t(X) %*% W %*% X)
H <- sqrt(W) %*% X %*% H %*% t(X) %*% sqrt(W)
h <- diag(H)
td <- resid(fit.model, type = "deviance") / sqrt(1 - h)
e <- matrix(0, n, 100)

for(i in 1:100){
  dif <- runif(n) - fitted(fit.model)
  dif[dif >= 0 ] <- 0
  dif[dif < 0] <- 1
  nresp <- dif
  fit <- glm(nresp ~ X, family = binomial)
  w <- fit$weights
  W <- diag(w)
  H <- solve(t(X) %*% W %*% X)
  H <- sqrt(W) %*% X %*% H %*% t(X) %*% sqrt(W)
  h <- diag(H)
  e[,i] <- sort(resid(fit, type = "deviance") / sqrt(1 - h))
}

e1 <- numeric(n)
e2 <- numeric(n)

for(i in 1:n){
  eo <- sort(e[i,])
  e1[i] <- eo[5]
  e2[i] <- eo[95]
}

med <- apply(e, 1, mean)
faixa <- range(td, e1, e2)
par(pty = "s")
qqnorm(td, xlab = "Percentis da N(0,1)", ylab = "Componente da deviance", ylim = faixa, pch = 16)
par(new = T)

qqnorm(e1,  axes = F, xlab = "", ylab = "", type = "l", ylim = faixa, lty = 1)
par(new = T)
qqnorm(e2,  axes = F, xlab = "", ylab = "", type = "l", ylim = faixa, lty = 1)
par(new = T)
qqnorm(med, axes = F, xlab = "", ylab = "", type = "l", ylim = faixa, lty = 2)



# Nomograma
ddist <- datadist(dados) 
options(datadist='ddist')
ajuste1r<-lrm(Ind1gbin_cat ~ Idade11 + TempoTERd,  data = dados) 
nom<-nomogram(ajuste1r,fun=plogis,funlabel="probabilidade", 
              fun.at=c(.01,.05,.1,.25,.5,.75,.90,.95,.99))
plot(nom,xfrac=0.45)
