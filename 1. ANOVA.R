#Analise de Variancia (ANOVA)



#Pacotes
if(!require(dplyr)) install.packages("dplyr")
library(dplyr)
if(!require(psych)) install.packages("psych") 
library(psych)
#Teste de Levene para verificar homogeidade dos desvios-padr�o
if(!require(lawstat)) install.packages("lawstat") 
library(lawstat)
if(!require(car)) install.packages("car") 
library(car)


#Banco de dados
dados <- read.table("http://www.est.ufmg.br/~enricoc/pdf/avancados_medicina/dados_aula2.txt", head=T) 


#Visualizacao
View(dados)
glimpse(dados) #Dplyr
dados$grupo <- factor(dados$grupo) #Transforma int em fac


#Analise descritiva
describe(dados) #Psych #Media global volume
describeBy(dados$volume, group = dados$grupo) #Psych #Estratificado
boxplot(volume ~ grupo, dados)
#Grupo 2 tem media/mediana superior
#Media ~= mediana, indicacao de simetria de distribuicoes
#Mesmo dp/range e uma indicacao de homocedasticidade


#Analise de variancia
dados_av <- aov (volume ~ grupo, dados) #ANOVA
dados_av
summary(dados_av) #Tabela de analise de variancia - ANOVA
dados_av$coef #Mostra os coeficientes estimados do modelo que s�o m�dias e desvios de m�dias
#O primeiro coeficiente � a m�dia do grupo 1
#O segundo coeficiente mais o primeiro � a m�dia do grupo 2
#O mesmo vale para o terceiro coeficiente para a m�dia do grupo 3
dados_av$res  #Mostra os res�duos do modelo -> os valores observados menos as m�dias
#Os res�duos s�o �teis para verificar a adequa��o da an�lise via ANOVA
#O valor-p=0,052 somente � v�lido se as suposi��es forem confirmadas


#Suposi��es do modelo
#A homogeneidade dos dp pode ser verificada pelo Teste de Bartlett ou de Levene (melhor)
bartlett.test(dados$volume, dados$grupo)
levene.test(dados$volume, dados$grupo)
leveneTest(volume ~ grupo, dados, center=median)
#Ambos testes mostram n�o haver evid�ncia contra a hip�tese de homocedasticidade
#Os res�duos tamb�m podem ser utilizados para verificar as suposi��es

#O teste de Shapiro-Wilks verifica a normalidade nos res�duos
shapiro.test(residuals(dados_av))

#Grafico dos residuos
plot(dados_av)
par(mfrow=c(2,2)) #colocando os quatro gr�ficos na tela
plot(dados_av)
par(mfrow=c(1,1))
#O primeiro (e o terceiro) mostrando faixas de mesma dispers�o atestam a adequa��o da homocedasticidade
#A linha vermelha nestes gr�ficos deve ser horizontal se a suposi��o de homocedasticidade for verdadeira
#E o segundo gr�fico, mostrando os pontos em torno da reta, atestam a adequa��o da normalidade
#O quarto gr�fico � usado para identificar poss�veis observa��es at�picas (outleirs)


#Como as suposi��es foram atendidads, n�s dizemos que existem diferen�a entre os grupos ao n�vel de 10%
#Portanto, necessitamos de compara��es m�ltiplas para encontrar quais grupos s�o diferentes e quais s�o iguais
#Vamos utilizar os m�todos de Bonferroni e Tukey
pairwise.t.test(dados$volume, dados$grupo, p.adj = "bonf")
pairwise.t.test(dados$volume, dados$grupo, p.adj = "holm") # M�todo de Holm
dados_tu <- TukeyHSD(dados_av,conf.level=0.90)
plot(dados_tu)
dados_tu
#Concluimos que existe diferen�a m�dia de 0,4 litros (IC 95%; 0,056 e 0,76) entre o grupo 2 (Rancho Los Amigos) 
#e 1 (John Hopkins). E n�o existe evid�ncia de diferen�a entre os grupos.