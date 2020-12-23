#An�lise de Vari�ncia (ANOVA) - PROJETO 1



#Testar a hip�tese de que a dosagem da hemoglobina glicosilada (HbA)
#� igual entre os tr�s grupos: Gestantes Normais (1),
#Gestantes com toler�ncia diminuida (2) e Gestantes diab�ticas (3).


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
dados <- read.table("http://www.est.ufmg.br/~enricoc/pdf/avancados_medicina/dadosexerc1.txt", 
                    head=T)


#Visualiza��o
View(dados)
glimpse(dados) #Dplyr
colnames(dados)
names(dados)[names(dados) == 'Grupo'] <- 'grupos'
dados$Grupo <- factor(dados$Grupo) #Transforma int em fac
glimpse(dados) #Dplyr


#An�lise descritiva
describe(dados) #Psych #M�dia global Hba
describeBy(dados$Hba, group = dados$Grupo) #Psych #Estratificado
dados$Grupo <- factor(dados$Grupo, levels=c("1","2","3"), labels=c("N", "TD", "D"))
boxplot(Hba ~ Grupo, dados) #Boxplot
#Grupo 3 tem m�dia/mediana superior


#An�lise de vari�ncia
dados_av <- aov (Hba ~ grupos, dados) #ANOVA
dados_av
summary(dados_av) #Tabela de an�lise de vari�ncia
#Mostra que as diferen�as entre as m�dias s�o significativas
dados_av$coef #Mostra os coeficientes estimados do modelo que s�o m�dias e desvios de m�dias
#O primeiro coeficiente � a m�dia do grupo 1
#O segundo coeficiente mais o primeiro � a m�dia do grupo 2
#O mesmo vale para o terceiro coeficiente para a m�dia do grupo 3
dados_av$res  #Mostra os res�duos do modelo -> os valores observados menos as m�dias
#Os res�duos s�o �teis para verificar a adequa��o da an�lise via ANOVA
#O valor-p somente � v�lido se as suposi��es forem confirmadas


#Suposi��es do modelo

#A homogeneidade dos dp pode ser verificada pelo Teste de Bartlett 
#ou de Levene (melhor)
bartlett.test(dados$Hba, dados$Grupo)
leveneTest(Hba ~ Grupo, dados, center=median)
#N�o h� uma diferen�a entre as vari�ncias
#Ambos testes mostram n�o haver evid�ncia contra a hip�tese de homocedasticidade
#Os res�duos tamb�m podem ser utilizados para verificar as suposi��es

#O teste de Shapiro-Wilks verifica a normalidade nos res�duos
shapiro.test(residuals(dados_av))

#Grafico dos residuos
plot(dados_av)
par(mfrow=c(2,2)) #colocando os quatro gr�ficos na tela
plot(dados_av)
par(mfrow=c(1,1))
#A linha vermelha nestes gr�ficos deve ser horizontal se a suposi��o de homocedasticidade for verdadeira;
#O segundo gr�fico, mostrando os pontos em torno da reta, atestam a adequa��o da normalidade;
#O quarto gr�fico � usado para identificar poss�veis observa��es at�picas (outleirs)

#Como as suposi��es foram atendidads, n�s dizemos que existem diferen�a entre os grupos ao n�vel de 0,5%.
#Portanto, necessitamos de compara��es m�ltiplas para encontrar quais grupos s�o diferentes e quais s�o iguais
#Vamos utilizar os m�todos de Bonferroni e Tukey
pairwise.t.test(dados$Hba, dados$Grupo, p.adj = "bonf") #Metodo de Bonf
pairwise.t.test(dados$Hba, dados$Grupo, p.adj = "holm") #Metodo de Holm
dados_tu <- TukeyHSD(dados_av,conf.level=0.95)
plot(dados_tu)
dados_tu