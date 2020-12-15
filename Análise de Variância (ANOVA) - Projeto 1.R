#Análise de Variância (ANOVA) - PROJETO 1



#Testar a hipótese de que a dosagem da hemoglobina glicosilada (HbA)
#é igual entre os três grupos: Gestantes Normais (1),
#Gestantes com tolerância diminuida (2) e Gestantes diabéticas (3).


#Pacotes
if(!require(dplyr)) install.packages("dplyr")
library(dplyr)
if(!require(psych)) install.packages("psych")
library(psych)
#Teste de Levene para verificar homogeidade dos desvios-padrão
if(!require(lawstat)) install.packages("lawstat")
library(lawstat)
if(!require(car)) install.packages("car")
library(car)


#Banco de dados
dados <- read.table("http://www.est.ufmg.br/~enricoc/pdf/avancados_medicina/dadosexerc1.txt", 
                    head=T)


#Visualização
View(dados)
glimpse(dados) #Dplyr
dados$Grupo <- factor(dados$Grupo) #Transforma int em fac
glimpse(dados) #Dplyr


#Análise descritiva
describe(dados) #Psych #Média global Hba
describeBy(dados$Hba, group = dados$Grupo) #Psych #Estratificado
boxplot(Hba ~ Grupo, dados) #Boxplot
#Grupo 3 tem média/mediana superior


#Análise de variância
dados_av <- aov (Hba ~ Grupo, dados) #ANOVA
dados_av
summary(dados_av) #Tabela de análise de variância
                  #Mostra que as diferenças entre as médias são significativas
dados_av$coef #Mostra os coeficientes estimados do modelo que são médias e desvios de médias
              #O primeiro coeficiente é a média do grupo 1
              #O segundo coeficiente mais o primeiro é a média do grupo 2
              #O mesmo vale para o terceiro coeficiente para a média do grupo 3
dados_av$res  #Mostra os resíduos do modelo -> os valores observados menos as médias
              #Os resíduos são úteis para verificar a adequação da análise via ANOVA
              #O valor-p somente é válido se as suposições forem confirmadas


#Suposições do modelo

#A homogeneidade dos dp pode ser verificada pelo Teste de Bartlett 
#ou de Levene (melhor)
bartlett.test(dados$Hba, dados$Grupo)
leveneTest(Hba ~ Grupo, dados, center=median)
#Não há uma diferença entre as variâncias
#Ambos testes mostram não haver evidência contra a hipótese de homocedasticidade
#Os resíduos também podem ser utilizados para verificar as suposições

#O teste de Shapiro-Wilks verifica a normalidade nos resíduos
shapiro.test(residuals(dados_av))

#Grafico dos residuos
plot(dados_av)
par(mfrow=c(2,2)) #colocando os quatro gráficos na tela
plot(dados_av)
par(mfrow=c(1,1))
#A linha vermelha nestes gráficos deve ser horizontal se a suposição de homocedasticidade for verdadeira;
#O segundo gráfico, mostrando os pontos em torno da reta, atestam a adequação da normalidade;
#O quarto gráfico é usado para identificar possíveis observações atípicas (outleirs)

#Como as suposições foram atendidads, nós dizemos que existem diferença entre os grupos ao nível de 0,5%.
#Portanto, necessitamos de comparações múltiplas para encontrar quais grupos são diferentes e quais são iguais
#Vamos utilizar os métodos de Bonferroni e Tukey
pairwise.t.test(dados$Hba, dados$Grupo, p.adj = "bonf") #Metodo de Bonf
pairwise.t.test(dados$Hba, dados$Grupo, p.adj = "holm") #Metodo de Holm
dados_tu <- TukeyHSD(dados_av,conf.level=0.95)
plot(dados_tu)
dados_tu
