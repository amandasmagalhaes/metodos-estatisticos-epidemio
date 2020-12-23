#Analise de Variancia (ANOVA)



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
dados_av$coef #Mostra os coeficientes estimados do modelo que são médias e desvios de médias
#O primeiro coeficiente é a média do grupo 1
#O segundo coeficiente mais o primeiro é a média do grupo 2
#O mesmo vale para o terceiro coeficiente para a média do grupo 3
dados_av$res  #Mostra os resíduos do modelo -> os valores observados menos as médias
#Os resíduos são úteis para verificar a adequação da análise via ANOVA
#O valor-p=0,052 somente é válido se as suposições forem confirmadas


#Suposições do modelo
#A homogeneidade dos dp pode ser verificada pelo Teste de Bartlett ou de Levene (melhor)
bartlett.test(dados$volume, dados$grupo)
levene.test(dados$volume, dados$grupo)
leveneTest(volume ~ grupo, dados, center=median)
#Ambos testes mostram não haver evidência contra a hipótese de homocedasticidade
#Os resíduos também podem ser utilizados para verificar as suposições

#O teste de Shapiro-Wilks verifica a normalidade nos resíduos
shapiro.test(residuals(dados_av))

#Grafico dos residuos
plot(dados_av)
par(mfrow=c(2,2)) #colocando os quatro gráficos na tela
plot(dados_av)
par(mfrow=c(1,1))
#O primeiro (e o terceiro) mostrando faixas de mesma dispersão atestam a adequação da homocedasticidade
#A linha vermelha nestes gráficos deve ser horizontal se a suposição de homocedasticidade for verdadeira
#E o segundo gráfico, mostrando os pontos em torno da reta, atestam a adequação da normalidade
#O quarto gráfico é usado para identificar possíveis observações atípicas (outleirs)


#Como as suposições foram atendidads, nós dizemos que existem diferença entre os grupos ao nível de 10%
#Portanto, necessitamos de comparações múltiplas para encontrar quais grupos são diferentes e quais são iguais
#Vamos utilizar os métodos de Bonferroni e Tukey
pairwise.t.test(dados$volume, dados$grupo, p.adj = "bonf")
pairwise.t.test(dados$volume, dados$grupo, p.adj = "holm") # Método de Holm
dados_tu <- TukeyHSD(dados_av,conf.level=0.90)
plot(dados_tu)
dados_tu
#Concluimos que existe diferença média de 0,4 litros (IC 95%; 0,056 e 0,76) entre o grupo 2 (Rancho Los Amigos) 
#e 1 (John Hopkins). E não existe evidência de diferença entre os grupos.