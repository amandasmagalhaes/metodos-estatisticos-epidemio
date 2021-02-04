# PROJETO 2: REGRESSÃO LINEAR MÚLTIPLA



# Pacotes
if (!require(pacman)) install.packages('pacman')
library(pacman)
pacman::p_load(dplyr, psych, ggplot2, ggpubr, lmtest, car)



# Banco de dados
dados <- read.table('http://www.est.ufmg.br/~enricoc/pdf/avancados_medicina/Banco_dados_projeto2.txt',
                    head = T)
View(dados)
glimpse(dados)



# Excluir variáveis que não devem ser utilizadas
dados <- subset(dados, select = -c(Tanita, Peso, Altura))
glimpse(dados)



# Alterar classe das variáveis
dados$Sexo = as.factor(dados$Sexo)
dados$Idade = as.numeric(dados$Idade)
dados$Classificação = as.factor(dados$Classificação)
glimpse(dados)



# Verificar dados ausentes
is.null(dados)



# Análise Descritiva
describe(dados, omit=T) #Omite variáveis não numéricas
summary(dados, digits = 2)

data.frame(table(dados$Sexo)) %>%
  mutate(Rel_Freq = Freq/sum(Freq)) %>%
  rename(Sexo = Var1)

data.frame(table(dados$Classificação)) %>%
  mutate(Rel_Freq = Freq/sum(Freq)) %>%
  rename(Classificação = Var1)

describeBy(dados, group = dados$Sexo, digits = 3)

par(mfrow=c(1,1))

#Idade
ggplot(dados, aes(x = '', y = Idade))+
  geom_boxplot(fill = 'gray', colour = 'black',
               outlier.colour = 'tomato', outlier.size = 2)+
  labs(x = '', y = 'Idade', title = 'Idade (anos)')+
  theme_classic()

#IMC
ggplot(dados, aes(x = '', y = IMC))+
  geom_boxplot(fill = 'gray', colour = 'black',
               outlier.colour = 'red', outlier.size = 2)+
  labs(x = '', y = 'IMC', title = 'Índice de massa corporal')+
  theme_classic()

#Pregas
ggplot(dados, aes(x = '', y = Pregas))+
  geom_boxplot(fill = 'gray', colour = 'black',
               outlier.colour = 'red', outlier.size = 2)+
  labs(x = '', y = 'Pregas', title = 'Percentual de gordura corporal por pregas')+
  theme_classic()

#Conicidade
ggplot(dados, aes(x = '', y = Conicidade))+
  geom_boxplot(fill = 'gray', colour = 'black',
               outlier.colour = 'red', outlier.size = 2)+
  labs(x = '', y = 'Conicidade', title = 'Índice de Conicidade')+
  theme_classic()

#CinturaQuadril
ggplot(dados, aes(x = '', y = CinturaQuadril))+
  geom_boxplot(fill = 'gray', colour = 'black',
               outlier.colour = 'red', outlier.size = 2)+
  labs(x = '', y = 'Cintura Quadril', title = 'Índice Cintura/Quadril')+
  theme_classic()

#CinturaEstatura
ggplot(dados, aes(x = '', y = CinturaEstatura))+
  geom_boxplot(fill = 'gray', colour = 'black',
               outlier.colour = 'red', outlier.size = 2)+
  labs(x = '', y = 'Cintura Estatura', title = 'Relação Cintura/Estatura')+
  theme_classic()

#Percentil
ggplot(dados, aes(x = '', y = Percentil))+
  geom_boxplot(fill = 'gray', colour = 'black',
               outlier.colour = 'red', outlier.size = 2)+
  labs(x = '', y = 'Percentil', title = 'Percentil do IMC')+
  theme_classic()

#Escorez
ggplot(dados, aes(x = '', y = Escorez))+
  geom_boxplot(fill = 'gray', colour = 'black',
               outlier.colour = 'red', outlier.size = 2)+
  labs(x = '', y = 'Escore z', title = 'Escore Z do IMC')+
  theme_classic()

#Sexo
ggplot(dados, aes(x = Sexo))+
  geom_bar()+
  labs(x = '', y = 'Frequência', title = 'Gênero')+
  scale_x_discrete(labels=c('0' = 'Masculino', '1' = 'Feminino'))+
  theme_classic()

#Classificação
ggplot(dados, aes(x = Classificação))+
  geom_bar()+
  labs(x = '', y = 'Frequência', title = 'Classificação')+
  scale_x_discrete(labels=c('0' = 'Eutrófico', '1' = 'Sobrepeso', '2' = 'Obeso'))+
  theme_classic()



# Análise Univariada (bivariada): Gráfico LOWESS* + Correlação, Regressão Linear Simples
#*'Suavização chamada 'Lowess' para verificar a linearidade'

#Idade
ggscatter(dados, x = 'Idade', y = 'BIA', 
          add = 'loess', conf.int = TRUE,
          add.params = list(color = 'red', fill = 'lightgray'),
          xlab = 'Idade', ylab = 'BIA', title = 'BIA x Idade (anos)')+
  stat_cor(method ='pearson', p.accuracy = 0.001, r.accuracy = 0.01,
           label.x = 9.5, label.y = )+
  theme_classic2()

modIdade <- lm(BIA ~ Idade, dados)
summary(modIdade)
confint(modIdade)


#IMC
ggscatter(dados, x = 'IMC', y = 'BIA', 
          add = 'loess', conf.int = TRUE,
          add.params = list(color = 'red', fill = 'lightgray'),
          xlab = 'IMC', ylab = 'BIA', title = 'BIA x Índice de massa corporal')+
  stat_cor(method ='pearson', p.accuracy = 0.001, r.accuracy = 0.01,
           label.x = , label.y = )+
  theme_classic2()

modIMC <- lm(BIA ~ IMC, dados)
summary(modIMC)
confint(modIMC)


#Pregas
ggscatter(dados, x = 'Pregas', y = 'BIA', 
          add = 'loess', conf.int = TRUE,
          add.params = list(color = 'red', fill = 'lightgray'),
          xlab = 'Pregas', ylab = 'BIA', title = 'BIA x Percentual de gordura corporal por pregas')+
  stat_cor(method ='pearson', p.accuracy = 0.001, r.accuracy = 0.01,
           label.x = , label.y = )+
  theme_classic2()

modPregas <- lm(BIA ~ Pregas, dados)
summary(modPregas)
confint(modPregas)


#Conicidade
ggscatter(dados, x = 'Conicidade', y = 'BIA', 
          add = 'loess', conf.int = TRUE,
          add.params = list(color = 'red', fill = 'lightgray'),
          xlab = 'Conicidade', ylab = 'BIA', title = 'BIA x Índice de Conicidade')+
  stat_cor(method ='pearson', p.accuracy = 0.001, r.accuracy = 0.01,
           label.x = , label.y = )+
  theme_classic2()

modConicidade <- lm(BIA ~ Conicidade, dados)
summary(modConicidade)
confint(modConicidade)


#CinturaQuadril
ggscatter(dados, x = 'CinturaQuadril', y = 'BIA', 
          add = 'loess', conf.int = TRUE,
          add.params = list(color = 'red', fill = 'lightgray'),
          xlab = 'CinturaQuadril', ylab = 'BIA', title = 'BIA x Índice Cintura/Quadril')+
  stat_cor(method ='pearson', p.accuracy = 0.001, r.accuracy = 0.01,
           label.x = , label.y = )+
  theme_classic2()

modCinturaQuadril <- lm(BIA ~ CinturaQuadril, dados)
summary(modCinturaQuadril)
confint(modCinturaQuadril)


#CinturaEstatura
ggscatter(dados, x = 'CinturaEstatura', y = 'BIA', 
          add = 'loess', conf.int = TRUE,
          add.params = list(color = 'red', fill = 'lightgray'),
          xlab = 'CinturaEstatura', ylab = 'BIA', title = 'BIA x Relação Cintura/Estatura')+
  stat_cor(method ='pearson', p.accuracy = 0.001, r.accuracy = 0.01,
           label.x = , label.y = )+
  theme_classic2()

modCinturaEstatura <- lm(BIA ~ CinturaEstatura, dados)
summary(modCinturaEstatura)
confint(modCinturaEstatura)


#Percentil
ggscatter(dados, x = 'Percentil', y = 'BIA', 
          add = 'loess', conf.int = TRUE,
          add.params = list(color = 'red', fill = 'lightgray'),
          xlab = 'Percentil', ylab = 'BIA', title = 'BIA x Percentil do IMC')+
  stat_cor(method ='pearson', p.accuracy = 0.001, r.accuracy = 0.01,
           label.x = , label.y = )+
  theme_classic2()

modPercentil <- lm(BIA ~ Percentil, dados)
summary(modPercentil)
confint(modPercentil)


#Escorez
ggscatter(dados, x = 'Escorez', y = 'BIA', 
          add = 'loess', conf.int = TRUE,
          add.params = list(color = 'red', fill = 'lightgray'),
          xlab = 'Escorez', ylab = 'BIA', title = 'BIA x Escore Z do IMC')+
  stat_cor(method ='pearson', p.accuracy = 0.001, r.accuracy = 0.01,
           label.x = , label.y = )+
  theme_classic2()

modEscorez <- lm(BIA ~ Escorez, dados)
summary(modEscorez)
confint(modEscorez)


#Sexo
ggplot(dados, aes(x = Sexo, y = BIA))+
  geom_boxplot(fill = 'gray', colour = 'black',
               outlier.colour = 'red', outlier.size = 2)+
  labs(x = 'Gênero', y = 'BIA', title = 'BIA x Gênero')+
  scale_x_discrete(labels=c('0' = 'Masculino', '1' = 'Feminino'))+
  theme_classic()

modSexo <- lm(BIA ~ Sexo, dados)
summary(modSexo)
confint(modSexo)


#Classificação
ggplot(dados, aes(x = Classificação, y = BIA))+
  geom_boxplot(fill = 'gray', colour = 'black',
               outlier.colour = 'red', outlier.size = 2)+
  labs(x = 'Classificação', y = 'BIA', title = 'BIA x Classificação')+
  scale_x_discrete(labels=c('0' = 'Eutrófico', '1' = 'Sobrepeso', '2' = 'Obeso'))+
  theme_classic()

modClassificação <- lm(BIA ~ Classificação, dados)
summary(modClassificação)
confint(modClassificação)



# Regressão Multivariada
mod1 <- lm(BIA ~ Idade+I(Idade^2) + Pregas+I(Pregas^2) + Conicidade + 
            CinturaEstatura + Percentil + Sexo, dados)
summary(mod1)


mod2 <- lm(BIA ~ Idade+I(Idade^2) + Conicidade +  
             CinturaEstatura + Sexo, dados)
summary(mod2)


mod3 <- lm(BIA ~ Idade+I(Idade^2) + Conicidade +  
             CinturaEstatura + Sexo, dados)
summary(mod3)


mod4 <- lm(BIA ~ Idade+I(Idade^2) + CinturaEstatura + Sexo, dados)
summary(mod4)



# Multicolinearidade
pairs.panels(dados)
cor.test(dados$Conicidade, dados$CinturaEstatura)



# Termo de Interação
mod5 <- lm(BIA ~ Idade+I(Idade^2) + CinturaEstatura + Sexo + 
             Idade:CinturaEstatura + +I(Idade^2):CinturaEstatura, dados)
summary(mod5)


mod5 <- lm(BIA ~ Idade+I(Idade^2) + CinturaEstatura + Sexo + 
             Idade:Sexo + +I(Idade^2):Sexo, dados)
summary(mod5)


mod5 <- lm(BIA ~ Idade+I(Idade^2) + CinturaEstatura + Sexo + 
             CinturaEstatura:Sexo, dados)
summary(mod5)



# Modelo Final
mod4 <- lm(BIA ~ Idade+I(Idade^2) + CinturaEstatura + Sexo, dados)
summary(mod4)
confint(mod4)



# Verificar Suposições do Modelo
par(mfrow=c(2, 2))
plot(mod4, which=c(1:4), pch=16, add.smooth=T)

par(mfrow=c(1,1))
boxplot(mod4$resid)


#Teste de Normalidade
shapiro.test(mod4$resid)


#Teste de Homocedasticidade
summary(lm(abs(resid(mod4)) ~ fitted(mod4)))


#Teste de Independência
#Assume-se a independância dos resíduos de acordo com estrutura de coleta dos dados.
#Em dados longitudinais podem ocorrer violação dessa suposição.
durbinWatsonTest(mod4)
plot(mod4$resid,pch = 16, ylab = 'Residuals')

