# Regressão Linear Múltipla

# Exemplo: (Clínica Oftalmológica) Acredita-se que a pressão intra-ocular é
# explicada (depende) pela idade e também pelo gênero.

# Variável Resposta: Pressão Intra-Ocular (PIO)
# Covariáveis: Idade e Gênero (0 - mulher e 1 - homem)



# Pacotes
if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(dplyr, psych, lmtest)


# Banco de Dados
dados <- read.table("http://www.est.ufmg.br/~enricoc/pdf/avancados_medicina/dados_aula4.txt", 
                    head = T)


# Leitura do Banco de Dados
View(dados)
glimpse(dados) #Classe das variáveis
describe(dados)
summary(dados)


# Converter a classe das variáveis
dados$pio <- as.numeric(dados$pio)
dados$idade <- as.numeric(dados$idade)
dados$sexo <- as.factor(dados$sexo)
glimpse(dados)


# Regressão Linear Simples - Idade
mod1 <- lm(pio ~ idade, dados)
summary(mod1) #teste t para beta
#a: Intercepto ou coeficiente linear é representa o ponto em que a reta regressora
#corta o eixo do y, quando x=0 (B0 é a interceptação y e B1 é a inclinação).
#B1: O aumento em ano na idade, aumenta a PIO média em 0,23 mmHg.
#R2: 71% dos dados são explicados pelo modelo.
#Teste F: Modelo proporciona um ajuste melhor do que o modelo somente com o intercepto.


# Regressão Linear Simples - Gênero
mod2 <- lm(pio ~ sexo, dados)
summary (mod2)
#B1: Homens têm em média 4% a mais de PIO do que os mulheres?.
#R2: 56% dos dados são explicados pelo modelo.
#Teste F: Modelo proporciona um ajuste melhor do que o modelo somente com o intercepto.


# Regressão Linear Múltipla - Idade e Gênero
mod3 <- lm(pio ~ idade+sexo, dados)
summary(mod3)
#B1: O aumento em ano na idade, aumenta a PIO média em 0,18 mmHg.
#B2: Não foi significativo.
#R2 ajustado: 71% dos dados são explicados pelo modelo.
#Teste F:


# Verificar Suposições do Modelo
par(mfrow=c(2, 2))
plot(mod3, which=c(1:4), pch=16, add.smooth=T)

# 1. Residuals vs Fittes (Resíduos vs Preditos): Homocedasticidade e linearidade.
#Os pontos devem ficar aleatoriamente distribuídos em torno de uma linha
#horizontal paralela ao eixo x na altura do resíduo zero.
#A violação do pressuposto de homocedasticidade produz faixas em forma de cone,
#enquanto violações do pressuposto de linearidade produz faixas não-lineares.

# 2.Normal Q-Q: Normalidade (Probabilidade dos resíduos).
#Os pontos deve ficar distribuídos ao longo da reta y = x.

# 3. Scale-Location: Homocedasticidade.
#Os pontos devem ficar aleatoriamente distribuídos em torno de uma linha
#horizontal paralela ao eixo x mas não do valor zero.

# 4. Cook's distance (Distância de Cook): Detecção de pontos atípicos.
#Observações atípicas.

par(mfrow=c(1,1))
boxplot(mod3$resid)


# Teste de Normalidade
shapiro.test(mod3$resid)
#P = 0.05

# Teste de Homocedasticidade
summary(lm(abs(resid(mod3)) ~ fitted(mod3)))
#P = 0.06

# Teste de Independência
#Assume-se a independância dos resíduos de acordo com estrutura de coleta dos dados.
#Em dados longitudinais podem ocorrer violação dessa suposição.
durbinWatsonTest(mod3)
plot(mod3$resid,pch = 16, ylab = 'Residuals')


levels(dados$sexo) <- c('mulher', 'homem')
boxplot(idade ~ sexo, dados, ylab = 'Idade', xlab = 'Gênero')
#Homens são mais velhor que as mulheres.
#Idade é um fator de confusão para a associação entre Pio e Gênero.





# Tópicos Especiais - Excluindo o paciente 25

if(!require(gamlss)) install.packages("gamlss")
library(gamlss)
dados2 <- dados[-25,]
mod4 <- gamlss(dados2$pio ~ dados2$idade + dados2$sexo, 
               sigma.formula = ~ dados2$idade, family = NO)
plot(mod4)
wp(mod4)
summary(mod4)

mod5 <- lm(pio ~ idade + sexo, dados2)
summary(mod5)
par(mfrow=c(2, 2))
plot(mod5, which=c(1:4), pch=16, add.smooth=T)
shapiro.test(mod5$resid)


