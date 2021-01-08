# Regressão Linear Simples
# Exemplo de Sala de aula: Pressão Intra-Ocular



# Pacotes
if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(dplyr, psych, car, ggplot2, rstatix, lmtest, ggpubr, gamlss)


# Banco de dados
dados <- read.table("http://www.est.ufmg.br/~enricoc/pdf/avancados_medicina/dados_aula3.txt", 
                    head=T)


# Leitura do banco de dados
View(dados)
dim(dados)
names(dados)
glimpse(dados) #ambas variáveis são numéricas
summary(dados)
describe(dados)


# Gráfico de Disperção
plot(dados$idade, dados$pio, xlab = "Idade", ylab = "PIO")
#O gráfico de dispersÃ£o indica uma possível correlação positiva entre as variéveis
#Ou seja, os mais velhos estão associados com valores de PIO mais altos


# Correlação entre PIO e Idade
cor.test(dados$idade, dados$pio) #teste e IC para \rho
#Mede a força de uma associação (linear)
#Ao nível de 5% há fortes evidência de que p é diferente de zero
#Coeficiente de correlação de Pearson foi de 0,84 (IC; 95% 0,68; 0,93)
#0,84 é verdadeiro ou é so um variação amostral?
#Testar hipotese que não existe associação


# Modelo de Regressão Linear Simples
mod <- lm(pio ~ idade, dados) #ajuste do modelo de regressão linear simples
summary(mod) #teste t para beta
#Qualidade do ajuste por meio do quadrado do coeficiente de correlaçao de Pearson
#R2 = 0,71 = 71% da variação dos dados está sendo explicada pela reta de regressão
#O aumento em ano na idade, aumenta a PIO média em 0,23 (IC; 95% 0,17; 0,29) mmHg. 
#Ou o aumento em quatro anos na idade, aumenta a PIO média em cerca de 1 mmHg (IC; 95% 0,68; 1,16) mmHg.


# Gráfico da reta ajustada
par(mfrow=c(1,1))
plot(dados$idade, dados$pio,abline(mod$coefficients[1], mod$coefficients[2]),
     xlab="Idade", ylab="PIO")
lines(lowess(dados$idade, dados$pio),col=2)
legend(60,17,"PIO = 6,64 + 0,23 idade",bty="n")
legend(60.65,17.15,"^",bty="n")


# Gráficos para verificar a adequação do ajuste
par(mfrow=c(2,2)) 
plot(mod, which=c(1:4), add.smooth=T,pch=20)
# 1. Residuals vs Fittes (Gráfico de resíduos vs preditos): Homocedasticidade e linearidade.
#Os pontos devem ficar aleatoriamente distribuídos em torno de uma linha horizontal paralela ao eixo x na altura do resíduo zero.
#A violação do pressuposto de homocedasticidade produz faixas em forma de cone, enquanto violações do pressuposto de linearidade produz faixas não-lineares.
# 2. Scale-Location: Homocedasticidade (somente).
#Os pontos devem ficar aleatoriamente distribuídos em torno de uma linha horizontal paralela ao eixo x mas não do valor zero.
# 3.Normal Q-Q: Normalidade: gráfico de probabilidade dos resíduos.
#Os pontos deve ficar distribuídos ao longo da reta y = x.
# 4. Cook's distance: Detecção de pontos atípicos: distância de Cook.
#Valores grandes são identificados como pontos atípicos.
par(mfrow=c(1,1)) 
boxplot(mod$resid)


# Teste de Normalidade
shapiro.test(mod$residuals)


# Teste de Homocedasticidade
#Um teste simples para verificar variância não-constante.
#Este teste não é completamente correto pois algum peso deve ser utilizado 
#e os graus de liberdade ajustados. Veja Faraway (2004)
summary(lm(abs(residuals(mod))~fitted(mod)))


#Conclusão: existe uma leve evidência de heterocedasticidade mas não foi significativa. 
#Uma possível razão desta violação é a presença de um valor atípico 
#(indivíduo 25 do banco de dados, 77 anos e 22 mmHg de PIO).

#Possíveis melhorias no ajuste: (1) excluir o indivíduo 25 (somente se houver suspeitas 
#reais de erro dos valores); (2) modelar a dispersão dos dados (modelo heterocedástico).


ggplot(data = dados, mapping = aes(x = idade, y = pio)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label..,
                                          sep = "*plain(\",\")~~")),
                        label.x = 35, label.y = 26) +
  theme_classic()


# Conclusão Final:
#Existe uma forte evidência de associação (linear) entre idade e PIO.
#Coeficiente de correlação de Pearson foi de 0,84 (IC; 95% 0,68; 0,93) e o R2 = 71%.
#O aumento em ano na idade, aumenta a PIO média em 0,23 (IC; 95% 0,17; 0,29) mmHg. 
#Ou o aumento em quatro anos na idade, aumenta a PIO mÃ©dia em cerca de 1 mmHg (IC; 95% 0,68; 1,16) mmHg.



### Tópicos Especiais ###


# 1- Excluir o caso 25 (atípico): somente válido se houver uma justificativa para excluí-lo.
dados1 <- dados[-25,]
mod1 <- lm(pio ~ idade, dados1)
summary(mod1)
par(mfrow=c(2,2)) 
plot(mod1, which=c(1:4), add.smooth=T,pch=20)
shapiro.test(mod1$residuals)
summary(lm(abs(residuals(mod1))~fitted(mod1)))
#Observe que a qualidade do ajuste melhorou em todos os aspectos.
# A conclusã£o continua mostrando uma associação entre PIO e idade. 
# No entanto, observe que beta^ passou de 0,23 para 0,27.

# 2- Modelar também a dispersÃ£o
#Como existe uma leve evidência contra a suposição de homocedasticidade
#vamos modelar também o desvio-padrão do erro em função de idade. 
#Ou seja, vamos testar se a dispersão aumenta com o aumento da idade.
#Para fazer isso precisamos utilizar o pacote gamlss. Não vamos tirar o paciente 25.
mod2 <- gamlss(dados$pio ~ dados$idade, sigma.formula = ~ dados$idade,family=NO)
summary(mod2) 
wp(mod2)
plot(mod2, which=c(1:4), add.smooth=T,pch=20)
shapiro.test(mod2$residuals)
summary(lm(abs(residuals(mod2))~fitted(mod2)))
#Não foi significativo o efeito de idade no modelo da dispersão. 
#Os gráficos confirmam não existir evidência contra a suposição de homocedasticidade.
