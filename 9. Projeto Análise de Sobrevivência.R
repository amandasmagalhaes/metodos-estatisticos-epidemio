# Remove objetos
#rm(list = ls())





# Pacotes
if (!require(pacman)) install.packages('pacman')
library(pacman)
pacman::p_load(psych, dplyr, car, RVAideMemoire, rstatix, janitor, survival, survminer)





# Banco de dados

#Dados completos
dados <- read.table('http://www.est.ufmg.br/~enricoc/pdf/avancados_medicina/514_pacientes_multivariada.txt', 
                    head=T)
dados <- dados[complete.cases(dados[c(1:9)]),]
View(dados)
glimpse(dados)

#Dados incompletos
dados2 <- read.table('http://www.est.ufmg.br/~enricoc/pdf/avancados_medicina/514_pacientes_multivariada.txt', 
                    head=T)
dados2 <- dados2[!complete.cases(dados2[c(1:9)]),]


# Excluir variáveis
dados <- subset(dados, select = -c(CATCLARK, MITOSESHE))
dados2 <- subset(dados2, select = -c(CATCLARK, MITOSESHE))
glimpse(dados)





# Recodificar variáveis resposta em 0 e 1
dados$METASTASE <- Recode(dados$METASTASE, '1 = 1; 2 = 0', as.factor=F)
dados2$METASTASE <- Recode(dados2$METASTASE, '1 = 1; 2 = 0', as.factor=F)
glimpse(dados)





# Alterar classe das variÃ¡veis
dados$GENERO = as.factor(dados$GENERO)
dados$GRUPOIDADE = as.factor(dados$GRUPOIDADE)
dados$LOCALMMPR = as.factor(dados$LOCALMMPR)
dados$TIPOHE = as.factor(dados$TIPOHE)
dados$CATBRESLOW = as.factor(dados$CATBRESLOW)
dados$ULCERHE = as.factor(dados$ULCERHE)
glimpse(dados)

dados2$GENERO = as.factor(dados2$GENERO)
dados2$GRUPOIDADE = as.factor(dados2$GRUPOIDADE)
dados2$LOCALMMPR = as.factor(dados2$LOCALMMPR)
dados2$TIPOHE = as.factor(dados2$TIPOHE)
dados2$CATBRESLOW = as.factor(dados2$CATBRESLOW)
dados2$ULCERHE = as.factor(dados2$ULCERHE)
glimpse(dados)




# Análise Descritiva



describe(dados$TEMPOACOMP)


data.frame(table(dados$METASTASE)) %>%
  mutate(Rel_Freq = Freq/sum(Freq)) %>%
  rename(METASTASE = Var1)


data.frame(table(dados$GENERO)) %>%
  mutate(Rel_Freq = Freq/sum(Freq)) %>%
  rename(GENERO = Var1)


data.frame(table(dados$GRUPOIDADE)) %>%
  mutate(Rel_Freq = Freq/sum(Freq)) %>%
  rename(GRUPOIDADE = Var1)


data.frame(table(dados$LOCALMMPR)) %>%
  mutate(Rel_Freq = Freq/sum(Freq)) %>%
  rename(LOCALMMPR = Var1)


data.frame(table(dados$TIPOHE)) %>%
  mutate(Rel_Freq = Freq/sum(Freq)) %>%
  rename(TIPOHE = Var1)


data.frame(table(dados$CATBRESLOW)) %>%
  mutate(Rel_Freq = Freq/sum(Freq)) %>%
  rename(CATBRESLOW = Var1)


data.frame(table(dados$ULCERHE)) %>%
  mutate(Rel_Freq = Freq/sum(Freq)) %>%
  rename(ULCERHE = Var1)





# Perda dos dados
colSums(is.na(dados))
round(colSums(is.na(dados)) * 100 / nrow(dados), 1)





# Análise Univariada


# Kaplan-Meier



splots1 <- list()



ekmGENERO <- survfit(Surv(TEMPOACOMP, METASTASE) ~ GENERO, data = dados)
summary(ekmGENERO)$table

splots1[[1]] <- ggsurvplot(ekmGENERO, palette = 'Dark2',
           xlab = 'Tempo (semanas)', ylab = 'Probabilidade de ocorrência',
           legend.title = 'Sexo', legend.labs = c('Masculino', 'Feminino'), legend = 'right',
           font.x = 11, font.y = 11, font.legend = 12, font.tickslab = 10,
           #risk.table = T,  risk.table.col = 'strata',
           #risk.table.title = 'Numero de pacientes em risco por tempo', surv.median.line = 'hv', 
           ggtheme = theme_survminer())

survdiff(Surv(TEMPOACOMP, METASTASE) ~ GENERO, data = dados, rho = 0) # logrank
survdiff(Surv(TEMPOACOMP, METASTASE) ~ GENERO, data = dados, rho = 1) # apr. Wilcoxon/TaroneWare

ggsave('ekmSEXO.png', plot = print(splots1[[1]]), width = 5, height = 4, dpi = 1000)





ekmGRUPOIDADE <- survfit(Surv(TEMPOACOMP, METASTASE) ~ GRUPOIDADE, data = dados)
summary(ekmGRUPOIDADE)$table

ggsurvplot(ekmGRUPOIDADE, pval = T, palette = 'Dark2',
           xlab = 'Tempo (semanas)', ylab = 'Probabilidade de ocorrência', legend = 'right',
           legend.title = 'Idade', legend.labs = c('18-40 anos', '> 40-60 anos', '> 60 anos'),
           font.x = 11, font.y = 11, font.legend = 12, font.tickslab = 10,
           #risk.table = T,  risk.table.col = 'strata',
           #risk.table.title = 'Numero de pacientes em risco por tempo', surv.median.line = 'hv', 
           ggtheme = theme_survminer())

survdiff(Surv(TEMPOACOMP, METASTASE) ~ GRUPOIDADE, data = dados, rho = 0) # logrank
survdiff(Surv(TEMPOACOMP, METASTASE) ~ GRUPOIDADE, data = dados, rho = 1) # apr. Wilcoxon/TaroneWare

pairwise_survdiff(Surv(TEMPOACOMP, METASTASE) ~ GRUPOIDADE, data = dados) #Comparações multiplas





ekmLOCALMMPR <- survfit(Surv(TEMPOACOMP, METASTASE) ~ LOCALMMPR, data = dados) 
summary(ekmLOCALMMPR)$table

splots1[[2]] <- ggsurvplot(ekmLOCALMMPR, pval = T, palette = 'Dark2',
           xlab = 'Tempo (semanas)', ylab = 'Probabilidade de ocorrência', legend = 'right',
           legend.title = 'Localização do sítio primário',
           legend.labs = c('Cabeça e Pescoço + Troco', 'Membros Superiores + Inferiores', 'Acral'),
           font.x = 11, font.y = 11, font.legend = 12, font.tickslab = 10,
           #risk.table = T,  risk.table.col = 'strata',
           #risk.table.title = 'Numero de pacientes em risco por tempo', surv.median.line = 'hv', 
           ggtheme = theme_survminer())

survdiff(Surv(TEMPOACOMP, METASTASE) ~ LOCALMMPR, data = dados, rho = 0) # logrank
survdiff(Surv(TEMPOACOMP, METASTASE) ~ LOCALMMPR, data = dados, rho = 1) # apr. Wilcoxon/TaroneWare

pairwise_survdiff(Surv(TEMPOACOMP, METASTASE) ~ LOCALMMPR, data = dados) #Comparações multiplas

ggsave('ekmLOCALMMPR.png', plot = print(splots1[[2]]), width = 5, height = 4, dpi = 1000)





splots2 <- list()



ekmTIPOHE <- survfit(Surv(TEMPOACOMP, METASTASE) ~ TIPOHE, data = dados)
summary(ekmTIPOHE)$table

splots2[[3]] <- ggsurvplot(ekmTIPOHE, palette = 'Dark2',
           xlab = 'Tempo (semanas)', ylab = 'Probabilidade de ocorrência', legend = 'right',
           legend.title = 'Tipo clinico patológico',
           legend.labs = c('Extensivo Superficial +  Lentigo Maligno', 'Nodular', 'Lentiginoso Acral'),
           font.x = 11, font.y = 11, font.legend = 12, font.tickslab = 10,
           #risk.table = T,  risk.table.col = 'strata',
           #risk.table.title = 'Numero de pacientes em risco por tempo', surv.median.line = 'hv', 
           ggtheme = theme_survminer())

survdiff(Surv(TEMPOACOMP, METASTASE) ~ TIPOHE, data = dados, rho = 0) # logrank
survdiff(Surv(TEMPOACOMP, METASTASE) ~ TIPOHE, data = dados, rho = 1) # apr. Wilcoxon/TaroneWare

pairwise_survdiff(Surv(TEMPOACOMP, METASTASE) ~ TIPOHE, data = dados) #Comparações multiplas

ggsave('ekmTIPOHE.png', plot = print(splots2[[3]]), width = 5, height = 4, dpi = 1000)





ekmCATBRESLOW <- survfit(Surv(TEMPOACOMP, METASTASE) ~ CATBRESLOW, data = dados)
summary(ekmCATBRESLOW)$table

splots2[[4]] <- ggsurvplot(ekmCATBRESLOW, palette = 'Dark2',
           xlab = 'Tempo (semanas)', ylab = 'Probabilidade de ocorrência', legend = 'right',
           legend.title = 'Índice de Breslow', legend.labs = c('<= 1 mm', '1-4 mm', '> 4 mm'),
           font.x = 11, font.y = 11, font.legend = 12, font.tickslab = 10,
           #risk.table = T,  risk.table.col = 'strata',
           #risk.table.title = 'Numero de pacientes em risco por tempo', surv.median.line = 'hv', 
           ggtheme = theme_survminer())

survdiff(Surv(TEMPOACOMP, METASTASE) ~ CATBRESLOW, data = dados, rho = 0) # logrank
survdiff(Surv(TEMPOACOMP, METASTASE) ~ CATBRESLOW, data = dados, rho = 1) # apr. Wilcoxon/TaroneWare

pairwise_survdiff(Surv(TEMPOACOMP, METASTASE) ~ CATBRESLOW, data = dados) #Comparações multiplas

ggsave('ekmCATBRESLOW.png', plot = print(splots2[[4]]), width = 5, height = 4, dpi = 1000)





splots3 <- list()



ekmULCERHE <- survfit(Surv(TEMPOACOMP, METASTASE) ~ ULCERHE, data = dados)
summary(ekmULCERHE)$table

splots3[[5]] <- ggsurvplot(ekmULCERHE, palette = 'Dark2',
           xlab = 'Tempo (semanas)', ylab = 'Probabilidade de ocorrência', legend = 'right',
           legend.title = 'Ulceração histológica', legend.labs = c('Não', 'Sim'),
           font.x = 11, font.y = 11, font.legend = 12, font.tickslab = 10,
           #risk.table = T,  risk.table.col = 'strata',
           #risk.table.title = 'Numero de pacientes em risco por tempo', surv.median.line = 'hv', 
           ggtheme = theme_survminer())

survdiff(Surv(TEMPOACOMP, METASTASE) ~ ULCERHE, data = dados, rho = 0) # logrank
survdiff(Surv(TEMPOACOMP, METASTASE) ~ ULCERHE, data = dados, rho = 1) # apr. Wilcoxon/TaroneWare

ggsave('ekmULCERHE.png', plot = print(splots3[[5]]), width = 5, height = 4, dpi = 1000)



#arrange_ggsurvplots(splots1, print = TRUE, ncol = 1, nrow = 2)
#ggsave('ekm.png', arrange_ggsurvplots(splots, print = FALSE, ncol = 2, nrow = 3))









# Análise Multivariada





# Recodificar variáveis
dados$GENERO <- Recode(dados$GENERO, '1 = "Masculino"; 2 = "Feminino"', as.factor = T)

dados$GRUPOIDADE <- Recode(dados$GRUPOIDADE, '1 = "18-40 anos"; 2 = "> 40-60 anos"; 3 = "> 60 anos"', 
                            as.factor = T)

dados$LOCALMMPR <- Recode(dados$LOCALMMPR, '1 = "Cabeça e Pescoço + Troco"; 
                           2 = "Membros Superiores + Inferiores"; 3 = "Acral"', 
                            as.factor = T)

dados$TIPOHE <- Recode(dados$TIPOHE, '1 = "Extensivo Superficial + Lentigo Maligno"; 
                           3 = "Nodular"; 4 = "Lentiginoso Acral"', 
                           as.factor = T)

dados$CATBRESLOW <- Recode(dados$CATBRESLOW, '1 = "< 1 mm"; 2 = "1-4 mm"; 3 = "> 4 mm"', as.factor = T)

dados$ULCERHE <- Recode(dados$ULCERHE, '1 = "Não"; 2 = "Sim"', as.factor = T)

glimpse(dados)



# Modelo de Regressão de Cox (modelo de taxas de falha proporcionais)
#Manter todas as covariáveis com pelo menos um valor-p < 0,25



#Passo 1: Ajustar todos os modelos contendo uma unica covariável. 
#Incluir todas as covariáveis que forem significativas ao nivel de 0,10.

coxGENERO <- coxph(Surv(TEMPOACOMP, METASTASE)~GENERO,
              data = dados, x = T, method = 'breslow')
summary(coxGENERO)


coxGRUPOIDADE <- coxph(Surv(TEMPOACOMP, METASTASE)~GRUPOIDADE,
                   data = dados, x = T, method = 'breslow')
summary(coxGRUPOIDADE)


coxLOCALMMPR <- coxph(Surv(TEMPOACOMP, METASTASE)~LOCALMMPR,
                       data = dados, x = T, method = 'breslow')
summary(coxLOCALMMPR)


coxLTIPOHE <- coxph(Surv(TEMPOACOMP, METASTASE)~TIPOHE,
                      data = dados, x = T, method = 'breslow')
summary(coxLTIPOHE)


coxCATBRESLOW <- coxph(Surv(TEMPOACOMP, METASTASE)~CATBRESLOW,
                      data = dados, x = T, method = 'breslow')
summary(coxCATBRESLOW)


coxULCERHE <- coxph(Surv(TEMPOACOMP, METASTASE)~ULCERHE,
                    data = dados, x = T, method = 'breslow')
summary(coxULCERHE)



#Passo 2: As covariáveis significativas no passo 1 são então ajustadas conjuntamente. 
#Ajustamos modelos reduzidos, excluindo uma unica covariável.

cox1 <- coxph(Surv(TEMPOACOMP, METASTASE)~GENERO+TIPOHE+CATBRESLOW+ULCERHE+GRUPOIDADE,
             data = dados, x = T, method = 'breslow')
summary(cox1)



#Passo 3: Neste passo as covariáveis excluidas no passo 2 retornam ao modelo 
#para confirmar que elas não são estatisticamente significantes.



#Passo 4: Neste passo retornamos com as covariáveis excluidas no passo 1 
#para confirmar que elas não são estatisticamente significantes.

cox1GRUPOIDADE <- coxph(Surv(TEMPOACOMP, METASTASE)~GENERO+TIPOHE+CATBRESLOW+ULCERHE+GRUPOIDADE,
              data = dados, x = T, method = 'breslow')
summary(cox1GRUPOIDADE)



#Passo 5: Ajustamos um modelo incluindo as covariáveis significativas no passo 4. 
#Neste passo testamos se alguma delas podem ser retiradas do modelo.



#Passo 6: Ajustamos o modelo final para os efeitos principais. 
#Devemos verificar a possibilidade de inclusão de termos de interação.

cox1 <- coxph(Surv(TEMPOACOMP, METASTASE)~GENERO+TIPOHE+CATBRESLOW+ULCERHE,
              data = dados, x = T, method = 'breslow')
summary(cox1)


cox2 <- coxph(Surv(TEMPOACOMP, METASTASE)~GENERO+TIPOHE+CATBRESLOW+ULCERHE + GENERO:TIPOHE,
              data = dados, x = T, method = 'breslow')
summary(cox2)


cox3 <- coxph(Surv(TEMPOACOMP, METASTASE)~GENERO+TIPOHE+CATBRESLOW+ULCERHE + GENERO:CATBRESLOW,
              data = dados, x = T, method = 'breslow')
summary(cox3)


cox4 <- coxph(Surv(TEMPOACOMP, METASTASE)~GENERO+TIPOHE+CATBRESLOW+ULCERHE + GENERO:ULCERHE,
              data = dados, x = T, method = 'breslow')
summary(cox4)


cox5 <- coxph(Surv(TEMPOACOMP, METASTASE)~GENERO+TIPOHE+CATBRESLOW+ULCERHE + TIPOHE:CATBRESLOW,
              data = dados, x = T, method = 'breslow')
summary(cox5) 


cox6 <- coxph(Surv(TEMPOACOMP, METASTASE)~GENERO+TIPOHE+CATBRESLOW+ULCERHE + TIPOHE:ULCERHE,
              data = dados, x = T, method = 'breslow')
summary(cox6)


cox7 <- coxph(Surv(TEMPOACOMP, METASTASE)~GENERO+TIPOHE+CATBRESLOW+ULCERHE + CATBRESLOW:ULCERHE,
              data = dados, x = T, method = 'breslow')
summary(cox7)




#Modelo Final

cox1 <- coxph(Surv(TEMPOACOMP, METASTASE)~GENERO+TIPOHE+CATBRESLOW+ULCERHE,
              data = dados, x = T, method = 'breslow')
summary(cox1)

#O valor de p para todos os três testes gerais (likelihood, Wald, and score)) 
#são significativos, indicando que o modelo são significativo.

#razão de risco HR = exp(coef) 



# Adequação do Modelo de Cox - Teste de Schoenfeld
#Suposição básica: taxas de falha proporcionais (Resíduos de Schoenfeld)

resid(cox1, type = 'scaledsch')
cox.zph(cox1, transform = 'identity')
par(mfrow=c(2,2))
plot(cox.zph(cox1))

#Não existem evidências contra a violação da suposição de proporcionalidade das taxas de falha.



GENERO<-matrix(c(87,151,132,144),nc=2)
chisq.test(GENERO,correct=F)

GRUPOIDADE<-matrix(c(62,100,76,61,95,110),nc=2)
chisq.test(GRUPOIDADE,correct=F)

LOCALMMPR<-matrix(c(125,70,43,150,75,42),nc=2)
chisq.test(LOCALMMPR,correct=F)

TIPOHE<-matrix(c(167,44,27,73,30,17),nc=2)
chisq.test(TIPOHE,correct=F)

CATBRESLOW<-matrix(c(140,77,21,96,79,23),nc=2)
chisq.test(CATBRESLOW,correct=F)

ULCERHE<-matrix(c(184,54,17,16),nc=2)
chisq.test(ULCERHE,correct=F)



################################################################################

# Frequências e Teste qui-quadrado

dados %>%
  tabyl(GENERO, METASTASE) %>%
  adorn_totals(c('row', 'col')) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns %>%
  adorn_title('combined') %>%
  knitr::kable()
chisq.test(dados$GENERO, dados2$GENERO, correct = F)
#p<0,001


dados %>%
  tabyl(GRUPOIDADE, METASTASE) %>%
  adorn_totals(c('row', 'col')) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns %>%
  adorn_title('combined') %>%
  knitr::kable()
chisq.test(dados$GRUPOIDADE, dados$METASTASE, correct = F)
#p=0,311


dados %>%
  tabyl(LOCALMMPR, METASTASE) %>%
  adorn_totals(c('row', 'col')) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns %>%
  adorn_title('combined') %>%
  knitr::kable()
chisq.test(dados$LOCALMMPR, dados$METASTASE, correct = F)
#p=0,108


dados %>%
  tabyl(TIPOHE, METASTASE) %>%
  adorn_totals(c('row', 'col')) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns %>%
  adorn_title('combined') %>%
  knitr::kable()
chisq.test(dados$TIPOHE, dados$METASTASE, correct = F)
#p<0,001


dados %>%
  tabyl(CATBRESLOW, METASTASE) %>%
  adorn_totals(c('row', 'col')) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns %>%
  adorn_title('combined') %>%
  knitr::kable()
chisq.test(dados$CATBRESLOW, dados$METASTASE, correct = F)
#p<0,001


dados %>%
  tabyl(ULCERHE, METASTASE) %>%
  adorn_totals(c('row', 'col')) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns %>%
  adorn_title('combined') %>%
  knitr::kable()
chisq.test(dados$ULCERHE, dados$METASTASE, correct = F)
#p<0,001






# Teste t / Teste de Mann-Whitney

#Verificação da normalidade dos dados - Shapiro por grupo
byf.shapiro(TEMPOACOMP ~ METASTASE2, dados) 

#Verificação da homogeneidade de variância - Teste de Levene
leveneTest(TEMPOACOMP ~ METASTASE2, dados) 

#Teste t para amostras independentes
t.test(TEMPOACOMP ~ METASTASE, dados, var.qual = T)



#Teste de Mann-Whitney (Wilcoxon rank-sum test) - Comparação de medianas
wilcox.test(TEMPOACOMP ~ METASTASE2, dados)

dados %>% group_by(METASTASE2) %>%
  get_summary_stats(TEMPOACOMP, type = 'median_iqr')

#O teste de Mann-Whitney mostrou que a mediana do tempo de acompanhamento daqueles 
#com metastase são diferente da mediana daqueles sem metastase (P <0,001).

par(mfrow=c(1,2))
hist(dados$TEMPOACOMP[dados$METASTASE2 == 'Não'],
     ylab = 'Frequência', xlab = 'Metastase', main = 'Sem metastase')
hist(dados$TEMPOACOMP[dados$METASTASE2 == 'Sim'],
     ylab = 'Frequência', xlab = 'Metastase', main = 'Com metastase')

ggplot(dados, aes(x = METASTASE2, y = TEMPOACOMP))+
  geom_boxplot(outlier.colour = '#D95F02', outlier.size = 2)+
  scale_fill_brewer(palette='Dark2')+
  labs(x = 'Metastase', y = 'Tempo de segmento (dias)', title = '')+
  theme_survminer()+
  stat_compare_means(method = 'wilcox.test', label.x = 1.8, label.y = 380)



#Análise de variânncia (ANOVA)
summary(aov(TEMPOACOMP ~ METASTASE2, dados)) #0,03*
#Homocedasticidade
leveneTest(TEMPOACOMP ~ METASTASE2, dados, center=median) 
#O teste de Shapiro-Wilks verifica a normalidade nos residuos
shapiro.test(residuals(aov(TEMPOACOMP ~ METASTASE2, dados))) 