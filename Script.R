#Pacotes----
library(parameters)
library(performance)
library(lme4)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(viridis)
library(data.table)
library(MuMIn)
library(car)
library(emmeans)
library(multcomp)

#Loading data----
ic <- read.table('E:/Invacost/invacost.BR.txt', header=T) %>% 
  #Seleciona as colunas de interesse
  dplyr::select(Type = Type_2, 
                Cost = Cost_estimate_per_year_2017_USD_exchange_rate,
                Year = Impact_year,
                Species = Species,
                ID = Reference_title,
                Sector= Impacted_sector_2) %>% 
  #Ajusta os nomes das variaveis: tudo que for Damage_cost e renomeado para Damage, caso contrario Manage
  mutate(Class = as.factor(if_else(Type == "Damage_costs","Damage","Managment")),
         #Atribui valores binarios para Damage (1) e Management (0)
         Bin = if_else(Class == "Damage", 1,0),
         #Transforma em custos usando logaritmo
         LogCost = log(Cost),
         #Unifica nome das especies
         Species=gsub("Aedes aegypti/Aedes albopictus", "Aedes sp", Species),
         Species=gsub("Pinus elliottii/Pinus taeda", "Pinus sp", Species))  %>%
  arrange(Year) %>% 
  as_tibble()

#Separa por tipo
group <- ic %>% group_split(Type)
names(group) <- c("Damage","Managment","Mixed")

#Conta o numero de estudos por ano
studyYear <- list()
CostMean <- list()
for(i in 1:3){
  studyYear[[i]] <- group[[i]] %>% group_by(Year) %>% count(name="NStudy")
  CostMean[[i]] <- group[[i]] %>% group_by(Year) %>% summarise(RawCost=mean(Cost), LogCost=mean(LogCost))
}
names(studyYear) <- c("Damage","Managment","Mixed")
names(CostMean) <- c("Damage","Managment","Mixed")

Count.data <- data.table::rbindlist(studyYear,idcol = "Type")
Cost.data <- data.table::rbindlist(CostMean,idcol = "Type")
relative.data.Type <- bind_cols(Cost.data,Count.data) %>% dplyr::select(Type,Year,NStudy,RawCost,LogCost)

write.csv(relative.data.Type, 'E:/Invacost/RelativeCosts_Type.csv', row.names = F)
write.table(relative.data.Type, 'E:/Invacost/RelativeCosts_Type.txt', row.names = F, sep=" ")

#Sem separar por tipo de custo
studyYear <- ic %>% group_by(Year) %>% count(name="NStudy")
CostMean <- ic %>% group_by(Year) %>% summarise(RawCost=mean(Cost), LogCost=mean(LogCost))
relative.data <- left_join(studyYear,CostMean, by="Year") %>% mutate(rel.raw.cost = RawCost/NStudy,
                                                                     rel.log.cost = LogCost/NStudy) 
write.csv(relative.data, 'E:/Invacost/RelativeCosts.csv', row.names = F)
write.csv(relative.data, 'E:/Invacost/RelativeCosts.csv', row.names = F)


#Nao considera duplicadas
studyYear2 <- ic %>% group_by(Year) %>% distinct(ID, .keep_all = T) %>% count(name="NStudy")
CostMean2 <- ic %>% group_by(Year) %>% summarise(RawCost=mean(Cost), LogCost=mean(LogCost))
relative.data2 <- left_join(studyYear2,CostMean2, by="Year") %>% mutate(rel.raw.cost = RawCost/NStudy,
                                                                        rel.log.cost = LogCost/NStudy)

write.csv(relative.data2, 'E:/Invacost/RelativeCosts2.csv', row.names = F)
write.csv(relative.data2, 'E:/Invacost/RelativeCosts2.csv', row.names = F)
             
#Salvando dados
write.csv(relative.data, 'E:/Invacost/RelativeCosts.csv', row.names = F)
write.table(relative.data, 'E:/Invacost/RelativeCosts.txt', row.names = F, sep=" ")

#note que as relacoes negativas 
#Relacao entre o custo em log e o ano sem discriminar por tipo de custo (Damage ou Management)
ic %>% group_by(Year) %>% summarise(Cost=mean(Cost)) %>% filter(Year > 1994) %>% lm(Year ~ Cost, data= .) %>% summary()

#Gerando imagem da relacao
ic %>% group_by(Year) %>% summarise(Cost=mean(Cost)) %>% filter(Year > 1994) %>% ggplot(., aes(x=Year, y=Cost)) + geom_point() + geom_smooth(method = "lm") + theme_bw()

#Explorando relacoes individuais. Note que os dados apresentam uma tendencia. Entretanto, as variacoes do eixo y precisam ser tratadas. 
ic %>% group_by(Year) %>% ggplot(., aes(x=Year, y=Cost, color=Type)) + geom_point() + geom_smooth(method = "lm") + theme_bw()

#Para isso: 
#1. quebro os dados em tres planilhas distintas (Damage, Manage e Mixed) (linha 55). 
#2. Para cada nova planilha agrupo os dados por ano e calculo a media dos custos dentro de cada ano (linhas 59 - 63) 
#3. Combino os tres novos banco de dados em um unico novo dataset atribuindo rotulos para tipo de custo (Linha 68)

#Rejustando os dados----
#Separa os dados por Sector Type (Damage, Manage e Mixed)
list <- ic %>%  group_split(Type) 

#Atribui nome para as listas
names(list) <- c("Damage","Manage","Mixed")

new <- list()
for(i in 1:3){
#Agrupa os dados de cada classe por ano e calcula a media dos custos por ano. O processo e feito independentemente para cada classe
new[[i]] <- list[[i]] %>% group_by(Year) %>% dplyr::summarize(Cost = mean(Cost))
}
names(new) <- names(list)

#Linear models----
#Unifica as listas
d <- data.table::rbindlist(new, idcol="Type")

#Visualizando custo no tempo. Note que informacoes para os anos de 1984 ate 1994 ocorrem somente para a classe manejo. Por isso essas informacoes nao foram consideradas no modelo. Note tambem a presenca de dois outliers para a classe manejo (pontos verdes com cruz nos anos de  2016 e 2017). Essas informacoes tambem nao foram consideradas. Portanto, o dado foi limpo para que e travado para os anos de 1995 ate 2020. 
d %>% 
  ggplot(.,aes(x=Year,y=Cost, color=Type)) +  
  geom_point() +
  #controla intervalo de escala do eixo x
  scale_x_continuous(name="Year", breaks=seq(1984, 2020, by=2)) + 
  geom_smooth(method = "lm", se=T, alpha=.2) + 
  theme_bw() + 
  #adiciona linha vertical
  geom_vline(xintercept = 1995, linetype="dotted") +
  #Indicando outliers
  annotate("point", x = 2016, y = 17.70216, shape="+", size=3) + 
  annotate("point", x = 2017, y = 18.79108, shape="+", size=3) 

#limpa os dados para a classe de management
m <- new$Manage %>% filter(Cost < 16 & Year > 1994) 
#isola damage
d <- new$Damage 

#combina em um novo dataset
data <- list(Damage=d, Manage=m)
d2 <- data.table::rbindlist(data, idcol="Type")

Cost.model1 <- d2 %>% lm(Cost ~ Type, data=.)
Cost.model2 <- d2 %>% lm(Cost ~ Year, data=.)
Cost.model3 <- d2 %>% lm(Cost ~ Type + Year, data=.)
Cost.model4 <- d2 %>% lm(Cost ~ Type*Year, data=.)
Cost.model5 <- d2 %>% lmer(Cost ~ Type + (1|Year), data=.)

#Essa funcao compara os modelos usando varias metricas de performance que sao compativeis com os modelos (i.e., AIC e BIC). O argumento rank = T faz a media dos de todos as metricas de performance para considerar as particularidades de cada metrica. Neste caso, AIC corresponde a media das metricas (como esperado que ocorra).
compare_performance(Cost.model1,
                    Cost.model2,
                    Cost.model3,
                    Cost.model4,
                    Cost.model5,
                    rank = T)

#comparanco AIC com AICc:
data.frame(Model=c("Cost.model1",
                    "Cost.model2",
                    "Cost.model3",
                    "Cost.model4",
                    "Cost.model5"),
           AICc=c(AICc(Cost.model1),
                  AICc(Cost.model2),
                  AICc(Cost.model3),
                  AICc(Cost.model4),
                  AICc(Cost.model5)),
           AIC=c(AIC(Cost.model1),
                  AIC(Cost.model2),
                  AIC(Cost.model3),
                  AIC(Cost.model4),
                  AIC(Cost.model5))) %>% arrange(AICc)

#Visualiza a performance de cada modelo
model_performance(Cost.model1)
model_performance(Cost.model2)
model_performance(Cost.model3)
model_performance(Cost.model4)
model_performance(Cost.model5)

#Visualiza os parametros de cada modelo
model_parameters(Cost.model1)
model_parameters(Cost.model2)
model_parameters(Cost.model3)
model_parameters(Cost.model4)
model_parameters(Cost.model5)

#Modelando os custos em funcao do ano
#Multiple R-squared:  0.6185,	Adjusted R-squared:  0.5961, Shapiro-Wilk normality test = 0.6847 
manejo <- d2 %>% filter(Type == "Manage") %>% lm(Cost ~ Year, data=.) 
summary(manejo)
shapiro.test(manejo$residuals)

#Multiple R-squared:  0.5968,	Adjusted R-squared:  0.5776, Shapiro-Wilk normality test = 0.05329
dano <- d2 %>% filter(!Type == "Manage") %>% lm(Cost ~ Year, data=.)
summary(dano)
shapiro.test(dano$residuals)

#plota as relacoes (Figura do 2B do artigo)
d2 %>% 
  ggplot(.,aes(x=Year,y=Cost, color=Type)) +  
  geom_point() + 
  geom_smooth(method = "lm", se=T, alpha=.2)+
  theme_bw()

#ANOVA----
#Esses codigos foram gerados para criar os tabela de dados SEM.
sector <- ic %>% dplyr::select(Species, Type, Sector, Cost) %>% group_by(Species, Type) %>% count(Sector) %>% spread(Sector,n, fill=0)

cost <- ic %>% dplyr::select(Species, Type, Sector, Cost) %>% group_by(Species,Cost) %>% summarise(Cost=mean(Cost))

#Tabela SEM.
j_table <- left_join(sector, cost)
#Reorganiza a table SEM para análise da ANOVA. O mesmo padrao pode ser encontrado fazendo uma ANOVA com os dados brutos (objeto ic), porem alguns outliers aparecem.
data <- j_table %>% dplyr::select(Type, Cost)
m1=aov(Cost ~Type, data=data)
summary(m1)
shapiro.test(resid(m1))

#testando contraste
ct= emmeans(m1, ~ Type)
pairs(ct)

##teste mais robusto
summary(as.glht(pairs(ct)), test=adjusted("free"))

#GLMs----

GLM.model1 <- ic %>% glm(Bin ~ Cost, family = binomial(link = "logit"), data=.) 
summary(GLM.model1)
GLM.model2 <- ic %>% glm(Bin ~ Year, family = binomial(link = "logit"), data=.)
summary(GLM.model2)
GLM.model3 <- ic %>% glm(Bin ~ Cost + Year, family = binomial(link = "logit"), data=.)
summary(GLM.model3)
GLM.model4 <- ic %>% glm(Bin ~ Cost*Year , family = binomial(link = "logit"), data=.)  
summary(GLM.model4)
GLM.model5 <- ic %>% lme4::glmer(Bin ~ Cost + (1|Year), family = binomial(link = "logit"), data=.)  
summary(GLM.model5)

#Compara a performance dos modelos
compare_performance (GLM.model1,
                     GLM.model2,
                     GLM.model3,
                     GLM.model4, 
                     GLM.model5,
                     rank = T)

#Caso queira AICc:
data.frame(Model=c("GLM.model1",
                   "GLM.model2",
                   "GLM.model3",
                   "GLM.model4",
                   "GLM.model5"),
           AICc=c(AICc(GLM.model1),
                  AICc(GLM.model2),
                  AICc(GLM.model3),
                  AICc(GLM.model4),
                  AICc(GLM.model5)),
           AIC=c(AIC(GLM.model1),
                 AIC(GLM.model2),
                 AIC(GLM.model3),
                 AIC(GLM.model4),
                 AIC(GLM.model5))) %>% arrange(AICc)

#visualiza performance de cada modelo
model_performance (GLM.model1)
model_performance (GLM.model2)
model_performance (GLM.model3)
model_performance (GLM.model4)
model_performance (GLM.model5)

#Visualiza os parametros de cada modelo
model_parameters (GLM.model1)
model_parameters (GLM.model2)
model_parameters (GLM.model3)
model_parameters (GLM.model4)
model_parameters (GLM.model5)

#
ic %>% ggplot(., aes(x=Cost, y=Bin)) + geom_point(aes(colour=factor(Bin)), size=3) + geom_smooth(method = "glm",method.args = list(family = "binomial"), se = T) + labs(y="Probability") + theme_bw()
