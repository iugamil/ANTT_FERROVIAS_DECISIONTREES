##################################################################################

# INICIANDO BIBLIOTECAS

##################################################################################

#library('tidyverse') Pacote básico de datawrangling
#library('ggplot2') Pacote gráficos
#library('rpart') Biblioteca de árvores
#library('rpart.plot') Conjunto com Rpart, plota a parvore
#library('gtools') funções auxiliares como quantcut,
#library('Rmisc') carrega a função sumarySE para a descritiva
#library('scales') importa paletas de cores
#library('caret') Funções úteis para machine learning
#library(viridis) paleta de cores
#library('plotROC')

library(tidyverse)
library(ggplot2)
library(dplyr)
library(rpart)
library(viridis)
library(rpart.plot)
library(gtools)
library(Rmisc)
library(scales)
library(randomForest)
library(caret)
library(plotROC)

#IMPORTANDO DATAFRAMES ANTT
print(paste0("IMPORTANDO AS BASES DE DADOS","-",now()))

base_2006 <- read.csv('C:/Users/Guilh/OneDrive/Área de Trabalho/GERAL/1-ANTT_FERROVIAS_DECISIONTREES/producao_origem_destino_2006.csv', sep=';', fileEncoding='latin1')
base_2007 <- read.csv2('C:/Users/Guilh/OneDrive/Área de Trabalho/GERAL/1-ANTT_FERROVIAS_DECISIONTREES/producao_origem_destino_2007.csv', sep=';', fileEncoding='latin1')
base_2008 <- read.csv2('C:/Users/Guilh/OneDrive/Área de Trabalho/GERAL/1-ANTT_FERROVIAS_DECISIONTREES/producao_origem_destino_2008.csv', sep=';', fileEncoding='latin1')
base_2009 <- read.csv2('C:/Users/Guilh/OneDrive/Área de Trabalho/GERAL/1-ANTT_FERROVIAS_DECISIONTREES/producao_origem_destino_2009.csv', sep=';', fileEncoding='latin1')
base_2010 <- read.csv2('C:/Users/Guilh/OneDrive/Área de Trabalho/GERAL/1-ANTT_FERROVIAS_DECISIONTREES/producao_origem_destino_2010.csv', sep=';', fileEncoding='latin1')
base_2011 <- read.csv2('C:/Users/Guilh/OneDrive/Área de Trabalho/GERAL/1-ANTT_FERROVIAS_DECISIONTREES/producao_origem_destino_2011.csv', sep=';', fileEncoding='latin1')
base_2012 <- read.csv2('C:/Users/Guilh/OneDrive/Área de Trabalho/GERAL/1-ANTT_FERROVIAS_DECISIONTREES/producao_origem_destino_2012.csv', sep=';', fileEncoding='latin1')
base_2013 <- read.csv2('C:/Users/Guilh/OneDrive/Área de Trabalho/GERAL/1-ANTT_FERROVIAS_DECISIONTREES/producao_origem_destino_2013.csv', sep=';', fileEncoding='latin1')
base_2014 <- read.csv2('C:/Users/Guilh/OneDrive/Área de Trabalho/GERAL/1-ANTT_FERROVIAS_DECISIONTREES/producao_origem_destino_2014.csv', sep=';', fileEncoding='latin1')
base_2015 <- read.csv2('C:/Users/Guilh/OneDrive/Área de Trabalho/GERAL/1-ANTT_FERROVIAS_DECISIONTREES/producao_origem_destino_2015.csv', sep=';', fileEncoding='latin1')
base_2016 <- read.csv2('C:/Users/Guilh/OneDrive/Área de Trabalho/GERAL/1-ANTT_FERROVIAS_DECISIONTREES/producao_origem_destino_2016.csv', sep=';', fileEncoding='latin1')
base_2017 <- read.csv2('C:/Users/Guilh/OneDrive/Área de Trabalho/GERAL/1-ANTT_FERROVIAS_DECISIONTREES/producao_origem_destino_2017.csv', sep=';', fileEncoding='latin1')
base_2018 <- read.csv2('C:/Users/Guilh/OneDrive/Área de Trabalho/GERAL/1-ANTT_FERROVIAS_DECISIONTREES/producao_origem_destino_2018.csv', sep=';', fileEncoding='latin1')
base_2019 <- read.csv2('C:/Users/Guilh/OneDrive/Área de Trabalho/GERAL/1-ANTT_FERROVIAS_DECISIONTREES/producao_origem_destino_2019.csv', sep=';', fileEncoding='latin1')
base_2020 <- read.csv2('C:/Users/Guilh/OneDrive/Área de Trabalho/GERAL/1-ANTT_FERROVIAS_DECISIONTREES/producao_origem_destino_2020.csv', sep=';', fileEncoding='latin1')
base_2021 <- read.csv2('C:/Users/Guilh/OneDrive/Área de Trabalho/GERAL/1-ANTT_FERROVIAS_DECISIONTREES/producao_origem_destino_2021.csv', sep=';', fileEncoding='latin1')
base_2022 <- read.csv2('C:/Users/Guilh/OneDrive/Área de Trabalho/GERAL/1-ANTT_FERROVIAS_DECISIONTREES/producao_origem_destino_2022.csv', sep=';', fileEncoding='latin1')
base_2023 <- read.csv2('C:/Users/Guilh/OneDrive/Área de Trabalho/GERAL/1-ANTT_FERROVIAS_DECISIONTREES/producao_origem_destino_2023.csv', sep=';', fileEncoding='latin1')


#########COMPILANDO AS BASES

print(paste0("COMPILANDO AS BASES","-",now()))
base_geral <- rbind(base_2006,base_2007,base_2008,
                    base_2009,base_2010,base_2011,
                    base_2012,base_2013,base_2014,
                    base_2015,base_2016,base_2017,
                    base_2018,base_2019,base_2020,
                    base_2021,base_2022,base_2023)


# EXCLUINDO O EXCESSO DE BASES

print(paste0("EXCLUINDO O EXCESSO DE BASES","-",now()))
rm(base_2006,base_2007,base_2008,
   base_2009,base_2010,base_2011,
   base_2012,base_2013,base_2014,
   base_2015,base_2016,base_2017,
   base_2018,base_2019,base_2020,
   base_2021,base_2022,base_2023)


# SUBSTITUINDO VALORES NA BASE 
print(paste0("SUBSTITUINDO VALORES","-",now()))
base_geral$TKU[is.na(base_geral$TKU)] <- 0
base_geral$TU[is.na(base_geral$TU)] <- 0

# NESTA PARTE ALGUNS NÚMEROS NÃO SÃO TRANSOFMRADOS DEVIDO AO TAMANHO
base_geral <- base_geral %>% mutate(TKU = as.double(TKU),TU =  as.double(TU))

# AQUI AJUSTAMOS OA VALORES QUE NÃO FORAM TRANSFORMADOS, ATRIBUINDO O NÚMERO 0
base_geral$TKU[is.na(base_geral$TKU)] <- 0
base_geral$TU[is.na(base_geral$TU)] <- 0


# INSERINDO COLUNA
base_geral <- base_geral %>% 
              mutate( ANO = str_sub(Mes_Ano,4,7) ) %>% 
              select(-Mes_Ano)


############ AGRUPANDO BASE DE DADOS
print(paste0("AGRUPANDO BASES","-",now()))
base_agrupada <- base_geral %>% 
  group_by(Mercadoria_ANTT, UF_Origem, UF_Destino, Ferrovia, ANO) %>% 
  summarise(TKU = sum(TKU), TU = sum(TU)) %>% 
  mutate(DT_FORMULA = paste('01-01-',ANO)) %>% 
  unique()%>%  
  mutate(DT_REF = as.Date(DT_FORMULA,
                        tryFormats = c("%d-%m-%Y", "%d/%m/%Y"))) %>% 
  select(UF_Origem, UF_Destino, Mercadoria_ANTT, Ferrovia, ANO, DT_REF,TKU,TU) 


#ORDENANDO A BASE AGRUPADA
base_agrupada <- base_agrupada[order(base_agrupada$UF_Origem,
                                     base_agrupada$UF_Destino,
                                     base_agrupada$Mercadoria_ANTT,
                                     base_agrupada$Ferrovia,
                                     base_agrupada$ANO),]


# CRIANDO COLUNA DE VAR_YOY E COD_ORIG E COD_DEST
base_agrupada <- base_agrupada %>% mutate(VAR_YOY = (TU - lag(TU, n= 1L))/lag(TU, n= 1L),
                                COD_ORIG = case_when( UF_Origem == 'AC' ~ 1,
                                UF_Origem == 'AL' ~ 2,
                                UF_Origem == 'AP' ~ 3,
                                UF_Origem == 'AM' ~ 4,
                                UF_Origem == 'BA' ~ 5,
                                UF_Origem == 'CE' ~ 6,
                                UF_Origem == 'ES' ~ 7,
                                UF_Origem == 'GO' ~ 8,
                                UF_Origem == 'MA' ~ 9,
                                UF_Origem == 'MT' ~ 10,
                                UF_Origem == 'MS' ~ 11,
                                UF_Origem == 'MG' ~ 12,
                                UF_Origem == 'PA' ~ 13,
                                UF_Origem == 'PB' ~ 14,
                                UF_Origem == 'PR' ~ 15,
                                UF_Origem == 'PE' ~ 16,
                                UF_Origem == 'PI' ~ 17,
                                UF_Origem == 'RJ' ~ 18,
                                UF_Origem == 'RN' ~ 19,
                                UF_Origem == 'RS' ~ 20,
                                UF_Origem == 'RO' ~ 21,
                                UF_Origem == 'RR' ~ 22,
                                UF_Origem == 'SC' ~ 23,
                                UF_Origem == 'SP' ~ 24,
                                UF_Origem == 'SE' ~ 25,
                                UF_Origem == 'TO' ~ 26,
                                UF_Origem == 'DF' ~ 27),
                                COD_DEST = case_when( UF_Destino == 'AC' ~ 1,
                                UF_Destino == 'AL' ~ 2,
                                UF_Destino == 'AP' ~ 3,
                                UF_Destino == 'AM' ~ 4,
                                UF_Destino == 'BA' ~ 5,
                                UF_Destino == 'CE' ~ 6,
                                UF_Destino == 'ES' ~ 7,
                                UF_Destino == 'GO' ~ 8,
                                UF_Destino == 'MA' ~ 9,
                                UF_Destino == 'MT' ~ 10,
                                UF_Destino == 'MS' ~ 11,
                                UF_Destino == 'MG' ~ 12,
                                UF_Destino == 'PA' ~ 13,
                                UF_Destino == 'PB' ~ 14,
                                UF_Destino == 'PR' ~ 15,
                                UF_Destino == 'PE' ~ 16,
                                UF_Destino == 'PI' ~ 17,
                                UF_Destino == 'RJ' ~ 18,
                                UF_Destino == 'RN' ~ 19,
                                UF_Destino == 'RS' ~ 20,
                                UF_Destino == 'RO' ~ 21,
                                UF_Destino == 'RR' ~ 22,
                                UF_Destino == 'SC' ~ 23,
                                UF_Destino == 'SP' ~ 24,
                                UF_Destino == 'SE' ~ 25,
                                UF_Destino == 'TO' ~ 26,
                                UF_Destino == 'DF' ~ 27))  


# SUBSTITUINDO VALORES NA BASE
base_agrupada$VAR_YOY[is.na(base_agrupada$VAR_YOY)] <- 0


# GERANDO A BASE PRE
print(paste0("CRIANDO BASE PRE","-",now()))
base_pre <- base_agrupada[base_agrupada$TU !=0 & base_agrupada$TKU !=0,] %>% 
  mutate(COD_FERROVIA = case_when(Ferrovia == 'FCA' ~ 10,
         Ferrovia == 'EFVM' ~ 20,
         Ferrovia == 'FTL' ~ 30,
         Ferrovia == 'RMC' ~ 40,
         Ferrovia == 'EFC' ~ 50,
         Ferrovia == 'FNSTN' ~ 60,
         Ferrovia == 'MRS' ~ 70,
         Ferrovia == 'RMO' ~ 80,
         Ferrovia == 'RMN' ~ 90,
         Ferrovia == 'RMS' ~ 100,
         Ferrovia == 'EFPO' ~ 110,
         Ferrovia == 'RMP' ~ 120,
         Ferrovia == 'FTC' ~ 130))

# FLAG DA VAR_YOY
base_pre$FLG_VAR_YOY <- case_when( base_pre$VAR_YOY > 0 ~ 'Y', base_pre$VAR_YOY <= 0 ~ 'N')

# CRIANDO BASE COMPLETA
print(paste0("CRIANDO BASE COMPLETA","-",now()))
base_completa <- rbind(base_pre)

# EXCLUINDO BASES IRRELEVANTES
rm(base_geral, base_agrupada)
#base_pre)


#INSERINDO COLUNA DESVIO_DA_MEDIA PARA SABER O QUANTO AQUELE NUMERO ESTA LONGE DA MEDIA
base_completa$DESVIO_TU_DA_MEDIA <- round(base_completa$TU-mean(base_completa$TU))


############################################

# ESTUDANDO A BASE - VENDO A DISTRIBUICAO

############################################
# VARIANCIA SOMA DO QUADRADO DOS DESVIOS/QTD ELEMENTOS
#DESVIO PADRÃO É A RAIZ QUADRADA DA VARIÂNCIA
print(paste0("ESTUDANDO A BASE - VENDO A DISTRIBUICAO","-",now()))

VARIANCIA_TU <- (sum((base_completa$TU)^2)/nrow(base_completa))
MEDIA_TU <- mean(base_completa$TU)
MEDIANA_TU <- median(base_completa$TU)
MINIMO_TU <- min(base_completa$TU, na.rm = T)
MAXIMO_TU <- max(base_completa$TU, na.rm = T)
Q1_TU <- quantile(base_completa$TU, probs = 0.25, na.rm = T)
Q2_TU <- quantile(base_completa$TU, probs = 0.5, na.rm = T)
Q3_TU <- quantile(base_completa$TU, probs = 0.75, na.rm = T)

VARIANCIA_DESV <- (sum((base_completa$DESVIO_TU_DA_MEDIA)^2)/nrow(base_completa))
MEDIA_DESV <- mean(base_completa$DESVIO_TU_DA_MEDIA)
MEDIANA_DESV <- median(base_completa$DESVIO_TU_DA_MEDIA)
MINIMO_DESV <- min(base_completa$DESVIO_TU_DA_MEDIA, na.rm = T)
MAXIMO_DESV <- max(base_completa$DESVIO_TU_DA_MEDIA, na.rm = T)
Q1_DESV <- quantile(base_completa$DESVIO_TU_DA_MEDIA, probs = 0.25, na.rm = T)
Q2_DESV <- quantile(base_completa$DESVIO_TU_DA_MEDIA, probs = 0.5, na.rm = T)
Q3_DESV <- quantile(base_completa$DESVIO_TU_DA_MEDIA, probs = 0.75, na.rm = T)



# INSERINDO A MEDIA DOS DESVIOS COMO COLUNA NO DATAFRAME
base_completa$MEDIA_DESV <- mean(base_completa$DESVIO_TU_DA_MEDIA)

# INSERINDO A MEDIA DAS TONELADAS COMO COLUNA NO DATAFRAME
base_completa$MEDIA_TU <- mean(base_completa$TU)


# INSERINDO COLUNA DOS QUARTIS NO DATAFRAME
base_completa$QUARTIL_TU <- case_when(base_completa$TU <= Q1_TU ~ 'Q1',
                                      base_completa$TU <= Q2_TU ~ 'Q2',
                                      base_completa$TU > Q2_TU ~ 'Q3')


# TRABALHANDO COM A MEDIA DOS DESVIOS, MONTANDO FLAG
base_completa$FLG_MEDIA_DESVIO <- case_when(base_completa$DESVIO_TU_DA_MEDIA > base_completa$MEDIA_DESV ~ 'MAIOR - POSIT',
                                            base_completa$DESVIO_TU_DA_MEDIA < base_completa$MEDIA_DESV ~ 'MENOR - NEG')



# ORGANIZANDO COLUNAS DO DATAFRAME, ORGANIZACAO VISUAL
base_completa <- base_completa %>% select(Mercadoria_ANTT, COD_FERROVIA, Ferrovia,
                                          COD_ORIG, UF_Origem, COD_DEST, UF_Destino, 
                                          ANO, DT_REF, TKU, TU, VAR_YOY, FLG_VAR_YOY,
                                          QUARTIL_TU, MEDIA_TU, DESVIO_TU_DA_MEDIA, 
                                          MEDIA_DESV, FLG_MEDIA_DESVIO)


##################################################

#CRIANDO BASE PARA USO NO MODELO

##################################################
print(paste0("CRIANDO BASE PARA USO NO MODELO","-",now()))
base_final <- base_completa %>% select(Mercadoria_ANTT,
                                       Ferrovia,
                                       COD_FERROVIA,
                                       ANO,
                                       UF_Origem,
                                       UF_Destino,
                                       TU,
                                       QUARTIL_TU,
                                       DESVIO_TU_DA_MEDIA,
                                       FLG_VAR_YOY,
                                       FLG_MEDIA_DESVIO) 


# CRIANDO FLAG PARA SABER SE O PRODUTO É MILHO
base_final$flg_mercadoria <- case_when(base_final$Mercadoria_ANTT == 'Grãos - Milho' ~ 'Y',
                                       base_final$Mercadoria_ANTT != 'Grãos - Milho' ~ 'N')


# CRIANDO FLAG PARA SABER SE O PRODUTO É MILHO E SE O YOY É POSITIVO AO MESMO TEMPO
base_final$flg_foco <- case_when(base_final$flg_mercadoria == 'Y' & base_final$FLG_VAR_YOY == 'Y' ~ 'Y',
                                 base_final$flg_mercadoria == 'N' | base_final$FLG_VAR_YOY == 'N' ~ 'N')



#AGRUPANDO A BASE DE DADOS E EXCLUINDO COLUNAS 
base_final <- base_final %>%  group_by(COD_FERROVIA,
                                     ANO,
                                     UF_Origem,
                                     QUARTIL_TU,
                                     DESVIO_TU_DA_MEDIA,
                                     FLG_VAR_YOY,
                                     FLG_MEDIA_DESVIO,
                                     flg_mercadoria,
                                     flg_foco) %>% 
                              summarise(TONELADAS = sum(TU))



# MODIFICANDO TIPO DE DADOS
base_final$ANO <- as.numeric(base_final$ANO)


##########################################

# CRIANDO O MODELO

##########################################
# Como é o comprtamento dos meus dados, considerando apenas
# as informações definidas como objetivo da analise?
print(paste0("CRIANDO O MODELO","-",now()))

# DEFININDO A BASE QUE SERA USADA NA ARVORE
base_arvore <- base_final %>% group_by(COD_FERROVIA, ANO, UF_Origem, 
                                       QUARTIL_TU, flg_foco) %>% 
                              summarise(TONELADAS = sum(TONELADAS))


# CATEGORIZANDO AS VARIAVEIS NUMERICAS
base_arvore$COD_FERROVIA <- quantcut(base_arvore$COD_FERROVIA, 4)
base_arvore$ANO <- quantcut(base_arvore$ANO, 4)

# ORGANIZANDO O DATAFRAME
base_arvore <- base_arvore %>% select(UF_Origem, COD_FERROVIA, ANO, TONELADAS,
                                      QUARTIL_TU, flg_foco)


###########################################################
###################### ARVORE
print(paste0("CRIANDO ARVORE","-",now()))
# ARVORE
set.seed(14263)
arvore <- rpart::rpart(flg_foco ~ .,
                       data= base_arvore,
                       xval=5,
                       parms = list(split = 'information'), # podemos trocar para  'information'
                       method='class', # Essa opção indica que a resposta é qualitativa
                       control=rpart.control(maxdepth = 30, cp=0)
)


# Definindo uma paleta de cores
#?viridis::turbo
paleta = viridis::turbo(2, begin = 0.9, end = 0.5)

# Plotando a árvore
rpart.plot::rpart.plot(arvore,
                       yesno = 2,
                       box.palette = paleta,
                       branch.lty = 2,
                       shadow.col = 'gray'
                       )
print(arvore)


#################################################################

# AVALIACAO DO MODELO

#################################################################
print(paste0("AVALIACAO DO MODELO","-",now()))

# verificando a importancia das variaveis
barplot(arvore$variable.importance)

# Probabilidade de ser o objetivo - Predizendo com a árvore
prob = predict( object = arvore, 
                newdata = base_arvore)


# Classificação para ser o objetivo
classificacao = prob[,2]>.5
head(classificacao)

# Matriz de confusão
tab <- table(classificacao, base_arvore$flg_foco)
tab

acc <- ((tab[1,1] + tab[2,2])/ sum(tab))*100
acc
