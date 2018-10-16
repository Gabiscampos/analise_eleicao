#Analise de dados de São Paulo para prefeito em 2016
library(cepespR)
library(dplyr)

SP_2016<-get_votes(2016, "Prefeito", state = "SP")
SP_2016<-SP_2016 %>% filter(NOME_MUNICIPIO=="São Paulo") %>% filter(NUMERO_CANDIDATO<95) %>% filter(NUM_TURNO==1)

967190/sum(SP_2016$QTDE_VOTOS)

write.csv(SP_2016, "sp2016.csv")

9