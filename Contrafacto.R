# Analise do contrafactual dos deputados federais


#Baixar banco
rm(list = ls())


library(jsonlite)
library(tidyverse)
library(rvest)

ufs <- read_html("https://pt.wikipedia.org/wiki/Unidades_federativas_do_Brasil") %>% 
  html_table(fill = TRUE) %>% 
  first() %>% 
  pull("Abreviação") %>% 
  str_to_lower()

u0 <- "https://s.glbimg.com/jo/el/2018/apuracao/1-turno/UF/deputado-federal.json"

urls <- str_replace(u0, "UF", ufs)

tabelas <- vector("list", length = length(ufs))

for(i in seq_along(urls)){
  print(ufs[[i]])
  Sys.sleep(1)
  tabelas[[i]] <- fromJSON(urls[[i]])[["candidatos"]]
  tabelas[[i]]$UF <- NA
  tabelas[[i]]$UF <- ufs[[i]]
  
  tabela_parte <- tabelas[[i]]$votos
  
  tabelas[[i]] <- bind_cols(tabelas[[i]][,-7], tabela_parte)
}

banco <- as.data.frame(bind_rows(tabelas))

banco <- as.tibble(banco)
banco <- banco %>% rename(NR_CANDIDATO = numero, 
                          SG_UF = UF) %>% mutate(UF=str_to_upper(SG_UF))


#Baixar banco de coligações 
pasta<-file.path(getwd(),"consulta_coligacao_2018") #Informamos a pasta
arquivos<-list.files(pasta)# Identificamos todos os arquivos

for(j in arquivos){
  print(j)
  x<-data.table::fread(file.path(pasta,j)) %>% data.frame()
  if(length(grep("_sup.txt",j))>0){
    x[,setdiff(names(rec.part),names(x))]<-NA
    rec.part[,setdiff(names(x),names(rec.part))]<-NA
    x<-x[,names(rec.part)]
  }
  if(j==arquivos[1]){colig_2018<-x}else{colig_2018<-rbind(colig_2018,x)}
  rm(x)
}

#baixar votos por coligação/partido
total_coligacao <- read_excel("~/07. Mestrado FGV/00. CEPESP/08. Valor/Analises_eleicao/Votacao_partidos(4).xlsx")
total_coligacao<- total_coligacao %>% rename(coligacao=Coligacao, 
                                             UF = Estado, 
                                             quantidade=Total,
                                             partido = Sigla)

partido_colig<-total_coligacao %>% select(UF, coligacao, partido)


#Baixar eleitos
eleitos_2018<-banco %>% filter(eleito=="S")
eleitos_2018<-eleitos_2018 %>% group_by(UF) %>% summarise(eleitos_UF=n_distinct(nome))

#Calculo QE------------------------------------------------------------------------------------------
total_votos<- total_coligacao %>% group_by(UF) %>% summarise(votos_UF=sum(quantidade))
total_votos<- left_join(eleitos_2018, total_votos, by = "UF")
total_votos<- total_votos %>% mutate(QE=round(votos_UF/eleitos_UF))

#Vamos calcular as cadeiras por coligacao (sem sobras)-----------------------------------------------
  
total_coligacao<- total_coligacao %>% group_by(coligacao, UF) %>% 
  summarise(votos_colig=sum(quantidade))

total_coligacao<- left_join(total_coligacao, total_votos, by = "UF")
total_coligacao<-total_coligacao %>% mutate(cadeiras_colig=floor(votos_colig/QE))

sobras<-total_coligacao %>% group_by(UF, eleitos_UF) %>% summarise(eleitos_UFC=sum(cadeiras_colig)) %>% 
  mutate(dif=eleitos_UF-eleitos_UFC) %>% arrange(UF)

#agora vamos calcular as sobras por estado na regra antiga (nanicos não entram na distribuição)------------------------

total_coligacao_exnanicos<- total_coligacao %>% filter(cadeiras_colig>0)

#Vamos calcular a média com cada rodada

total_coligacao_exnanicos<-total_coligacao_exnanicos %>% arrange(UF, cadeiras_colig) %>% 
  group_by(UF, coligacao) %>% 
  mutate(media1=votos_colig/(cadeiras_colig+1)) %>% ungroup() %>%  group_by(UF) %>% 
  mutate(cadeiras_round1=(ifelse(media1==max(media1),1,0)+cadeiras_colig)) %>% 
  mutate(media2=votos_colig/(cadeiras_round1+1)) %>% ungroup() %>%  group_by(UF) %>% 
  mutate(cadeiras_round2=(ifelse(media2==max(media2),1,0)+cadeiras_round1)) %>% 
  mutate(media3=votos_colig/(cadeiras_round2+1)) %>% ungroup() %>%  group_by(UF) %>% 
  mutate(cadeiras_round3=(ifelse(media3==max(media3),1,0)+cadeiras_round2)) %>% 
  mutate(media4=votos_colig/(cadeiras_round3+1)) %>% ungroup() %>%  group_by(UF) %>% 
  mutate(cadeiras_round4=(ifelse(media4==max(media4),1,0)+cadeiras_round3)) %>% 
  mutate(media5=votos_colig/(cadeiras_round4+1)) %>% ungroup() %>%  group_by(UF) %>% 
  mutate(cadeiras_round5=(ifelse(media5==max(media5),1,0)+cadeiras_round4)) %>% 
  mutate(media6=votos_colig/(cadeiras_round5+1)) %>% ungroup() %>%  group_by(UF) %>% 
  mutate(cadeiras_round6=(ifelse(media6==max(media6),1,0)+cadeiras_round5)) %>% 
  mutate(media7=votos_colig/(cadeiras_round6+1)) %>% ungroup() %>%  group_by(UF) %>% 
  mutate(cadeiras_round7=(ifelse(media7==max(media7),1,0)+cadeiras_round6)) %>% 
  mutate(media8=votos_colig/(cadeiras_round7+1)) %>% ungroup() %>%  group_by(UF) %>% 
  mutate(cadeiras_round8=(ifelse(media8==max(media8),1,0)+cadeiras_round7)) %>% 
  mutate(media9=votos_colig/(cadeiras_round8+1)) %>% ungroup() %>%  group_by(UF) %>% 
  mutate(cadeiras_round9=(ifelse(media9==max(media9),1,0)+cadeiras_round8)) %>% 
  mutate(media10=votos_colig/(cadeiras_round9+1)) %>% ungroup() %>%  group_by(UF) %>% 
  mutate(cadeiras_round10=(ifelse(media10==max(media10),1,0)+cadeiras_round9)) %>% 
  mutate(media11=votos_colig/(cadeiras_round10+1)) %>% ungroup() %>%  group_by(UF) %>% 
  mutate(cadeiras_round11=(ifelse(media11==max(media11),1,0)+cadeiras_round10)) 
sp_sn<-total_coligacao_exnanicos %>% filter(UF=="SP")

#agora mesmo calculo com nanicos (sem atingir QE)

total_coligacao_comnaninos<-total_coligacao %>% arrange(UF, cadeiras_colig) %>% 
  group_by(UF, coligacao) %>% 
  mutate(media1=votos_colig/(cadeiras_colig+1)) %>% ungroup() %>%  group_by(UF) %>% 
  mutate(cadeiras_round1=(ifelse(media1==max(media1),1,0)+cadeiras_colig)) %>% 
  mutate(media2=votos_colig/(cadeiras_round1+1)) %>% ungroup() %>%  group_by(UF) %>% 
  mutate(cadeiras_round2=(ifelse(media2==max(media2),1,0)+cadeiras_round1)) %>% 
  mutate(media3=votos_colig/(cadeiras_round2+1)) %>% ungroup() %>%  group_by(UF) %>% 
  mutate(cadeiras_round3=(ifelse(media3==max(media3),1,0)+cadeiras_round2)) %>% 
  mutate(media4=votos_colig/(cadeiras_round3+1)) %>% ungroup() %>%  group_by(UF) %>% 
  mutate(cadeiras_round4=(ifelse(media4==max(media4),1,0)+cadeiras_round3)) %>% 
  mutate(media5=votos_colig/(cadeiras_round4+1)) %>% ungroup() %>%  group_by(UF) %>% 
  mutate(cadeiras_round5=(ifelse(media5==max(media5),1,0)+cadeiras_round4)) %>% 
  mutate(media6=votos_colig/(cadeiras_round5+1)) %>% ungroup() %>%  group_by(UF) %>% 
  mutate(cadeiras_round6=(ifelse(media6==max(media6),1,0)+cadeiras_round5)) %>% 
  mutate(media7=votos_colig/(cadeiras_round6+1)) %>% ungroup() %>%  group_by(UF) %>% 
  mutate(cadeiras_round7=(ifelse(media7==max(media7),1,0)+cadeiras_round6)) %>% 
  mutate(media8=votos_colig/(cadeiras_round7+1)) %>% ungroup() %>%  group_by(UF) %>% 
  mutate(cadeiras_round8=(ifelse(media8==max(media8),1,0)+cadeiras_round7)) %>% 
  mutate(media9=votos_colig/(cadeiras_round8+1)) %>% ungroup() %>%  group_by(UF) %>% 
  mutate(cadeiras_round9=(ifelse(media9==max(media9),1,0)+cadeiras_round8)) %>% 
  mutate(media10=votos_colig/(cadeiras_round9+1)) %>% ungroup() %>%  group_by(UF) %>% 
  mutate(cadeiras_round10=(ifelse(media10==max(media10),1,0)+cadeiras_round9)) %>% 
  mutate(media11=votos_colig/(cadeiras_round10+1)) %>% ungroup() %>%  group_by(UF) %>% 
  mutate(cadeiras_round11=(ifelse(media11==max(media11),1,0)+cadeiras_round10)) 

sp_cn<-total_coligacao_comnaninos %>% filter(UF=="SP")

#agora vamos limitar o número de rodadas por estado
total_coligacao_exnanicos<- total_coligacao_exnanicos %>% group_by(UF) %>% arrange(UF) %>% 
  mutate(cadeiras_round1=ifelse(sum(cadeiras_round1)>eleitos_UF, cadeiras_colig, cadeiras_round1)) %>% 
  mutate(cadeiras_round2=ifelse(sum(cadeiras_round2)>eleitos_UF, cadeiras_round1, cadeiras_round2)) %>% 
  mutate(cadeiras_round3=ifelse(sum(cadeiras_round3)>eleitos_UF, cadeiras_round2, cadeiras_round3)) %>% 
  mutate(cadeiras_round4=ifelse(sum(cadeiras_round4)>eleitos_UF, cadeiras_round3, cadeiras_round4)) %>% 
  mutate(cadeiras_round5=ifelse(sum(cadeiras_round5)>eleitos_UF, cadeiras_round4, cadeiras_round5)) %>% 
  mutate(cadeiras_round6=ifelse(sum(cadeiras_round6)>eleitos_UF, cadeiras_round5, cadeiras_round6)) %>% 
  mutate(cadeiras_round7=ifelse(sum(cadeiras_round7)>eleitos_UF, cadeiras_round6, cadeiras_round7)) %>% 
  mutate(cadeiras_round8=ifelse(sum(cadeiras_round8)>eleitos_UF, cadeiras_round7, cadeiras_round8)) %>% 
  mutate(cadeiras_round9=ifelse(sum(cadeiras_round9)>eleitos_UF, cadeiras_round8, cadeiras_round9)) %>% 
  mutate(cadeiras_round10=ifelse(sum(cadeiras_round10)>eleitos_UF, cadeiras_round9, cadeiras_round10)) %>% 
  mutate(cadeiras_round11=ifelse(sum(cadeiras_round11)>eleitos_UF, cadeiras_round10, cadeiras_round11))

total_coligacao_comnaninos<- total_coligacao_comnaninos %>% group_by(UF) %>% arrange(UF) %>% 
  mutate(cadeiras_round1=ifelse(sum(cadeiras_round1)>eleitos_UF, cadeiras_colig, cadeiras_round1)) %>% 
  mutate(cadeiras_round2=ifelse(sum(cadeiras_round2)>eleitos_UF, cadeiras_round1, cadeiras_round2)) %>% 
  mutate(cadeiras_round3=ifelse(sum(cadeiras_round3)>eleitos_UF, cadeiras_round2, cadeiras_round3)) %>% 
  mutate(cadeiras_round4=ifelse(sum(cadeiras_round4)>eleitos_UF, cadeiras_round3, cadeiras_round4)) %>% 
  mutate(cadeiras_round5=ifelse(sum(cadeiras_round5)>eleitos_UF, cadeiras_round4, cadeiras_round5)) %>% 
  mutate(cadeiras_round6=ifelse(sum(cadeiras_round6)>eleitos_UF, cadeiras_round5, cadeiras_round6)) %>% 
  mutate(cadeiras_round7=ifelse(sum(cadeiras_round7)>eleitos_UF, cadeiras_round6, cadeiras_round7)) %>% 
  mutate(cadeiras_round8=ifelse(sum(cadeiras_round8)>eleitos_UF, cadeiras_round7, cadeiras_round8)) %>% 
  mutate(cadeiras_round9=ifelse(sum(cadeiras_round9)>eleitos_UF, cadeiras_round8, cadeiras_round9)) %>% 
  mutate(cadeiras_round10=ifelse(sum(cadeiras_round10)>eleitos_UF, cadeiras_round9, cadeiras_round10)) %>% 
  mutate(cadeiras_round11=ifelse(sum(cadeiras_round11)>eleitos_UF, cadeiras_round10, cadeiras_round11))

#Agora vamos usar só a visão final
total_coligacao_exnanicos<-total_coligacao_exnanicos %>% select(1:7, 29)
total_coligacao_comnaninos<-total_coligacao_comnaninos %>% select(1:7, 29)

#Proximo passo é marcar no banco de candidato quem seria eleito -----------------------------------

#Juntamos o banco de candidatos com o calculo sem nanicos
partido_colig_1<-left_join(partido_colig, total_coligacao_exnanicos, by = c("UF", "coligacao"))
banco_eleitos_exnanicos<-left_join(banco, partido_colig_1, by = c("UF", "partido") ) %>% 
  select(-coligacao.x) %>% rename(coligacao=coligacao.y)

#Rankeamos os candidatos da coligação e quem atinge o ranking é considerado eleito
banco_eleitos_exnanicos<-banco_eleitos_exnanicos %>% group_by(UF, coligacao) %>% 
  mutate(posicao = dense_rank(desc(quantidade))) %>% ungroup() %>% 
  mutate(eleito_regra_antiga=ifelse((posicao<=cadeiras_round11),"S","N"))

#Próximo juntamos o banco de candidatos com o calculo com nanicos
partido_colig_2<-left_join(partido_colig, total_coligacao_comnaninos, by = c("UF", "coligacao"))
banco_eleitos_comnanicos<-left_join(banco, partido_colig_2, by = c("UF", "partido") ) %>% 
  select(-coligacao.x) %>% rename(coligacao=coligacao.y)

#Rankeamos os candidatos da coligação e quem atinge o ranking é considerado eleito
banco_eleitos_comnanicos<-banco_eleitos_comnanicos %>% group_by(UF, coligacao) %>% 
  mutate(ranking = dense_rank(desc(quantidade))) %>% ungroup() %>% 
  mutate(eleito_regra_antiga=ifelse(ranking<=cadeiras_round11,"S","N"))

write.csv(banco_eleitos_exnanicos, "banco_eleitos_exnanicos.csv")
write.csv(banco_eleitos_comnanicos, "banco_eleitos_comnanicos.csv")

write.csv(total_coligacao_comnaninos, "total_coligacao_comnanicos.csv")
write.csv(total_coligacao_exnanicos, "total_coligacao_exnanicos.csv")
