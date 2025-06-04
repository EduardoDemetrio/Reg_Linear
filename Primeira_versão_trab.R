
#Carregando os Dados e formatando tabela


#Variável Resposta (Renda Média Domiciliar per Capita)

exp <-  read.csv("Trabalho_1/Renda Média Domiciliar per Capita_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character")

exp$Renda_M_10 <- as.numeric(gsub(",", ".", gsub("\\.", "", exp$X2010)))

exp$X2010 <- NULL ; exp$Região.a.que.Pertence <- NULL

summary(exp)

exp <- exp[-1, ] #Remove a primeira linha "Estado do Paraná"



hist(exp$Renda_M_10)

exp[exp$Renda_M_10 == max(exp$Renda_M_10),]
#PIB

PIB <- read.csv("Trabalho_1/PIB per Capita_Tabela.csv",  sep = "\t", fileEncoding = "UTF-8", skip = 1,  colClasses = "character")


PIB$PIB21 <- as.numeric(gsub("\\.", "", PIB$X2021))

PIB$Região.a.que.Pertence <- NULL ; PIB$X2021 <- NULL

summary(PIB)




#População 


pop <-  read.csv("Trabalho_1/População Estimada - IBGE_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )


pop$populacao24 <- as.numeric(gsub("\\.", "", pop$X2024))

pop$Região.a.que.Pertence <- NULL ; pop$X2024 <- NULL


summary(pop)


#Taxa de alfabetização

alf <-  read.csv("Trabalho_1/Taxa de Alfabetização_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )

alf$Município.Estado <- alf$X ; alf$X <- NULL ; alf$X.1 <- NULL




alf <- alf[-c(1,2), ]#Remove a primeira linha "Estado do Paraná"

head(alf)

cols <- setdiff(names(alf), "Município.Estado")
alf[cols] <- lapply(alf[cols], function(x) as.numeric(gsub(",", ".", x)))

summary(alf)



#Taxa bruta de natalidade
nat <-  read.csv("Trabalho_1/Taxa Bruta de Natalidade_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )

nat$natalidade23 <- as.numeric(gsub(",", ".",  nat$X2023))  

nat$X2023 <- NULL ; nat$Região.a.que.Pertence <- NULL

head(nat)

summary(nat)

nat <- nat[-1, ]#Remove a primeira linha "Estado do Paraná"




#Gravidez na adolescencia 

grav <-  read.csv("Trabalho_1/Gravidez na Adolescência_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )


grav$grav_15a17 <- as.numeric(gsub(",", ".",  grav$grav_15a17))  

grav$Município.Estado <- grav$X ; grav$X <- NULL

grav$X.1 <- NULL



grav <- grav[-1, ]#Remove a primeira linha "Estado do Paraná"

head(grav)

summary(grav)



#Densidade demografica

densi <-  read.csv("Trabalho_1/Densidade Demográfica_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )





densi$densi_demo24 <-  as.numeric(gsub(",", ".", gsub("\\.", "", densi$X2024)))

densi$X2024 <- NULL ; densi$Região.a.que.Pertence <- NULL



summary(densi)

head(densi)
densi[is.na(densi$densi_demo24), ]


densi <- densi[-1, ]#Remove a primeira linha "Estado do Paraná"


#Turismo 
tur <-  read.csv("Trabalho_1/Estabelecimentos nas ACTs_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1 )

tur$Município.Estado <-  tur$X ; tur$X <- NULL ; tur$X.1 <- NULL


tur$Total <-  as.numeric(tur$Total)

tur$Alimentação <-  as.numeric(tur$Alimentação)
tur <- tur[-c(1,2), ]#Remove a primeira linha "Estado do Paraná"

tur[is.na(tur)] <- 0
head(tur)

summary(tur)



urb <-  read.csv("Trabalho_1/Grau de Urbanização_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1,  colClasses = "character" )



urb$pct_urb22 <-  as.numeric(gsub(",", ".", gsub("\\.", "", urb$X2022))) ; urb$X2022 <- NULL


urb$Região.a.que.Pertence <-  NULL


summary(urb)


#Distância até a capital 

cap <-  read.csv("Trabalho_1/Distância à Capital_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )


cap$dist_cap21 <-  as.numeric(gsub(",", ".", gsub("\\.", "", cap$X2021))) ;  cap$X2021 <-  NULL


cap$Município.Estado <- cap$Município

cap$Município <- NULL

cap$Região.a.que.Pertence <- NULL
cap[is.na(cap)] <- 0


summary(cap)



#Unidades de Ensino 

ensino <- read.csv("Trabalho_1/Estabelecimentos_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )


ensino$qtd_ensino24 <-  as.numeric(gsub(",", ".", gsub("\\.", "", ensino$Total))) ; ensino$Total <-  NULL

ensino$Município.Estado <- ensino$X ; ensino$X <- NULL

ensino$X.1 <- NULL




#Esgoto 
sane <- read.csv("Trabalho_1/Atendimento de Esgoto_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )


sane$un_san23 <-  as.numeric(gsub(",", ".", gsub("\\.", "", sane$Unidades.Atendidas...Total))) ; sane$Unidades.Atendidas...Total <-  NULL

sane$Município.Estado <- sane$X ; sane$X.1 <- NULL ;sane$X <- NULL

summary(sane)


sane[is.na(sane)] <- 0


summary(ensino)

fecun <- read.csv("Trabalho_1/Taxa de Fecundidade_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )

fecun$taxa_fecun10 <-  as.numeric(gsub(",", ".", gsub("\\.", "", fecun$X2010))) ; fecun$X2010 <-  NULL

fecun$Região.a.que.Pertence <- NULL
summary(fecun)



#Docentes_Ensino basico

prof <- read.csv("Trabalho_1/Docentes_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )

summary(prof)

prof$qtd_prof24 <-  as.numeric(gsub(",", ".", gsub("\\.", "", prof$Total))) ; prof$Total <-  NULL

prof$Município.Estado <- prof$X ; prof$X <- NULL ; prof$X.1 <- NULL 


#pop_faixa etária 

fx_etaria <- read.csv("Trabalho_1/População Censitária_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )

head(fx_etaria)

# Lista de colunas que você quer tratar
cols <- c("X5.anos", "X6.anos", "X7.anos", "X8.anos", "X9.anos", 
          "X10.anos", "X11.anos", "X12.anos", "X13.anos", "X14.anos",
          "X15.anos", "X16.anos", "X17.anos", "X18.anos")


fx_etaria[cols] <- lapply(fx_etaria[cols], function(x) as.numeric(gsub("\\.", "", x)))

fx_etaria$Pop_5_18 <- rowSums(fx_etaria[cols], na.rm = TRUE)

fx_etaria$Município.Estado <- fx_etaria$X

summary(fx_etaria)



dados <- merge(exp, PIB, by = "Município.Estado")

dados <- merge(dados, pop, by = "Município.Estado")

dados <- merge(dados, alf, by = "Município.Estado")

dados <- merge(dados, nat, by = "Município.Estado")

dados <- merge(dados, grav, by = "Município.Estado")

dados <- merge(dados, densi, by = "Município.Estado")

dados <- merge(dados, tur, by = "Município.Estado")

dados <- merge(dados, urb, by = "Município.Estado")

dados <- merge(dados, cap, by = "Município.Estado")

dados <- merge(dados, ensino, by = "Município.Estado")

dados <- merge(dados, sane, by = "Município.Estado")

dados <- merge(dados, fecun, by = "Município.Estado")

dados <- merge(dados, prof, by = "Município.Estado")

dados <- merge(dados, fx_etaria[, c("Município.Estado", "Pop_5_18")], 
               by = "Município.Estado", all.x = TRUE)




dados$aln_por_prof <-  dados$Pop_5_18/ dados$qtd_prof24

dados$tamanho <- dados$populacao24 / dados$densi_demo24


dados$com_tur_km <- dados$Total / dados$tamanho


dados$densidade_saneamento <- dados$un_san23 / dados$tamanho
dados$unid_ensino_por_hab <- dados$qtd_ensino24 / dados$populacao24


dados$densidade_ensino <- dados$qtd_ensino24 / dados$tamanho

summary(dados)

dados$Município.Estado <- NULL

modelo <- lm(Renda_M_10 ~ ., data = dados)
summary(modelo)

mod_back_AIC <- step(modelo, direction = 'backward', k = 2)
summary(mod_back_AIC)


plot(modelo$residuals)

sort(cor(dados$Renda_M_10, dados[, -1], use = "complete.obs"),decreasing = T)

#Testando variáveis importantes para o modelo
modelo_reduzido <- lm(Renda_M_10 ~ PIB21 + populacao24 + X65_mais + pct_urb22 + dist_cap21 +taxa_fecun10 + aln_por_prof , data = dados) # A melhor dentre as variáveis de alfabetismo foi a 65 +
summary(modelo_reduzido)


plot(modelo_reduzido$residuals)


residuos_padronizados <- rstandard(modelo_reduzido)

# Plota os resíduos padronizados
plot(residuos_padronizados, 
     main = "Resíduos Padronizados",
     ylab = "Resíduos Padronizados",
     xlab = "Índice",
     pch = 20, col = "blue")
abline(h = 0, col = "red", lty = 2)


dados[which(residuos_padronizados > 3), ]




modelo_reduzido_2 <- lm(Renda_M_10 ~ PIB21 + populacao24 + X65_mais + natalidade23 , data = dados) 
summary(modelo_reduzido_2)



AIC(modelo_reduzido,modelo_reduzido_2 )#Aparentemente Natalidade não agregou pro modelo 

#------- Grav

dados$grav_15a17 <- ifelse(is.na(dados$grav_15a17), 0, dados$grav_15a17)

modelo_reduzido_3 <- lm(Renda_M_10 ~ PIB21 + populacao24 + X65_mais + grav_15a17 , data = dados) 
summary(modelo_reduzido_3) #Gravidez também não 


# -------- Densdemo 
modelo_reduzido_4 <- lm(Renda_M_10 ~ PIB21 + populacao24 + X65_mais + densi_demo24 , data = dados) 
summary(modelo_reduzido_4) #Densi também não 


# -------- Turismo 
modelo_reduzido_5 <- lm(Renda_M_10 ~ PIB21 + populacao24 + X65_mais + Total , data = dados) 
summary(modelo_reduzido_5) #Densi também não 





