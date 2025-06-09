### O Trabalho vai seguir um fluxo para desenvolvimento.

### Consolidar as variaveis
### Limpeza e tratamento das variaveis
### Seleção de variaveis
### Analise descritiva (distribuição/mudança de magnitude/one hot encoder/ vizualizações)
### Aqui começa o n = 5 (tu aleatoriza o treino/validação) 
### Experimentações (tops 10, top 20.. etc)
### Escolha final
### Analise modelo final


### PRIMEIRA ETAPA - CONSOLIDAÇÃO DAS VARIAVEIS

## Vamos ler as tabelas

exp <-  read.csv("Trabalho_1/Renda Média Domiciliar per Capita_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character")
PIB <- read.csv("Trabalho_1/PIB_PERCapita_Tabela.csv",  sep = "\t", fileEncoding = "UTF-8", skip = 1,  colClasses = "character")
pop <-  read.csv("Trabalho_1/População Estimada - IBGE_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )
alf <-  read.csv("Trabalho_1/Taxa de Alfabetização_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )
nat <-  read.csv("Trabalho_1/Taxa Bruta de Natalidade_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )
grav <-  read.csv("Trabalho_1/Gravidez na Adolescência_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )
densi <-  read.csv("Trabalho_1/Densidade Demográfica_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )
tur <-  read.csv("Trabalho_1/Estabelecimentos nas ACTs_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1 )
urb <-  read.csv("Trabalho_1/Grau de Urbanização_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1,  colClasses = "character" )
cap <-  read.csv("Trabalho_1/Distância à Capital_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )
ensino <- read.csv("Trabalho_1/Estabelecimentos_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )
sane <- read.csv("Trabalho_1/Atendimento de Esgoto_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )
fecun <- read.csv("Trabalho_1/Taxa de Fecundidade_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )
prof <- read.csv("Trabalho_1/Docentes_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )
fx_etaria <- read.csv("Trabalho_1/População Censitária_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )
AgenciasBancarias <- read.csv("Trabalho_1/Agências Bancárias_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character")
Consumo <- read.csv("Trabalho_1/Consumo_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character")
ContagemPopulacao <- read.csv("Trabalho_1/Contagem da População_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character")
EstabelecimentoSaude <- read.csv("Trabalho_1/Estabelecimento Saúde.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character")
EstabelecimentosSetores <- read.csv("Trabalho_1/Estabelecimentos - Setores_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character")
AereAero <- read.csv("Trabalho_1/Aeroportos e Aeródromos_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character" )
TaxaMortalidade <- read.csv("Trabalho_1/Taxa de Mortalidade_Tabela.csv", sep = "\t", fileEncoding = "UTF-16LE", skip = 1, colClasses = "character")

## Transformando a coluna X em Municipio
alf$'Município.Estado' <- alf$X
grav$'Município.Estado' <- grav$X
tur$'Município.Estado' <- tur$X
ensino$'Município.Estado' <- ensino$X
sane$'Município.Estado' <- sane$X
prof$'Município.Estado' <- prof$X
cap$'Município.Estado' <- cap$Município
fx_etaria$'Município.Estado' <- fx_etaria$X
AgenciasBancarias$'Município.Estado' <- AgenciasBancarias$X
Consumo$'Município.Estado' <- Consumo$X
ContagemPopulacao$'Município.Estado' <- ContagemPopulacao$X
EstabelecimentoSaude$'Município.Estado' <- EstabelecimentoSaude$X
EstabelecimentosSetores$'Município.Estado' <- EstabelecimentosSetores$X
AereAero$'Município.Estado' <- AereAero$X
TaxaMortalidade$'Município.Estado' <- TaxaMortalidade$X




converter_colunas_numericas <- function(df) {
  cols <- setdiff(names(df), "Município.Estado")
  df[cols] <- lapply(df[cols], function(x) as.numeric(gsub(",", ".", x)))
  return(df)
}

# ## Vamos formatar as variaveis em numeric

exp$Renda_M_10 <- as.numeric(gsub(",", ".", gsub("\\.", "", exp$X2010)))
PIB$PIB21 <- as.numeric(gsub("\\.", "", PIB$X2021))
pop$populacao24 <- as.numeric(gsub("\\.", "", pop$X2024))
nat$natalidade23 <- as.numeric(gsub(",", ".",  nat$X2023))
grav$grav_15a17 <- as.numeric(gsub(",", ".",  grav$grav_15a17))
densi$densi_demo24 <-  as.numeric(gsub(",", ".", gsub("\\.", "", densi$X2024)))

alf <- converter_colunas_numericas(alf)
alf <- alf[-c(1),]  
tur <- converter_colunas_numericas(tur)
tur <- tur[-c(1),] ; tur$Total_Turismo <- tur$Total
urb$pct_urb22 <-  as.numeric(gsub(",", ".", gsub("\\.", "", urb$X2022)))
cap$dist_cap21 <-  as.numeric(gsub(",", ".", gsub("\\.", "", cap$X2021)))
ensino$qtd_ensino24 <-  as.numeric(gsub(",", ".", gsub("\\.", "", ensino$Total)))
sane$un_san23 <-  as.numeric(gsub(",", ".", gsub("\\.", "", sane$Unidades.Atendidas...Total)))
fecun$taxa_fecun10 <-  as.numeric(gsub(",", ".", gsub("\\.", "", fecun$X2010)))
prof$qtd_prof24 <-  as.numeric(gsub(",", ".", gsub("\\.", "", prof$Total)))
fx_etaria <- converter_colunas_numericas(fx_etaria)
fx_etaria <- fx_etaria[-c(1),]
AgenciasBancarias$Total_agencia <- as.numeric(gsub(",", ".", gsub("\\.", "", AgenciasBancarias$Total)))
Consumo$Total_consumo <- as.numeric(gsub(",", ".", gsub("\\.", "", Consumo$Total..Mwh.)))

ContagemPopulacao <- converter_colunas_numericas(ContagemPopulacao)
ContagemPopulacao <- ContagemPopulacao[-c(1),]
ContagemPopulacao$Total_popolucao <- ContagemPopulacao$Total ; 

excluir <- "Município.Estado"
names(ContagemPopulacao)[names(ContagemPopulacao) != excluir] <- 
  paste0(names(ContagemPopulacao)[names(ContagemPopulacao) != excluir], "_Contagem")


EstabelecimentoSaude$Total_saude <- as.numeric(gsub(",", ".", gsub("\\.", "", EstabelecimentoSaude$Total)))
EstabelecimentosSetores <- converter_colunas_numericas(EstabelecimentosSetores)
EstabelecimentosSetores <- EstabelecimentosSetores[-c(1),]
EstabelecimentosSetores$Total_Estabelecimentos <- EstabelecimentosSetores$Total
AereAero$Total_aeroportos <- as.numeric(gsub(",", ".", gsub("\\.", "", AereAero$Quantidade)))
TaxaMortalidade$Total_mortalidade <- as.numeric(gsub(",", ".", gsub("\\.", "", TaxaMortalidade$Geral..mil.habitantes.)))



## Removendo colunas e linhas

exp <- subset(exp, select = -c(Região.a.que.Pertence,X2010))
PIB <- subset(PIB, select = -c(Região.a.que.Pertence,X2021))
pop <- subset(pop, select = -c(Região.a.que.Pertence,X2024))
alf <- subset(alf, select = -c(X, X.1))
nat <- subset(nat, select = -c(Região.a.que.Pertence,X2023))
grav <- subset(grav, select = -c(X, X.1))
densi <- subset(densi, select = -c(Região.a.que.Pertence,X2024))
tur <- subset(tur, select = -c(X, X.1,Total))
urb <- subset(urb, select = -c(Região.a.que.Pertence,X2022))
cap <- subset(cap, select = -c(Região.a.que.Pertence,Município,X2021))
ensino <- subset(ensino, select = -c(X, X.1,Total))
sane <- subset(sane, select = -c(X, X.1,Unidades.Atendidas...Total))
fecun <- subset(fecun, select = -c(Região.a.que.Pertence,X2010))
prof<- subset(prof, select = -c(X, X.1,Total))
fx_etaria<- subset(fx_etaria, select = -c(X, X.1,Total))
AgenciasBancarias<- subset(AgenciasBancarias, select = -c(X, X.1,Total))
Consumo<- subset(Consumo, select = -c(X, X.1,Total..Mwh.))
ContagemPopulacao<- subset(ContagemPopulacao, select = -c(X, X.1,Total)) 
cols <- c("X5.anos", "X6.anos", "X7.anos", "X8.anos", "X9.anos", 
          "X10.anos", "X11.anos", "X12.anos", "X13.anos", "X14.anos",
          "X15.anos", "X16.anos", "X17.anos", "X18.anos")
fx_etaria$Pop_5_18 <- rowSums(fx_etaria[cols], na.rm = TRUE)

EstabelecimentoSaude<- subset(EstabelecimentoSaude, select = -c(X, X.1,Total))
EstabelecimentosSetores<- subset(EstabelecimentosSetores, select = -c(X, X.1,Total))
AereAero<- subset(AereAero, select = -c(X, X.1,Quantidade))
TaxaMortalidade<- subset(TaxaMortalidade, select = -c(X, X.1,Geral..mil.habitantes.))



## Verificando os NA da Base
dfs <- list(
  exp   = exp,
  PIB   = PIB,
  pop   = pop,
  alf   = alf,
  nat   = nat,
  grav  = grav,
  densi = densi,
  tur   = tur,
  urb   = urb,
  cap   = cap,
  ensino = ensino,
  sane  = sane,
  fecun = fecun,
  prof  = prof,
  fx_etaria = fx_etaria,
  AgenciasBancarias = AgenciasBancarias,
  Consumo = Consumo,
  ContagemPopulacao = ContagemPopulacao,
  EstabelecimentoSaude = EstabelecimentoSaude,
  EstabelecimentosSetores = EstabelecimentosSetores,
  AereAero = AereAero,
  TaxaMortalidade = TaxaMortalidade 
)

na_por_coluna <- lapply(dfs, function(df) {
  na_counts <- colSums(is.na(df))
  na_counts[na_counts > 0]
})
na_somente <- Filter(function(x) length(x) > 0, na_por_coluna)

na_matriz <- sapply(dfs, function(df) colSums(is.na(df)))
na_matriz

## Transformando os NA em 0 (Aprincipio por conta da indepência e sem relação para usar a média ou mediana)

tur[is.na(tur)] <- 0
grav[is.na(grav)] <- 0
cap[is.na(cap)] <- 0
sane[is.na(sane)] <- 0
fx_etaria[is.na(fx_etaria)] <- 0
AgenciasBancarias[is.na(AgenciasBancarias)]<-0 
Consumo[is.na(Consumo)] <- 0 
ContagemPopulacao[is.na(ContagemPopulacao)] <-0
EstabelecimentosSetores[is.na(EstabelecimentosSetores)] <- 0 
AereAero [is.na(AereAero)]<-0
## Rodando novamente a função pra verificar os NA

### Vamos agora unir as variáveis em df só

dados <- merge(exp, PIB, by = "Município.Estado")
dados <- merge(dados, pop, by = "Município.Estado")
dados <- merge(dados, alf, by = "Município.Estado")
dados <- merge(dados, nat, by = "Município.Estado")
dados <- merge(dados, grav, by = "Município.Estado")
dados <- merge(dados, densi, by = "Município.Estado")
dados <- merge(dados, tur, by = "Município.Estado")
dados <- merge(dados, urb, by = "Município.Estado")
dados <- merge(dados, ensino, by = "Município.Estado")
dados <- merge(dados, sane, by = "Município.Estado")
dados <- merge(dados, fecun, by = "Município.Estado")
dados <- merge(dados, prof, by = "Município.Estado")
dados <- merge(dados, fx_etaria, by = "Município.Estado")
dados <- merge(dados, AgenciasBancarias, by= "Município.Estado")
dados <- merge(dados, Consumo, by= "Município.Estado")
dados <- merge(dados, ContagemPopulacao, by= "Município.Estado")
dados <- merge(dados, EstabelecimentoSaude, by= "Município.Estado")
dados <- merge(dados, EstabelecimentosSetores, by= "Município.Estado")
dados <- merge(dados, AereAero, by= "Município.Estado")
dados <- merge(dados, TaxaMortalidade, by= "Município.Estado")
dados <- merge(dados, cap, by = "Município.Estado")

### Criou variaveis - Gustavo
dados$tamanho <- dados$populacao24 / dados$densi_demo24
dados$densidade_saneamento <- dados$un_san23 / dados$tamanho
dados$unid_ensino_por_hab <- dados$qtd_ensino24 / dados$populacao24
dados$densidade_ensino <- dados$qtd_ensino24 / dados$tamanho

dados$com_tur_km <- dados$Total_Turismo / dados$tamanho
dados$aln_por_prof <-  dados$Pop_5_18/ dados$qtd_prof24


### Vamos analisar as variaveis explicativas, com a correlação de pearson
library(dplyr)
library(corrplot)
## Vamos pegar apenas os dados numericos 
dados_num <- dados %>% select_if(is.numeric)


## Vamos fazer uma analise de correlação das variaveis
corr_matrix <- cor(dados_num, use = "pairwise.complete.obs")

corr_matrix <- round(corr_matrix,1)
corrplot(
  corr_matrix,
  method = "color",      # mapa de cores
  type   = "lower",      # só metade inferior (ou "upper" ou "full")
  order  = "hclust", 
  number.digits = 1, 
  tl.cex = 0.8,          # tamanho do texto dos rótulos
  addCoef.col = "black"  # opcional: adiciona os valores numéricos no quadrado
)

### Com base no gráfico de correlação conseguimos analisar quais são as maiores relações com a nossa target (Variavel Resposta)


## Criar um fluxo que analisa correlação das variaveis explicativas a mais de 60% e verificar ambas com a target para ver quem fica ou sai da escolha da variavel

variavel_comparativa <- c()
variavel_comparada <- c()
var_combinada <- c()
valores_correlacao <- c()
df1 <- data.frame(corr_matrix)

## 41 - Com as mais - 29 com anteriores
for (i in 1:157){
  dados_comparativos <- df1[i]
  validacao <- rownames(dados_comparativos)[dados_comparativos[,1]>0.6]
  valores <- dados_comparativos[validacao,1]
  x <- rep(dimnames(dados[i+1])[[2]],  length(validacao))
  pares <- paste(validacao, x, sep='_')
  var_combinada <- c(var_combinada, pares)
  variavel_comparativa <- c(variavel_comparativa, validacao)
  variavel_comparada <- c(variavel_comparada,x)
  valores_correlacao <- c(valores_correlacao, valores)
}

result <- data.frame(var_comparativa = variavel_comparativa,
                      var_comparada = variavel_comparada,
                      valores_correlacao = valores_correlacao,
                      # var_combinada = var_combinada,
                      stringsAsFactors = FALSE)



## vamos validar agora com a target
result$valor_var_comparativa <- df1$Renda_M_10[
  match(result$var_comparativa, rownames(df1))
]

### Aqui temos as melhores variaveis comparando com a target, ai no caso temos casos que são o mesmo valor de correlação
result_max <- result %>%
  group_by(var_comparada) %>%
  filter(valor_var_comparativa == max(valor_var_comparativa, na.rm = TRUE)) %>%
  ungroup()

### Aqui temos as melhores variaveis comparando com a target, ai no caso ele pega apenas uma
result_max_1 <- result %>%
group_by(var_comparada) %>%
slice_max(order_by = valor_var_comparativa, n = 1, with_ties = FALSE) %>%
ungroup()


########### GRAFICO DE ANÁLISE DAS VARIAVEIS COM A TARGET

variaveis_explicativas <- unique(result_max_1$var_comparada)

par(mfrow = c(4, 3))

for (var in variaveis_explicativas) {
  if (is.numeric(dados[[var]])) {
    plot(dados[[var]], dados$Renda_M_10,
         xlab = var, ylab = "IDHM", pch = 1, col = "black",
         main = paste("Renda_M_10 x", var))
    abline(lm(Renda_M_10 ~ dados[[var]], data = dados), col = "red")
  } else {
    cat("Variável ignorada (não numérica):", var, "\n")
  }
}

###

# Abrir um PDF para salvar os gráficos
pdf("graficos_renda_vs_variaveis_anteriores.pdf", width = 10, height = 8)

# Definir layout: 3 colunas x 3 linhas (ajuste se quiser)
par(mfrow = c(3, 3))

contador <- 0

for (var in variaveis_explicativas) {
  if (is.numeric(dados[[var]])) {
    plot(dados[[var]], dados$Renda_M_10,
         xlab = var, ylab = "Renda_M_10", pch = 1, col = "black",
         main = paste("Renda_M_10 x", var))
    abline(lm(Renda_M_10 ~ dados[[var]], data = dados), col = "red")
    
    contador <- contador + 1
    if (contador %% 9 == 0) {
      par(mfrow = c(3, 3))  # reinicia layout a cada 9 gráficos
    }
  } else {
    cat("Variável ignorada (não numérica):", var, "\n")
  }
}

# Fecha o arquivo PDF
dev.off()


#### COM TODAS
# Seleciona variáveis numéricas, excluindo as identificadoras
variaveis_numericas <- names(dados)[
  sapply(dados, is.numeric) & !(names(dados) %in% c("Renda_M_10", "Município.Estado"))
]

# Abrir PDF
pdf("graficos_renda_completos.pdf", width = 10, height = 8)

# Contador de gráficos por página
contador <- 0

# Inicializa layout da 1ª página
par(mfrow = c(3, 3))

for (var in variaveis_numericas) {
  x <- dados[[var]]
  y <- dados$Renda_M_10
  
  # Checar se a variável é válida para plotagem
  if (all(is.na(x)) || sd(x, na.rm = TRUE) == 0) {
    cat("Variável ignorada (NA ou constante):", var, "\n")
    next
  }
  
  # Plotagem segura
  try({
    plot(x, y,
         xlab = var, ylab = "Renda_M_10",
         pch = 1, col = "black",
         main = paste("Renda_M_10 x", var))
    abline(lm(y ~ x), col = "red")
  }, silent = TRUE)
  
  contador <- contador + 1
  
  # Nova página a cada 9 gráficos
  if (contador %% 9 == 0) {
    par(mfrow = c(3, 3))
  }
}

dev.off()
