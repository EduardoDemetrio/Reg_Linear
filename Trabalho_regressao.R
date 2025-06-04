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
PIB <- read.csv("Trabalho_1/PIB per Capita_Tabela.csv",  sep = "\t", fileEncoding = "UTF-8", skip = 1,  colClasses = "character")
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


## Vamos formatar as variaveis em numeric

exp$Renda_M_10 <- as.numeric(gsub(",", ".", gsub("\\.", "", exp$X2010)))
PIB$PIB21 <- as.numeric(gsub("\\.", "", PIB$X2021))
pop$populacao24 <- as.numeric(gsub("\\.", "", pop$X2024))
nat$natalidade23 <- as.numeric(gsub(",", ".",  nat$X2023))
alf$'Município.Estado' <- alf$X
alf <- alf[-c(1,2), ]
cols <- setdiff(names(alf), "Município.Estado")
alf[cols] <- lapply(alf[cols], function(x) as.numeric(gsub(",", ".", x)))
grav$grav_15a17 <- as.numeric(gsub(",", ".",  grav$grav_15a17))
grav$Município.Estado <- grav$X 
densi$densi_demo24 <-  as.numeric(gsub(",", ".", gsub("\\.", "", densi$X2024)))
tur$Total <-  as.numeric(tur$Total) ; tur$Município.Estado <-  tur$X ; tur$Alimentação <-  as.numeric(tur$Alimentação)
urb$pct_urb22 <-  as.numeric(gsub(",", ".", gsub("\\.", "", urb$X2022)))
cap$Município.Estado <- cap$Município
cap$dist_cap21 <-  as.numeric(gsub(",", ".", gsub("\\.", "", cap$X2021)))
ensino$Município.Estado <- ensino$X 
ensino$qtd_ensino24 <-  as.numeric(gsub(",", ".", gsub("\\.", "", ensino$Total)))
sane$Município.Estado <- sane$X
sane$un_san23 <-  as.numeric(gsub(",", ".", gsub("\\.", "", sane$Unidades.Atendidas...Total)))
fecun$taxa_fecun10 <-  as.numeric(gsub(",", ".", gsub("\\.", "", fecun$X2010)))
prof$Município.Estado <- prof$X 
prof$qtd_prof24 <-  as.numeric(gsub(",", ".", gsub("\\.", "", prof$Total))) 
cols <- c("X5.anos", "X6.anos", "X7.anos", "X8.anos", "X9.anos", 
          "X10.anos", "X11.anos", "X12.anos", "X13.anos", "X14.anos",
          "X15.anos", "X16.anos", "X17.anos", "X18.anos")


fx_etaria[cols] <- lapply(fx_etaria[cols], function(x) as.numeric(gsub("\\.", "", x)))
fx_etaria$Pop_5_18 <- rowSums(fx_etaria[cols], na.rm = TRUE)

fx_etaria$Município.Estado <- fx_etaria$X


## Removendo colunas e linhas
exp$X2010 <- NULL ; exp$Região.a.que.Pertence <- NULL ; exp <- exp[-1, ] 
PIB$Região.a.que.Pertence <- NULL ; PIB$X2021 <- NULL
pop$Região.a.que.Pertence <- NULL ; pop$X2024 <- NULL
alf$X <- NULL ; alf$X.1 <- NULL
nat$X2023 <- NULL ; nat$Região.a.que.Pertence <- NULL ; nat <- nat[-1, ]
grav$X <- NULL ; grav$X.1 <- NULL ; grav <- grav[-1, ]
densi$X2024 <- NULL ; densi$Região.a.que.Pertence <- NULL ;densi <- densi[-1, ]
tur$X <- NULL ; tur$X.1 <- NULL ; tur <- tur[-c(1,2), ]
urb$Região.a.que.Pertence <-  NULL ; urb$X2022 <- NULL
cap$Município <- NULL ; cap$Região.a.que.Pertence <- NULL ; cap$X2021 <-  NULL
ensino$X <- NULL ; ensino$X.1 <- NULL ; ensino$Total <-  NULL
sane$X.1 <- NULL ;sane$X <- NULL ; sane$Unidades.Atendidas...Total <-  NULL
prof$X <- NULL ; prof$X.1 <- NULL  ; prof$Total <-  NULL
fecun$X2010 <-  NULL ; fecun$Região.a.que.Pertence <- NULL

fx_etaria$X <- NULL ; fx_etaria$X.1 <- NULL


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
  fx_etaria = fx_etaria
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
sane[is.na(sane)] <-0

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
dados <- merge(dados, cap, by = "Município.Estado")
dados <- merge(dados, ensino, by = "Município.Estado")
dados <- merge(dados, sane, by = "Município.Estado")
dados <- merge(dados, fecun, by = "Município.Estado")
dados <- merge(dados, prof, by = "Município.Estado")
dados <- merge(dados, fx_etaria[, c("Município.Estado", "Pop_5_18")],
               by = "Município.Estado", all.x = TRUE)

### Criou variaveis - Gustavo
dados$aln_por_prof <-  dados$Pop_5_18/ dados$qtd_prof24
dados$tamanho <- dados$populacao24 / dados$densi_demo24
dados$com_tur_km <- dados$Total / dados$tamanho
dados$densidade_saneamento <- dados$un_san23 / dados$tamanho
dados$unid_ensino_por_hab <- dados$qtd_ensino24 / dados$populacao24
dados$densidade_ensino <- dados$qtd_ensino24 / dados$tamanho



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

for (i in 1:29){
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