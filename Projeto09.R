# DSA - DATA SCIENCE ACADEMY 
# FORMACAO CIENTISTA DE DADOS
# BUSINESS ANALYTICS
#
# PROJETO COM FEEDBACK 9, Prevendo Tendências Macroeconômicas 
# ALUNO: EDUARDO FRIGINI DE JESUS 
# 
# Goal: Predizer y
# Este conjunto de dados contém recursos anônimos relacionados a um valor
# variável no tempo para um instrumento financeiro. Cada instrumento tem um id.
# O tempo é representado pelo recurso 'timestamp' e a variável a predizer é 'y'.
# Nenhuma informação adicional será fornecida sobre o significado dos recursos, as
# transformações que foram aplicadas a eles, a escala de tempo ou o tipo de
# instrumentos que estão incluídos nos dados. Além disso, de acordo com as regras,
# os alunos não devem usar dados além dos dados vinculados ao site.



# Instalando a biblioteca para leitura do arquivo .h5
# RODAR APENAS A PRIMEIRA VEZ, DEPOIS DE INSTALADO NAO PRECISA MAIS
rm(list = ls())
download.file("http://bioconductor.org/biocLite.R", destfile = "biocLite.R")
source("biocLite.R")
biocLite("rhdf5")


# Carregando a bibliotecas
library(rhdf5)
suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(library(psych))
suppressMessages(library(randomForest))
suppressMessages(library(lattice))
suppressMessages(library(corrplot))
suppressMessages(library(corrgram))


# Definindo o diretorio de trabalho
setwd("D:/FCD/Business_Analytics/Cap09/Projeto9")
getwd()

# Carregando o arquivo de treino
df = h5read('train.h5','train')

# Tratando e visualizando o data frame
colnames = df[['axis0']]
View(colnames)

df = cbind(t(df[['block0_values']]),t(df[['block1_values']]))
df = as.data.frame(df)
colnames(df) = colnames
View(df)
str(df)
class(df)
length(df$id)

# Tratando dados NA. Resolvi preencher todos NA por 0, não sei se foi a melhor escolha
df[is.na(df)]= 0
df=as.data.table(df)
str(df)

## Convertendo a variavel ID  para o tipo fator (categórica)
to.factors <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.factor(df[[variable]])
  }
  return(df)
}
dados <- to.factors(df = df, variables = 'id')
str(dados)
# sao 1424 id's diferentes


# VOU SELECIONAR O ID = 10
id = 10

# Extrai o nome das colunas no df original e coloca num data frame vazio df_sel (data frame selecionado)
df_sel = as.data.frame(df[0,])

# Numero de linhas do data frame original
n_lin = length(df$id)
n_lin

# Cria um data frame com as linhas apenas do ID selecionado
i=0
for (l in seq(1, n_lin)) {
  if (df$id[l] == id) {
    df_sel[i,] <- df[l,]
    i = i+1
    print(i)
  }
}

cat("Sao ", i," observacoes (linhas) do ID: ", id)

View(df_sel)
str(df_sel)

# Visualizacao os dados graficamente
hist(df_sel$y)
plot(df_sel$y)



# Como sao muitas variaveis (colunas) resolvi automatizar o processo de Feature Selection
# Decidi escolher as variaveis que mais correlacionam com o TARGET
# atraves do p-value <= 0.05 e correlation > 0.07

# Alterar o valor de correl(correlacao) para reduzir ou aumentar o nº de variaveis preditoras
correl = 0.0005

# O valor normalmente utilizado para o pvalue é menor que 0.05
pvalue = 0.5

n_col = length(colnames)
alvo = df_sel$y
length(alvo)

# Inicializa o processo de Feature Selection
col_sel <- c()
num_sel <- c()
nom_sel <- c()
c=3
cx = 0   # numero de colunas selecionadas

# Loop para Feature Selection
for (c in seq(3, n_col-1)) {
  colx = df_sel[,c]
  soma = sum(as.numeric(colx), na.rm = TRUE)

  # Verifica se a coluna nao esta vazia
  if (soma>1){
    teste_h <-cor.test(colx, alvo,  method = "kendall")
    r_quadr = abs(as.numeric(teste_h[4]))
    p_valor = as.numeric(teste_h[3])
    
    if ((p_valor <= pvalue) & (r_quadr > correl)) {
      cx = cx + 1
      num_sel[cx] <- c
      nom_sel[cx] <- colnames[c]
      col_sel <- cbind(num_sel, nom_sel)

      print(cx)
      print(c)
      print(colnames[c])
      print(r_quadr)
      print(p_valor)
    }
  }
}

# Essas foram as variaveis (colunas) selecionadas
print(cx)
col_sel

nom_sel


# Vetor com os métodos de correlação
metodos <- c("pearson", "spearman")

# Aplicando os métodos de correlação com a função cor()
cors <- lapply(metodos, function(method) 
  (cor(df_sel[, nom_sel], method = method)))

head(cors)

# Preparando o plot
plot.cors <- function(x, labs){
  diag(x) <- 0.0 
  plot(levelplot(x, 
                  main = paste("Plot de Correlação usando Método", labs),
                  scales = list(x = list(rot = 90), cex = 1.0)) )
}

# Mapa de Correlação
Map(plot.cors, cors, metodos)


# Avalidando a importância de todas as variaveis
modelo_rf <- randomForest(y ~ . , 
                       data = df_sel, 
                       ntree = 100, 
                       nodesize = 10,
                       importance = TRUE)
# Plotando as variáveis por grau de importância
varImpPlot(modelo_rf)

# modelo com as variaveis selecionadas
modelo_1 <- lm(y ~  fundamental_8 + fundamental_19 + fundamental_20 + fundamental_28 +
                  fundamental_29 + fundamental_31 + fundamental_33 + fundamental_39 +
                  fundamental_40 + fundamental_46 + fundamental_49 + technical_35 +  
                  technical_36, data = df_sel)
summary(modelo)

# modelo com todas as variaveis
modelo_2 <- lm(y ~  ., data = df_sel)
summary(modelo)

# Prevendo y 
previsao <- predict(modelo)
class(previsao)
head(previsao)
plot(previsao, df_sel$y)
cor(previsao, df_sel$y) # Correlacao de 0.56




