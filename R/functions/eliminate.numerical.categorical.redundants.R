############################################################################
######### functions/eliminate.numerical.categorical.redundants.R ###########
#
# funções auxiliares, utilizadas pelas funções de pré-processamento para 
# remover variáveis redundantes antes de fazer uma discretização ou binarização. 
# As variáveis redundantes são definidas pelo usuário em dois arquivos, 
# um para variáveis categóricas e outro para numéricas relacionados pelo 
# número da linha. Por exemplo, a linha X do arquivo de variáveis numérica
# contém a variável "a_imc1" (Índice de Massa Corporal) e a mesma linha X, 
# no arquivo de variáveis categóricas, contém a variável "a_imc2", que é  
# o IMC discretizado e portanto pode ser considerado redundante.
# 
#
############################################################################

# categoric.names e numeric.names sao vetores (de mesmo tamanho) 
# representando mapeamento de variaveis (tem o mesmo significado)

# indice das variáveis numéricas cujas correspondentes categóricas existam no df
numerical.redundant.index <- function(df.names, categoric.names.file = "data/AnaliseVariaveis/var_categoricas_mod.txt", numeric.names.file = "data/AnaliseVariaveis/var_numericas_mod.txt")
{
  categoric.names <- read.table(categoric.names.file);
  categoric.names <- categoric.names$V1;
  categoric.names <- tolower(categoric.names);
  numeric.names <- read.table(numeric.names.file);
  numeric.names <- numeric.names$V1;
  numeric.names <- tolower(numeric.names);
  
  # seleciona as categóricas que estao no df atualmente
  index.categoric.in.df <- which(categoric.names %in% df.names);
  index.to.remove <- c();
  if (length(index.categoric.in.df) > 0) 
  {
    # seleciona variaveis do df que estao na lista de numericas que contem uma 
    # correspondente categorica que esta no df (selecionada anteriormente)
    index.to.remove <- which(df.names %in% numeric.names[index.categoric.in.df]);
  }
  return(index.to.remove);  
}

# indice das variáveis categóricas cujas correspondentes numericas existam no df
categorical.redundant.index <- function(df.names, categoric.names.file = "data/AnaliseVariaveis/var_categoricas_mod.txt", numeric.names.file = "data/AnaliseVariaveis/var_numericas_mod.txt")
{
  categoric.names <- read.table(categoric.names.file);
  categoric.names <- categoric.names$V1;
  categoric.names <- tolower(categoric.names);
  numeric.names <- read.table(numeric.names.file);
  numeric.names <- numeric.names$V1;
  numeric.names <- tolower(numeric.names);
  
  index.numeric.in.df <- which(numeric.names %in% df.names);
  index.to.remove <- c();
  if (length(index.numeric.in.df) > 0) 
  {
    index.to.remove <- which(df.names %in% categoric.names[index.numeric.in.df]);
  }
  return(index.to.remove);  
}