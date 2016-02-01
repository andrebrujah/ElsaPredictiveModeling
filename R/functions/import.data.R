############################################################################
####################### functions/import.data.R ############################
# 
# Funções para importação dos dados. A definição de quais variáveis são 
# categóricas ou numéricas é feita através de arquivos externos contendo os
# nomes das variáveis. O nome do arquivo é passado como parâmetro.
# Também é feita uma limpeza no conjunto de dados. Definições de variáveis a 
# remover ou à selecionar, se for o caso, são feitas através de arquivos de 
# texto externos, também. 
#
############################################################################

## Importacao dos dados e selecao/limpeza baseados nos arquivos com nomes das variaveis
import.data <- function(dataset.file = "data/dataset.csv", save.output = FALSE, output.filename = "output/dataset", 
                        verbose = TRUE, remove.women.vars = TRUE,  foldertoremove = "data/var/vars_to_remove/", 
                        factorial.filename = "data/var/categoricas.txt", orderedfactor.filename = "data/var/categoricas_ordenadas.txt",
                        numeric.filename = "data/var/numericas.txt")
{
  missing.codes <- c(" ","", ".D", ".PULO", "P", "B", "S", "Q", "R", "F", "N", "C", "A", "T", "I", "L", "D", "V", "U", "E");
  
  dataset <- read.table(dataset.file, header=TRUE, sep = "$", strip.white = TRUE, 
                        comment.char = "", quote = "", dec=".", allowEscapes = FALSE, 
                        na.strings = missing.codes, fileEncoding = "windows-1252");
  
  # coloca nome das variaveis em minusculo para facilitar futuras buscas
  names(dataset) <- tolower(names(dataset));
  
  # remove casos que nao possuem informacao de classe
  rows_na <- which(is.na(dataset$a_dm));
  if (length(rows_na) > 0)
  {
    dataset <- dataset[-rows_na,];  
  } 
  
  # inverte valor 0 por 1 e viceversa
  # pois alguns algoritmos pressupoe que o valor positivo da classe alvo seja 0 (e originalmente é 1) 
  rows_zero <- which(dataset$a_dm == 0);
  dataset$a_dm[rows_zero] <- 1;
  dataset$a_dm[-rows_zero] <- 0;
    
  ### Tratamento variáveis mulher 
  if (remove.women.vars)
  {
    # seleciona variaveis com respeito a mulher (mula) 
    mula.vars <- grep("mula", names(dataset));    
    # a principio remove essas variaveis, depois pode-se pensar em fazer modelo separado para as mulheres e para os homens
    if (length(mula.vars) > 0)
    {
      dataset <- dataset[,-mula.vars]; 
    }      
  }
  else
  {
    # uma observação da variável mula5a (Menopausa natural) está preenchida incorretamente
    # Seria para preencher com 1 (sim) ou 0 (não), porém foi preenchida com a idade quando ocorreu o evento
    index.obs <- which(dataset$mula5a == 56);
    if (length(index.obs) > 0)
    {
      dataset$mula5a[index.obs] <- 1;  
    }
  }

  if (length(foldertoremove) > 0)
  {
    dataset <- remove_variables_in_folder(foldertoremove, dataset);    
  }
  
  # definir categoricas/numericas/texto
  ### CATEGORICAS
  factorial.nomes <- read.table(factorial.filename);
  factorial.indices <- which(names(dataset) %in% factorial.nomes$V1);
  
  # transforma variaveis que devem ser categoricas para factor
  for (ind in factorial.indices)
  {
    # substitui valores M por valores negativos. A principio suponho que o valor negativo para todas variáveis é 0
    rows.m <- which(dataset[,ind] == "M");
    if (length(rows.m) > 0)
    {
      dataset[,ind] <- as.character(dataset[,ind]);
      dataset[rows.m, ind] <- 0;
    }  
    dataset[,ind] <- as.factor(dataset[,ind]);
  }
  
  # ORDERED FACTOR
  if (file.exists(orderedfactor.filename))
  {
    orderedfactorial.nomes <- read.table(orderedfactor.filename);
    orderedfactorial.indices <- which(names(dataset) %in% orderedfactorial.nomes$V1);
    
    for (ind in orderedfactorial.indices)
    {    
      dataset[,ind] <- as.ordered(dataset[,ind]);
    }
  }
    
  ### NUMERICAS
  if (file.exists(numeric.filename))
  {
    numericas.nomes <- read.table(numeric.filename);
    numericas.indices <- which(names(dataset) %in% numericas.nomes$V1);
    for (ind in numericas.indices)
    {
      dataset[,ind] <- as.numeric(dataset[,ind]);
    }
  }  
  
  ## checar se existe alguma variavel nos dois conjuntos
  categoricas_em_numericas <- factorial.indices[which(factorial.indices %in% numericas.indices)];
  if (length(categoricas_em_numericas) > 0)
  {
    var_names <- names(dataset[,categoricas_em_numericas]);
    cat("\n\nERRO: Variaveis categoricas e numericas ao mesmo tempo: \n");
    print(var_names);
  }
  
  if (save.output)
  {
    save(dataset, file=output.filename);
  }
  
  return(dataset);
}

subset.var.from.file <- function(dataset, var.filename = "AnaliseVariaveis/fatores_literatura/subconjuntos/sem_laboratoriais.txt", verbose = TRUE)
{
  var.names <- read.table(var.filename);
  var.names <- tolower(var.names$V1);
  base.indices <- which(names(dataset) %in% var.names);
  
  var_not_found <- var.names[-which(var.names %in% names(dataset))];
  
  if(length(var_not_found) > 0 && verbose)
  {
    cat("\nAs seguintes variaveis nao foram encontradas: \n");
    print(var_not_found);
  }
  
  ind_class <- which(names(dataset) == "a_dm")
  
  if (length(base.indices) > 0)
  {
    dataset <- dataset[,c(base.indices, ind_class)];    
  }
  else
  {
    if (verbose) cat("\nConjunto de dados nao modificado pois nenhuma variavel foi encontrada\n");    
  }
  
  return(dataset)
}