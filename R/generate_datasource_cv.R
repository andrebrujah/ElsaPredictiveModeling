############################################################################
####################### generate_datasource_cv.R ###########################
#
# Script utizado para criar e salvar em disco os conjuntos de dados utilizados
# nas diferentes iterações das validações cruzadas. Cada conjunto de dados 
# é identificado pelo seu nome, que contém entre outras informações: 
# se ele é de treino ou teste, pre-processamento utilizado e identificador 
# do fold e da repetição.
# O parâmetro n é configurável e indica o número de repetições da validação
# cruzada. Foram usados n = 3 e n = 10.
#
############################################################################

rm(list=ls());
source('R/load.libraries.R');
source('R/load.perform.tests.functions.R');
source('R/functions/generate.datasource.cv.functions.R');

### importa dados
dataset.orig <- import.data(foldertoremove="data/var/vars_to_remove/");
dataset.orig$a_dm <- factor(dataset.orig$a_dm, levels = c("1", "0")); # Positive class = '0'

### seleciona manualmente variavels
dataset.orig <- subset.var.from.file(dataset.orig, var.filename = "data/AnaliseVariaveis/fatores_literatura/subconjuntos/sem_laboratoriais.txt");

# substitui dados faltantes por resposta negativa para as seguintes variáveis
na.columns <- c("a_chdlight", "a_chdhard", "a_prvdcc");
dataset.imp <- dataset.orig;
for (na.column in na.columns){
  dataset.imp[which(!complete.cases(dataset.orig[,na.column])), na.column] <- "0";
}
dataset.orig <- dataset.imp;

set.seed(666);
cv.partition  <- createDataPartition(dataset.orig$a_dm, p = 0.70, list = FALSE);
dataset.cv <- dataset.orig[cv.partition, ];
test.cases.remove <- which(!complete.cases(dataset.cv));


transformation.list <- list(c(), c("factor"), c("numeric"));

i <- length(subsets.list.unique); 

idx_class <- which(names(dataset.orig) == "a_dm");
subset_orig <- names(dataset.orig)[-idx_class];
save(subset_orig, file="output/subset_orig");

# n = 3 para validação cruzada repetida por 3 vezes e n = 10 para validação cruzada repetida por 10x (descomentar 
n <- 3;
# n <- 10;
k <- 10;
# cria soh para subset original
for (transf in transformation.list) {
  for (imput in imputation.method.list) {
    ds <- dataset.orig;
    ds.id <- paste("tuning.ds");
      gerar_datasets(dataset = ds, training.only.complete = TRUE,  
                     testing.only.complete = TRUE, ds.id = ds.id,
                     imputation.method = imput, test.cases.remove = c(test.cases.remove),
                     transformation = transf, n = n, k = k, seed=666, verbose = TRUE);
  }
}

