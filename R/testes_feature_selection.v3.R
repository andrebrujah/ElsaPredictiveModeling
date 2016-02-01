############################################################################
##################### testes_feature_selection.v3.R ########################
#
# Script utilizado para geração dos subconjuntos de variáveis para etapa 
# de seleção automática de variáveis.
#
############################################################################


rm(list=ls());
source('R/load.libraries.R');
source('R/load.perform.tests.functions.R');
source('R/functions/fs.evaluators.v3.R');

### importa dados
dataset.orig <- import.data(foldertoremove="data/var/vars_to_remove/");
dataset.orig$a_dm <- factor(dataset.orig$a_dm, levels = c("1", "0")); # Positive class = '0'
### seleciona manualmente variavels
dataset.orig <- subset.var.from.file(dataset.orig, var.filename = "data/AnaliseVariaveis/fatores_literatura/subconjuntos/sem_laboratoriais.txt");
index.class <- which(colnames(dataset.orig) == "a_dm");
varsubset.orig <- colnames(dataset.orig)[-index.class];

n <- 3;
k <- 10;
training.only.complete = TRUE;
testing.only.complete = TRUE;
ds.id <- "tuning.ds";

algorithms <- c("glm", "nnet", "knn", "nb");
for (algorithm in algorithms) {
  if (algorithm == "nb") {
    transformation <- "factor";
  } else {
    transformation <- c();
  }   
  # evaluator utiliza informações em memória como n, k, algoritmo e nome do conjunto de dados
  fs <- forward.search(varsubset.orig, evaluator);
  fname <- paste("output/feature_selection_v3/fs.", algorithm, sep = "");
  save(fs, file=fname);
}


