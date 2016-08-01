# No.PubliELSA/Requisição: 	14_0173
# Titulo da Proposta:       Comparison of data mining techniques for prediction of undiagnosed diabetes
# Finalidade do programa:   Importação, seleção e particionamento dos dados para serem utilizados na validação cruzada.
# Nº do programa:           analise01.R
# Autor:                    André Rodrigues Olivera
# Centro:                   ELSA-Brasil
# Data:                     01/06/2016
# Comentários:              Primeiro passo da análise. O atual script depende de alguns dados de entrada como 
#                           os dados do ELSA em CSV e a lista de variáveis utilizadas. Este script não gera nenhum documento.

rm(list=ls());
source('R/load.libraries.R');
source('R/load.perform.tests.functions.R');
source('R/functions/generate.datasource.cv.functions.R');

### importa dados
dataset.orig <- import.data(foldertoremove="data/var/vars_to_remove/");
dataset.orig$a_dm <- factor(dataset.orig$a_dm, levels = c("1", "0")); # Positive class = '0'

### seleciona manualmente variavels
dataset.orig <- subset.var.from.file(dataset.orig, var.filename = "data/AnaliseVariaveis/fatores_literatura/subconjuntos/sem_laboratoriais.txt");

set.seed(666);
cv.partition  <- createDataPartition(dataset.orig$a_dm, p = 0.70, list = FALSE);
dataset.cv <- dataset.orig[cv.partition, ];
test.cases.remove <- which(!complete.cases(dataset.cv));

transformation.list <- list(c(), c("factor"));
imputation.method.list <- list(c());

idx_class <- which(names(dataset.orig) == "a_dm");
subset_orig <- names(dataset.orig[,-idx_class]);
save(subset_orig, file = "output/subset_orig");

# para comparar removendo os casos incompletos que existem no dataset completo
k <- 10;
for (n in c(3,10)) {
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
}