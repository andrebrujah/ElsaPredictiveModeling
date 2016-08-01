# No.PubliELSA/Requisição: 	14_0173
# Titulo da Proposta:       Comparison of data mining techniques for prediction of undiagnosed diabetes
# Finalidade do programa:   Etapa de Afinação de parâmetros: Realização da validação cruzada de k=10 folds 
#                           repetida por n=3 vezes com o objetivo de estimar os melhores parâmetros para cada algoritmo.
# Nº do programa:           analise02.R
# Autor:                    André Rodrigues Olivera
# Centro:                   ELSA-Brasil
# Data:                     01/06/2016
# Comentários:              Antes desta etapa, os dados já foram importados, particionados e pré-processados. Os parâmetros 
#                           de cada algoritmo são definidos manualmente através de um método que é sobrescrito para cada algoritmo 
#                           carregado através da execução do script "R/reload.perform.tests.for.tunning.R".

rm(list=ls());
source('R/load.libraries.R');
source('R/load.perform.tests.functions.R');
# script abaixo recarrega as funcoes que testam diversos parametros
source('R/reload.perform.tests.for.tunning.R');


transformation.list <- list(c("factor"), c());

imputation.method.list <- list(c());
test.cases.remove <- c();

load(file="output/subset_orig");
subset.idx <- 1;
subsets.list <- list();
subsets.list[[1]] <- subset_orig;

# 1: algorithms <- c("glm", "nb", "knn", "rf", "nnet"); 
# 2: algorithms <- c("nb", "knn", "rf", "nnet"); 
# 3: algorithms <- c("nb", "knn", "rf", "nnet"); 
# 4: algorithms <- c("knn", "rf"); 
# 5: algorithms <- c("rf"); 

algorithms <- c("glm", "nb", "knn", "rf", "nnet"); 
algorithms <- c("knn"); 

is.tuning <- TRUE;

n <- 3;
k <- 10;
for (transformation in transformation.list) {
  for (imputation.method in imputation.method.list) {
    tst.id = "cv-paramtun-rep-";
    if (length(transformation) == 0) {
      tst.id = paste(tst.id, "semtransf-", sep = ""); 
    } else {
      tst.id = paste(tst.id, transformation, sep = "");
      tst.id = paste(tst.id, "-", sep = "");
    }
    
    if (length(imputation.method) == 0) {
      tst.id = paste(tst.id, "semimp-", sep = "");
    } else {
      tst.id = paste(tst.id, "comimp-", sep = "");
    }
    
    ds.id <- paste("tuning.ds");
    tst.id = paste(tst.id, ds.id, sep = "");
    tst.id = paste(tst.id, subset.idx, sep = ".");
    perform.test (ds.id = ds.id, tst.id = tst.id, subset = subsets.list[[subset.idx]], 
                  transformation = transformation,  imputation.method = imputation.method, 
                  training.only.complete = TRUE, testing.only.complete = TRUE, 
                  test.cases.remove = c(test.cases.remove),
                  n = n, k = k, seed = 666, algorithms = algorithms);
  }
}
