############################################################################
############################ perform.tests.R ###############################
#
# Script utilizado como ponto de partida para criação e avaliação dos  
# modelos preditivos das etapas de afinação de parâmetros (configurando 
# n = 3 e sem utilizar diferentes subconjuntos de variáveis) e estimativa de 
# erros (configurando n = 10 e com subconjuntos de variáveis)
#
############################################################################


rm(list=ls());
source('R/load.libraries.R');
source('R/load.perform.tests.functions.R');
# descomentar linha abaixo para afinação de parametros, ela sobrescreve a função que cria os parâmetros que serão testados
# source('R/reload.perform.tests.for.tunning.R');

transformation.list <- list(c("factor"), c("numeric"), c());
imputation.method <- c();


# carrega subsets:
load(file="output/feature_selection_v3/subsets.list"); 
algorithms <- c( "glm", "nnet", "knn", "nb", "rf", "nbalt", "c5alt", "c5rulesalt"); 

# n <- 3 # para parameter tuning, descomentar essa linha
n <- 10; # para error estimate, descomentar essa linha
k <- 10;

for (transformation in transformation.list) {
  for (subset.idx in 1:6) {
    tst.id = "cv-dtp-";
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
