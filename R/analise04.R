# No.PubliELSA/Requisição: 	14_0173
# Titulo da Proposta:       Comparison of data mining techniques for prediction of undiagnosed diabetes
# Finalidade do programa:   Etapa de estimativa de erros: Realização de validação cruzada de k=10 folds repetida por 
#                           n=10 vezes utilizando os resultados obtidos das etapas anteriores 
#                           (melhores parametros e melhores subconjuntos de variáveis). 
# Nº do programa:           analise04.R
# Autor:                    André Rodrigues Olivera
# Centro:                   ELSA-Brasil
# Data:                     01/06/2016
# Comentários:              Antes desta etapa, os dados já foram importados, particionados e pré-processados. 
#                           O carregamento dos métodos utilizados é feito através do script 'R/load.perform.tests.functions.R'. 

rm(list=ls());
source('R/load.libraries.R');
source('R/load.perform.tests.functions.R');


transformation.list <- list(c("factor"), c());

imputation.method.list <- c();
test.cases.remove <- c();

subsets.list <- list();
#constroi subsets:
  load(file="output/feature_selection_artigo/fs.glm");
  fs.glm <- fs;
  subsets.list[[1]] <- fs.glm;
  
  load(file="output/feature_selection_artigo/fs.nb");
  fs.nb <- fs;
  subsets.list[[2]] <- fs.nb;
  
  load(file="output/feature_selection_artigo/fs.knn");
  fs.knn <- fs;
  subsets.list[[3]] <- fs.knn;
  
  load(file="output/feature_selection_artigo/fs.nnet");
  fs.nnet <- fs;
  subsets.list[[4]] <- fs.nnet;
  
  load(file="output/subset_orig");
  subsets.list[[5]] <- subset_orig;


if (exists("is.tuning")) {
  rm(is.tuning);
}

algorithms <- c("glm", "nb", "knn", "rf", "nnet"); 

n <- 10;
k <- 10;
for (transformation in transformation.list) {
  for (imputation.method in imputation.method.list) {
    for (subset.idx in 1:length(subsets.list)) {
      tst.id = "error-est-rep-";
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
}
