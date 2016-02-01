############################################################################
##################### perform.tests.rf.params.R ############################
#
# Algoritmo: Random Forest
#
# Função que sobrescreve perform.tests.xxx utilizada na validação cruzada 
# para definir os parâmetros que serão testados na etapa de afinação de parâmetros
# Os parâmetros são definidos pelo analista diretamente no código fonte. 
#
############################################################################
perform.tests.rf <- function(n,k, ds.id, subset = c(), transformation = c(), imputation.method = c(), training.only.complete = FALSE, testing.only.complete = FALSE, test.cases.remove = c(), tst.id, verbose = TRUE)
{
  # parametros para comparar e descobrir o melhor atraves de validacao cruzada  
  # parametros que variam
  ntree.vector <- c(seq(1000, 1100, 10), 1300, 1500);
  # constroi lista combinando os parametros que variam
  # dai so precisa dessa lista para testar 
  param.df <- data.frame(ntree = numeric(0));
  param.index <- 0; 
  for (ntree in ntree.vector)
  {
    param.index <- param.index + 1;
    param.df[param.index,] <- c(ntree);
  }
  # como acessar os parametros
  cutoffs <- c(seq(0.07,0.15,0.01));
  
  ## ESPECIFICO DO ALGORITMO - chamada funcao
  number.folds <- n * k;
  result <- perform.cv.rf(n=n, k=k, ds.id = ds.id, subset = subset, transformation = transformation, imputation.method = imputation.method, training.only.complete = training.only.complete, testing.only.complete = testing.only.complete, test.cases.remove = test.cases.remove,  cutoffs = cutoffs, param.df = param.df, verbose = verbose);
  output.cv.results.rf(result = result, cutoffs = cutoffs, param.df = param.df, number.folds = number.folds, root.path = "output/results/rf", tst.id = tst.id, verbose = verbose); 
}
