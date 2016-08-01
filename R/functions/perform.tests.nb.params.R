############################################################################
################# perform.tests.nb.params.R ########################
#
# Algoritmo: Naive Bayes
#
# Função que sobrescreve perform.tests.xxx utilizada na validação cruzada 
# para definir os parâmetros que serão testados na etapa de afinação de parâmetros
# Os parâmetros são definidos pelo analista diretamente no código fonte. 
#
############################################################################

perform.tests.nb <- function(n,k, ds.id, subset = c(), transformation = c(), imputation.method = c(), training.only.complete = FALSE, testing.only.complete = FALSE, test.cases.remove = c(), tst.id, verbose = TRUE)
{
  cat("\n PERFORM TESTS NB \n")
  #1: laplace.vector <- c(0, 0.00001, 0.001, 0.01, 0.1, 0.2, 0.5, 0.8);
  #2: laplace.vector <- c(0.001, 0.3, 0.4, 0.5, 0.6, 0.7);
  #laplace.vector <- c(0.8, 0.9);
  #laplace.vector <- c(1);

  if (length(grep("factor", tst.id)) > 0) {
    laplace.vector <- c(0.001);
    cutoffs <- c(0.09);
  } else {
    laplace.vector <- c(1);
    cutoffs <- c(0.31);
  }
  
  param.df <- data.frame(laplace = numeric(0));
  param.index <- 0; 
  for (laplace in laplace.vector)
  {
    param.index <- param.index + 1;
    param.df[param.index,] <- c(laplace);    
  }  
  
  
  set.seed(666);
  
  number.folds <- n *  k;
  result <- perform.cv.nb(n=n, k=k, ds.id = ds.id, subset = subset, transformation = transformation, imputation.method = imputation.method, training.only.complete = training.only.complete, testing.only.complete = testing.only.complete, test.cases.remove = test.cases.remove,  cutoffs = cutoffs, param.df = param.df, verbose = verbose);
  output.cv.results.nb(result = result, cutoffs = cutoffs, param.df = param.df, number.folds = number.folds, root.path = "output/results/nb", tst.id = tst.id, verbose = verbose); 
}
