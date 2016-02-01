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
  laplace.vector <- c( 0.015,  0.01, 0.005, 0.00001, 0.000001, 0);
    
  param.df <- data.frame(laplace = numeric(0));
  param.index <- 0; 
  for (laplace in laplace.vector)
  {
    param.index <- param.index + 1;
    param.df[param.index,] <- c(laplace);    
  }  
  
  cutoffs <- c(seq(0.01,0.2,0.01));
  set.seed(666);
  
  number.folds <- n *  k;
  result <- perform.cv.nb(n=n, k=k, ds.id = ds.id, subset = subset, transformation = transformation, imputation.method = imputation.method, training.only.complete = training.only.complete, testing.only.complete = testing.only.complete, test.cases.remove = test.cases.remove,  cutoffs = cutoffs, param.df = param.df, verbose = verbose);
  output.cv.results.nb(result = result, cutoffs = cutoffs, param.df = param.df, number.folds = number.folds, root.path = "output/results/nb", tst.id = tst.id, verbose = verbose); 
}
