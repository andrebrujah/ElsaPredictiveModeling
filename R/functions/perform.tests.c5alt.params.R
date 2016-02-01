############################################################################
################### perform.tests.c5alt.params.R ###########################
#
# Algoritmo: C5 (árvore de decisão)
#
# Função que sobrescreve perform.tests.xxx utilizada na validação cruzada 
# para definir os parâmetros que serão testados na etapa de afinação de parâmetros
# Os parâmetros são definidos pelo analista diretamente no código fonte. 
#
############################################################################
perform.tests.c5alt <- function(n, k, ds.id, subset = c(), transformation = c(), imputation.method = c(), training.only.complete = FALSE, testing.only.complete = FALSE, test.cases.remove = c(), tst.id, verbose = TRUE)
{
  # parametros para comparar e descobrir o melhor atraves de validacao cruzada  
# costs = 9; minCases = 1; fuzzyThreshold = 1; earlyStopping = 1; winnow = 1; noGlobalPruning = 0
  noGlobalPruning.vector <- c(FALSE);
  fuzzyThreshold.vector <- c(TRUE);
  earlyStopping.vector <- c(TRUE);
  winnow.vector <- c(FALSE);
  minCases.vector <- c(150, 200, 230, 250, 300);
  costs.vector <-c(9, 10);
  # constroi lista combinando os parametros que variam
  # dai so precisa dessa lista para testar 
  param.df <- data.frame(costs = numeric(0), minCases = numeric(0), fuzzyThreshold = logical(), earlyStopping = logical(), winnow = logical(), noGlobalPruning = logical());
  param.index <- 0; 
  for (costs in costs.vector)
  {
    for (minCases in minCases.vector)
    {
      for (fuzzyThreshold in fuzzyThreshold.vector)
      {
        for (earlyStopping in earlyStopping.vector)
        {
          for (winnow in winnow.vector)
          {
            for (noGlobalPruning in noGlobalPruning.vector)
            {
              param.index <- param.index + 1;
              param.df[param.index,] <- c(costs, minCases, fuzzyThreshold, earlyStopping, winnow, noGlobalPruning);
            }
          }
        }
      }    
    }
  }
  cutoffs <- c(seq(0.07,0.15,0.01));
  set.seed(666);
  
  number.folds <- n * k;
  result <- perform.cv.c5alt(n = n, k = k, ds.id = ds.id, subset = subset, transformation = transformation, imputation.method = imputation.method, training.only.complete = training.only.complete, testing.only.complete = testing.only.complete, test.cases.remove = test.cases.remove,  cutoffs = cutoffs, param.df = param.df, verbose = verbose);
  output.cv.results.c5alt(result = result, cutoffs = cutoffs, param.df = param.df, number.folds = number.folds, root.path = "output/results/c5ALT", tst.id = tst.id, verbose = verbose); 
}
