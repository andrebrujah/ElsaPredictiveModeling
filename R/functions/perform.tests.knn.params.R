############################################################################
################# perform.tests.knn.params.R ########################
#
# Algoritmo: K-Nearest Neighbours
#
# Função que sobrescreve perform.tests.xxx utilizada na validação cruzada 
# para definir os parâmetros que serão testados na etapa de afinação de parâmetros
# Os parâmetros são definidos pelo analista diretamente no código fonte. 
#
############################################################################

perform.tests.knn <- function(n,k, ds.id, subset = c(), transformation = c(), imputation.method = c(), training.only.complete = FALSE, testing.only.complete = FALSE, test.cases.remove = c(), tst.id, verbose = TRUE)
{
  # parametros para comparar e descobrir o melhor atraves de validacao cruzada  
  # parametros que variam
  neighbor.vector <- c(seq(475, 575, 5));
  min.votes.vector <- c(1);
  # constroi lista combinando os parametros que variam
  # dai so precisa dessa lista para testar 
  param.df <- data.frame(neighbor = numeric(0), min.votes = numeric(0));
  param.index <- 0; 
  for (neighbor in neighbor.vector)
  {
    for (min.votes in min.votes.vector)
    {
      if(min.votes < neighbor)
      {        
        param.index <- param.index + 1;
        param.df[param.index,] <- c(neighbor, min.votes);
      }
    }   
  }
  # como acessar os parametros
  cutoffs <- c(seq(0.07,0.15,0.01));
  set.seed(666);
  
  number.folds <- n * k;
  result <- perform.cv.knn(n=n, k=k, ds.id = ds.id, subset = subset, transformation = transformation, imputation.method = imputation.method, training.only.complete = training.only.complete, testing.only.complete = testing.only.complete, test.cases.remove = test.cases.remove,  cutoffs = cutoffs, param.df = param.df, verbose = verbose);  
  output.cv.results.knn(result = result, cutoffs = cutoffs, param.df = param.df, number.folds = number.folds, root.path = "output/results/knn", tst.id = tst.id, verbose = verbose); 

}
