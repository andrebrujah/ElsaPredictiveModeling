############################################################################
################### perform.tests.glm.params.R ###########################
#
# Algoritmo: Regressão Logística
#
# Função que sobrescreve perform.tests.xxx utilizada na validação cruzada 
# para definir os parâmetros que serão testados na etapa de afinação de parâmetros
# Os parâmetros são definidos pelo analista diretamente no código fonte. 
#
############################################################################

perform.tests.glm <- function(n,k, ds.id, subset = c(), transformation = c(), imputation.method = c(), training.only.complete = FALSE, testing.only.complete = FALSE, test.cases.remove = c(), tst.id, verbose = TRUE)
{
  # parametros para comparar e descobrir o melhor atraves de validacao cruzada  

  epsilon.vector <- c(0.001,0.005, 0.01, 0.015, 0.04);  
  maxit.vector <- c(5, 15);
  # constroi lista combinando os parametros que variam
  param.df <- data.frame(epsilon = numeric(0), maxit = numeric(0));
  param.index <- 0; 
  for (epsilon in epsilon.vector)
  {
    for (maxit in maxit.vector)
    {
      param.index <- param.index + 1;
      param.df[param.index,] <- c(epsilon, maxit);
    }
  }
  
  cutoffs <- c(seq(0.07,0.15,0.01));
  set.seed(666);
  
  number.folds <- n * k;
  result <- perform.cv.glm(n=n, k=k, ds.id = ds.id, subset = subset, transformation = transformation, imputation.method = imputation.method, training.only.complete = training.only.complete, testing.only.complete = testing.only.complete, test.cases.remove = test.cases.remove,  cutoffs = cutoffs, param.df = param.df, verbose = verbose);
  output.cv.results.glm(result = result, cutoffs = cutoffs, param.df = param.df, number.folds = number.folds, root.path = "output/results/glm", tst.id = tst.id, verbose = verbose); 
}
