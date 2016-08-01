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
  ## ESPECIFICO DO ALGORITMO - preparacaodo param.df
  #1:
  #neighbor.vector <- c(5, 10, 50, 100, 200, 300, 400, 450, 500);
  #min.votes.vector <- c(0,1);
  #2:
  #neighbor.vector <- c(150, 175, 200, 225, 250, 425, 450, 475);
  #min.votes.vector <- c(0);
  #3: 
  # neighbor.vector <- c(240, 260, 275, 470, 480, 490, 500, 525, 550, 600);
  #neighbor.vector <- c(280, 285, 290, 295, 475, 485);
  min.votes.vector <- c(0);
  
  if (length(grep("factor", tst.id)) > 0) {
    # neighbor = 275
    neighbor.vector <- c(275);
    cutoffs <- c(0.09);
  } else {
    neighbor.vector <- c(475);
    cutoffs <- c(0.1);
  }
  
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
  set.seed(666);
  
  ## ESPECIFICO DO ALGORITMO - chamada funcao
  number.folds <- n * k;
  result <- perform.cv.knn(n=n, k=k, ds.id = ds.id, subset = subset, transformation = transformation, imputation.method = imputation.method, training.only.complete = training.only.complete, testing.only.complete = testing.only.complete, test.cases.remove = test.cases.remove,  cutoffs = cutoffs, param.df = param.df, verbose = verbose);  
  output.cv.results.knn(result = result, cutoffs = cutoffs, param.df = param.df, number.folds = number.folds, root.path = "output/results/knn", tst.id = tst.id, verbose = verbose); 

}
