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
  ## ESPECIFICO DO ALGORITMO - preparacaodo param.df
  #1: ntree.vector <- c(5, 50, 100, 250, 500, 750, 1000);
  #2: ntree.vector <- c(900, 1000, 1100, 1200, 1500);
  #3: ntree.vector <- c(1150, 1250, 1450, 1550, 1600, 1700, 1800, 1900, 2000);
  #4: ntree.vector <- c(1950, 2050, 2100, 2250, 2500);
  #5: ntree.vector <- c(2400, 2600, 2700, 2800, 2900, 3000);
  #6: ntree.vector <- c(2850, 2950, 3100, 3500);
  #7: ntree.vector <- c(3400, 3600, 3800, 4000);
  #8: ntree.vector <- c(3700, 3900, 4250, 4500, 4750, 5000);
  #9: ntree.vector <- c(4150, 4250, 4350, 4900, 5100, 5500, 6000);
     #ntree.vector <- c(4200, 4300, 5750, 6250, 6500, 6750, 7000);
  if (length(grep("factor", tst.id)) > 0) {
    ntree.vector <- c(7000);
    cutoffs <- c(0.12);
  } else {
    ntree.vector <- c(4300);
    cutoffs <- c(0.13);
  }

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
  #cutoffs <- c(seq(0.07,0.15,0.01));
  
  ## ESPECIFICO DO ALGORITMO - chamada funcao
  number.folds <- n * k;
  result <- perform.cv.rf(n=n, k=k, ds.id = ds.id, subset = subset, transformation = transformation, imputation.method = imputation.method, training.only.complete = training.only.complete, testing.only.complete = testing.only.complete, test.cases.remove = test.cases.remove,  cutoffs = cutoffs, param.df = param.df, verbose = verbose);
  output.cv.results.rf(result = result, cutoffs = cutoffs, param.df = param.df, number.folds = number.folds, root.path = "output/results/rf", tst.id = tst.id, verbose = verbose); 
}
