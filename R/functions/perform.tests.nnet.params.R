############################################################################
#################### perform.tests.nnet.params.R ###########################
#
# Algoritmo: Redes Neurais
#
# Função que sobrescreve perform.tests.xxx utilizada na validação cruzada 
# para definir os parâmetros que serão testados na etapa de afinação de parâmetros
# Os parâmetros são definidos pelo analista diretamente no código fonte. 
#
############################################################################

perform.tests.nnet <- function(n,k, ds.id, subset = c(), transformation = c(), imputation.method = c(), training.only.complete = FALSE, testing.only.complete = FALSE, test.cases.remove = c(), tst.id, verbose = TRUE)
{
  # 1:
  # size.vector <- c(0, 1, 2, 5, 10, 25, 50, 75, 100);
  # decay.vector <- c(0, 1, 2, 5, 10);
  # skip.vector <- c(TRUE, FALSE); 
  
  # 2: 
  #if (length(grep("factor", tst.id)) > 0) {
  #  size.vector <- c(90, 100, 110, 150);
  #  decay.vector <- c(3, 4, 6, 7);
  #  skip.vector <- c(TRUE);
  #} else {
  #  size.vector <- c(90, 100, 110, 150);
  #  decay.vector <- c(2, 3, 4);
  #  skip.vector <- c(FALSE);
  #}
  
  
  if (length(grep("factor", tst.id)) > 0) {
    # size = 100; decay = 3; skip = TRUE;
    size.vector <- c(100);
    decay.vector <- c(3);
    skip.vector <- c(TRUE);
    cutoffs <- c(0.11);
  } else {
    # size = 175; decay = 2; skip = FALSE;
    size.vector <- c(175);
    decay.vector <- c(2);
    skip.vector <- c(FALSE);
    cutoffs <- c(0.12);
  }
  
  
  # constroi lista combinando os parametros que variam
  # dai so precisa dessa lista para testar 
  param.df <- data.frame(size = numeric(0), decay = numeric(0), skip = logical());
  param.index <- 0; 
  for (size in size.vector)
  {
    for (decay in decay.vector)
    {
      for (skip in skip.vector)
      {
        if (size != 0 || skip) # size == 0 && skip == FALSE  nao pode passar
        {
          param.index <- param.index + 1;
          param.df[param.index,] <- c(size, decay, skip);
        }        
      }
    }    
  }
  # como acessar os parametros
  #cutoffs <- c(seq(0.07,0.15,0.01));
  set.seed(666);
  
  ## ESPECIFICO DO ALGORITMO - chamada funcao
  number.folds <- n * k;
  result <- perform.cv.nnet(n=n, k=k, ds.id = ds.id, subset = subset, transformation = transformation, imputation.method = imputation.method, training.only.complete = training.only.complete, testing.only.complete = testing.only.complete, test.cases.remove = test.cases.remove,  cutoffs = cutoffs, param.df = param.df, verbose = verbose);
  output.cv.results.nnet(result = result, cutoffs = cutoffs, param.df = param.df, number.folds = number.folds, root.path = "output/results/nnet", tst.id = tst.id, verbose = verbose); 
}
