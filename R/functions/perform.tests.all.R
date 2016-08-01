############################################################################
######################## perform.tests.all.R ###############################
#
# Funções utilizadas na validação cruzada responsável por chamar as funções
# que realizam a validação cruzada de cada algoritmo. O "all" indica que 
# todos algoritmos (passados os nomes em uma lista como argumento) serão 
# executados.
#
############################################################################

perform.test <- function(dataset = NULL, tst.id = "tst_padrao", ds.id = "mod", subset, transformation = c(), imputation.method = c(), training.only.complete = FALSE, testing.only.complete = FALSE, test.cases.remove = c(), n = 10, k = 10, seed = 666, algorithms = c("nb", "glm", "c5", "c5rules", "jrip", "rf", "nnet", "c5alt", "c5rulesalt", "jripalt", "knnalt"), perform.generalization = FALSE)
{
  # separa dados para validacao cruzada
  
  
  if (!perform.generalization) 
  {
    # realiza validacao cruzada dos algoritmos passados como parametro
    perform.cv.all(n = n, k = k, ds.id = ds.id, subset = subset, algorithms = algorithms, 
                   tst.id = tst.id, transformation = transformation, imputation.method = imputation.method, training.only.complete = training.only.complete, 
                   testing.only.complete = testing.only.complete, test.cases.remove = test.cases.remove,  verbose = TRUE);
  } 
  else 
  {
    set.seed(seed);
    cv.partition  <- createDataPartition(dataset$a_dm, p = 0.70, list = FALSE);
    dataset.cv <- dataset[cv.partition, ];
    dataset.testing <- dataset[-cv.partition, ];
    perform.generalization.all(dataset.training = dataset.cv, dataset.testing = dataset.testing, 
                               transformation = transformation, imputation.method = imputation.method, training.only.complete = training.only.complete,
                               testing.only.complete = testing.only.complete, test.cases.remove = test.cases.remove,  
                               algorithms = algorithms, tst.id = tst.id, verbose = TRUE);
  }
}

perform.generalization.all <- function(dataset.training, dataset.testing, transformation = c(), imputation.method = c(), training.only.complete = FALSE, testing.only.complete = FALSE, test.cases.remove = c(), algorithms, tst.id, verbose = TRUE) 
{
  args <- list(dataset.testing = dataset.testing, dataset.training = dataset.training, transformation = transformation, imputation.method = imputation.method, training.only.complete = training.only.complete, testing.only.complete = testing.only.complete, test.cases.remove = test.cases.remove,  tst.id = tst.id, verbose = verbose);
  capture.output({cat("\n\n",format(Sys.time(), "%a %b %d %X"),"\n%%%%%%%%%%%%%%%%%%%%%%% Inicio: ",tst.id,"\n");}, file="output/log_folder/log", append=TRUE);
  # chama as funcoes mudando o nome
  for (algorithm.name in algorithms)
  {
    if (verbose)
    {
      cat("\n\n######################################################################
          Executando Algoritmo:", algorithm.name,
          "\n######################################################################\n\n");
      capture.output({cat("\n\n",format(Sys.time(), "%a %b %d %X"),"\nTestando algoritmo: ",algorithm.name," id:", tst.id,"\n");}, file="output/log_folder/log", append=TRUE);
    }
    function.name <- paste0("perform.generalization.",algorithm.name);
    tryCatch({do.call(function.name, args);}, error = function(e) 
    {capture.output({cat("\n\n",format(Sys.time(), "%a %b %d %X"),"erro em: ",algorithm.name," id:", tst.id,"\n");print(e);}, file="output/log_folder/log.ERR", append=TRUE);    
    }); 
    capture.output({cat("\n",format(Sys.time(), "%a %b %d %X")," - FIM ALGORITMO: : ",algorithm.name," id:", tst.id,"\n");}, file="output/log_folder/log", append=TRUE);
  }
  capture.output({cat("\n\n",format(Sys.time(), "%a %b %d %X"),"\n### FIM: ",tst.id,"\n");}, file="output/log_folder/log", append=TRUE);
}

# passar funcao com parametros como argumento
# http://r.789695.n4.nabble.com/function-pointer-question-td2064621.html
perform.cv.all <- function(n, k, ds.id = "mod", subset, transformation = c(), imputation.method = c(), training.only.complete = FALSE, testing.only.complete = FALSE, test.cases.remove = c(), algorithms, tst.id, verbose = TRUE)
{
  args <- list(n = n, k = k, ds.id = ds.id, subset = subset, transformation = transformation, imputation.method = imputation.method, training.only.complete = training.only.complete, testing.only.complete = testing.only.complete, test.cases.remove = test.cases.remove,  tst.id = tst.id, verbose = verbose);
  capture.output({cat("\n\n",format(Sys.time(), "%a %b %d %X"),"\n%%%%%%%%%%%%%%%%%%%%%%% Inicio: ",tst.id,"\n");}, file="output/log_folder/log", append=TRUE);
  # chama as funcoes mudando o nome
  for (algorithm.name in algorithms)
  {
    if (verbose)
    {
      cat("\n\n######################################################################
                     Executando Algoritmo:", algorithm.name,
          "\n######################################################################\n\n");
      capture.output({cat("\n\n",format(Sys.time(), "%a %b %d %X"),"\nTestando algoritmo: ",algorithm.name," id:", tst.id,"\n");}, file="output/log_folder/log", append=TRUE);
    }
    function.name <- paste0("perform.tests.",algorithm.name);
    tryCatch({do.call(function.name, args);}, error = function(e) 
      {capture.output({cat("\n\n",format(Sys.time(), "%a %b %d %X"),"erro em: ",algorithm.name," id:", tst.id,"\n");print(e);}, file="output/log_folder/log.ERR", append=TRUE);    
       }); 
    capture.output({cat("\n",format(Sys.time(), "%a %b %d %X")," - FIM ALGORITMO: : ",algorithm.name," id:", tst.id,"\n");}, file="output/log_folder/log", append=TRUE);
  }
  capture.output({cat("\n\n",format(Sys.time(), "%a %b %d %X"),"\n### FIM: ",tst.id,"\n");}, file="output/log_folder/log", append=TRUE);
}


# testeS do.call
# myfun <- function (name, surname)
# {
#   cat("name: ", name, "surname:", surname);
# }
# os tres.pontos servem para passar argumentos de uma funcao para outra
# fatherfun <- function(father.name, ...)
# {
#   cat("father.name: ", father.name, "\n");
#   myfun(...);
# }
# fun <- function(father.name, ...)
# {
#   #fatherfun(father.name=father.name, ...); #OK
#   do.call("fatherfun", list(father.name=father.name, ...)); #OK
# }
# fatherfun(father.name="sergio", name="andre", surname="olivera"); #OK
# do.call("myfun", list(name = "andre", surname = "olivera")); #OK
# fun(father.name="sergio", name="andre", surname="olivera"); #OK

# nao esta em uso
perform.generalization.all2 <- function(algorithms, ...)
{
  args <- list(training = training, testing = testing, best.cutoff = best.cutoff, ...);
  # chama as funcoes mudando o nome
  for (algorithm.name in algorithms)
  {
    function.name <- paste0("perform.generalization.",algorithm.name);
    do.call(function.name, args);
  }
}