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
    # realiza validacao cruzada dos algoritmos passados como parametro
    perform.cv.all(n = n, k = k, ds.id = ds.id, subset = subset, algorithms = algorithms, 
                   tst.id = tst.id, transformation = transformation, imputation.method = imputation.method, training.only.complete = training.only.complete, 
                   testing.only.complete = testing.only.complete, test.cases.remove = test.cases.remove,  verbose = TRUE);
  } 
}

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