############################################################################
################ generate.datasource.cv.functions.R ########################
#
# Método utizado para criar e salvar em disco os conjuntos de dados utilizados
# nas diferentes iterações das validações cruzadas. Cada conjunto de dados 
# é identificado pelo seu nome, que contém entre outras informações: 
# se ele é de treino ou teste, pre-processamento utilizado e identificador 
# do fold e da repetição.
# O parâmetro n é configurável e indica o número de repetições da validação
# cruzada. Foram usados n = 3 e n = 10.
#
############################################################################

gerar_datasets <- function (dataset, ds.id, n, k, seed, transformation, imputation.method = c(), training.only.complete, testing.only.complete, test.cases.remove = c(), verbose = FALSE) {
  set.seed(seed);
  cv.partition  <- createDataPartition(dataset$a_dm, p = 0.70, list = FALSE);
  dataset <- dataset[cv.partition, ];
  
  # remove casos com dados faltantes
  dataset = get.complete.cases(dataset = dataset, verbose = FALSE);
  test.cases.remove <- c();
  
  # cria folds para validacao cruzada
  set.seed(seed);
  foldInds.list <- list();
  for (i in 1:n)
  {
    foldInds.list[[i]] <- createFolds(dataset$a_dm, k=k, list=TRUE, returnTrain=FALSE);
  }
  
  for(foldInds.index in 1:length(foldInds.list))
  {
    if (verbose)
    {
      cat("\nCV iteracao:", foldInds.index,"de",length(foldInds.list),"\n");
    }
    foldInds <- foldInds.list[[foldInds.index]];
    for (fold in 1:length(foldInds))
    {
      if (verbose)
      {
        cat("\nFold:", fold,"de",length(foldInds),"\n");
      }
      testing.ind <- foldInds[[fold]];
      training <- dataset[-testing.ind,];
      if (length(test.cases.remove) > 0) {
        testing.ind <- setdiff(testing.ind, test.cases.remove);
      }
      testing <- dataset[testing.ind,]; 
      cv_id <- "";
      if (length(imputation.method) > 0) {
        imput.parameters <- get.imput.parameters(training);          
        if (imputation.method == "imput.train") {
          training <- naive.imput(training, imput.parameters);
          cv_id <- sprintf("%simput-",cv_id);
        }           
      }
      
      if (training.only.complete) 
      {
        training = get.complete.cases(dataset = training, verbose = FALSE);
        cv_id <- sprintf("%strn_comp-",cv_id);
      }
      if (testing.only.complete) 
      {
        testing = get.complete.cases(dataset = testing, verbose = FALSE);
        cv_id <- sprintf("%stst_comp-",cv_id);
      }
      
      res <- scale.numeric(training, testing);
      training <- res[[1]];
      testing <- res[[2]]; 
      
      if (length(transformation) > 0) 
      {
        if (transformation == "numeric") 
        {
          res <- transform.to.numeric(training, testing);
          training <- res[[1]];
          testing <- res[[2]]; 
          cv_id <- sprintf("%snum-",cv_id);
        } else if (transformation == "factor")
        {
          res <- discretize(training, testing);
          training <- res[[1]];
          testing <- res[[2]]; 
          
          if (testing.only.complete) 
          {
            testing = get.complete.cases(dataset = testing, verbose = FALSE);
          }
          cv_id <- sprintf("%sfac-",cv_id);
        }
      } 
      dir.create("output/datasets", showWarnings = FALSE);
      output.folder <- file.path("output/datasets", sprintf("%s-cv%s-%s", ds.id, n, k));
      dir.create(output.folder, showWarnings = FALSE);
      training.namefile <- sprintf("%s/training-%s%s-%s",output.folder, cv_id, foldInds.index, fold);
      testing.namefile <- sprintf("%s/testing-%s%s-%s",output.folder, cv_id, foldInds.index, fold);
      save(training, file = training.namefile);
      save(testing, file = testing.namefile);
    }
  }
}