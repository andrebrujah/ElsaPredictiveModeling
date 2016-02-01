############################################################################
######################## perform.tests.c5alt.R #############################
#
# Algoritmo: C5 (árvore de decisão)
#
# Funções para geração de modelos e validação cruzada.
# Também tem uma função para predição do teste de generalização.
#
############################################################################
perform.cv.c5alt <- function(n, k, ds.id = "mod", subset = c(), transformation = c(), imputation.method = c(), training.only.complete = FALSE, testing.only.complete = FALSE, test.cases.remove = c(), cutoffs = 0.5, param.df, verbose = TRUE)
{
  # best.minCases, best.fuzzyThreshold, best.earlyStopping, best.winnow, best.noGlobalPruning
  result <- list();
  for (test in 1:nrow(param.df)) {
    result[[test]] <- 0;
  }
  for (test in 1:nrow(param.df)) 
  {
    test.fail <- FALSE;
    if (verbose)
    {
      cat("\nParam:", test,"de",nrow(param.df),"\n");
    }
    ## ESPECIFICO DO ALGORITMO
    param.minCases <- param.df$minCases[test];
    param.fuzzyThreshold <- param.df$fuzzyThreshold[test];
    param.earlyStopping <- param.df$earlyStopping[test];
    param.winnow <- param.df$winnow[test];
    param.noGlobalPruning <- param.df$noGlobalPruning[test];
    misclassification.cost <- param.df$costs[test];
    param.costs <- matrix(c(
      NA, 1,
      misclassification.cost, NA    
    ), 2, 2, byrow=TRUE);
    rownames(param.costs) <- colnames(param.costs) <- c("0", "1");
    
    fold.count <- 0; # para armazenar os resultados em uma lista
    result.roc.cv <- list();
    conf <- list();
    for(foldInds.index in 1:n)
    {
      if (verbose)
      {
        cat("\nCV iteracao:", foldInds.index,"de",n,"\n");
      }
      for (fold in 1:k)
      {
        if (verbose)
        {
          cat("\nFold:", fold,"de",k,"\n");
        }    
        cv_id <- "";
        
        if (training.only.complete) 
        {
          cv_id <- sprintf("%strn_comp-",cv_id);
        }
        
        if (testing.only.complete) 
        {
          cv_id <- sprintf("%stst_comp-",cv_id);
        }
        
        ### transformacoes de tipo
        if (length(transformation) > 0) 
        {
          if (transformation == "numeric") 
          {
            cv_id <- sprintf("%snum-",cv_id); 
          } else if (transformation == "factor")
          {
            cv_id <- sprintf("%sfac-",cv_id);
          }
          
        } 
        
        output.folder <- file.path("output/datasets", sprintf("%s-cv%s-%s", ds.id, n, k));
        training.namefile <- sprintf("%s/training-%s%s-%s",output.folder, cv_id, foldInds.index, fold);
        testing.namefile <- sprintf("%s/testing-%s%s-%s",output.folder, cv_id, foldInds.index, fold);
        load(file = training.namefile);
        load(file = testing.namefile);     
        # verifica quais as variaveis do conjunto de treino que começam com o nome 
        # das variaveis que estao no subset
        # se variavel do subset nao existir (como a a_imc1) ela sera desconsiderada
        # se existir mais de uma variavel como no BD numerico, ele vai considerar 
        # todas variaveis criadas pela binarização
        subset.aux <- subset;
        subset <- c(); # modifiquei o subset para nao precisar modificar outro codigo
        for (varname in subset.aux) {
          idx.var <- grep(varname, colnames(training));
          if (length(idx.var) > 0) {
            subset <- c(subset, colnames(training)[idx.var]);  
          }
        }
        index.class <- which(colnames(training) == "a_dm");
        set.seed(42);
        model <- C5.0(as.simple.formula(subset, "a_dm"), data = training, trials = 1, costs = param.costs,
                      control = C5.0Control(minCases = param.minCases, fuzzyThreshold = param.fuzzyThreshold, earlyStopping = param.earlyStopping, winnow = param.winnow, noGlobalPruning = param.noGlobalPruning)); 
        set.seed(43);
        pred.class <- predict(model, newdata = testing[,-index.class], type='class');
        notPredictedCases <- length(which(is.na(pred.class)));
        if (notPredictedCases > 0) 
        {
          test.fail <- TRUE;
          capture.output({cat("\n\nC5ALT:", 
                              " \nCasos nao previstos:",notPredictedCases,"\n");}, 
                         file="output/log_folder/log_na_test", append=TRUE);
        }
        fold.count <- fold.count + 1;
        xtab <- table(pred = pred.class, truth = testing$a_dm);
        conf[[fold.count]] <- confusionMatrix(xtab, positive = "0");
      }
      if (!test.fail)
      result[[test]] = list(conf, result.roc.cv);
    }
  }
  return(result);
}

output.cv.results.c5alt <- function(result, cutoffs, param.df, number.folds, root.path = "output/results/c5ALT", tst.id = "cv_mean_test", verbose = TRUE)
{
  dir.create(root.path, showWarnings = FALSE);
  output.folder <- file.path(root.path, tst.id);
  output.folder.csv <- file.path("output/results");
  dir.create(output.folder.csv, showWarnings = FALSE);
  dir.create(output.folder, showWarnings = FALSE);
  
  # gerar uma tabela onde cada linha é um teste e as colunas são os parâmetros do teste e o resultado de cada caso
  # e colocar num csv

  balanced_accuracy.roc.mean <- c();
  sensitivity.roc.mean <- c();
  specificity.roc.mean <- c();
  
  balanced_accuracy.roc.sd <- c();
  sensitivity.roc.sd <- c();
  specificity.roc.sd <- c();
  
  balanced_accuracy.roc.list <- list();
  sensitivity.roc.list <- list();
  specificity.roc.list <- list();
  
  param <- c();
  ## ESPECIFICO DO ALGORITMO  
  costs <- c(); 
  minCases <- c();
  fuzzyThreshold <- c();
  earlyStopping <- c();
  winnow <- c();
  noGlobalPruning <- c();  
  for (test in 1:nrow(param.df))
  {      
    if (is.list(result[[test]]))
    {
      ## ESPECIFICO DO ALGORITMO
      param.costs <- param.df$costs[test];
      param.minCases <- param.df$minCases[test];
      param.fuzzyThreshold <- param.df$fuzzyThreshold[test];
      param.earlyStopping <- param.df$earlyStopping[test];
      param.winnow <- param.df$winnow[test];
      param.noGlobalPruning <- param.df$noGlobalPruning[test];    
      
      balanced_accuracy.roc.list[[test]] <- list();  # precisa para fazer os boxplots     
      conf <- result[[test]][[1]];
      balanced_accuracy.roc.list[[test]] <- c(rep(0, number.folds));        
      sensitivity.roc.list[[test]] <- c(rep(0, number.folds));
      specificity.roc.list[[test]] <- c(rep(0, number.folds));
      for (fold in 1:number.folds)
      {
        balanced_accuracy.roc.list[[test]][fold] <- conf[[fold]]$byClass[[8]];
        sensitivity.roc.list[[test]][fold] <- conf[[fold]]$byClass[[1]];
        specificity.roc.list[[test]][fold] <- conf[[fold]]$byClass[[2]];
      }
      
      # calcula a média dos folds da validacao cruzada
      balanced_accuracy.roc.mean <- c(balanced_accuracy.roc.mean, mean(balanced_accuracy.roc.list[[test]])); 
      sensitivity.roc.mean  <- c(sensitivity.roc.mean, mean(sensitivity.roc.list[[test]]));
      specificity.roc.mean  <- c(specificity.roc.mean, mean(specificity.roc.list[[test]]));
      
      # calcula desvio padrao
      balanced_accuracy.roc.sd <- c(balanced_accuracy.roc.sd, sd(balanced_accuracy.roc.list[[test]])); 
      sensitivity.roc.sd  <- c(sensitivity.roc.sd, sd(sensitivity.roc.list[[test]]));
      specificity.roc.sd  <- c(specificity.roc.sd, sd(specificity.roc.list[[test]]));

      param <- c(param, test);
      costs <- c(costs, param.costs);
      minCases <- c(minCases, param.minCases);
      fuzzyThreshold <- c(fuzzyThreshold, param.fuzzyThreshold);
      earlyStopping <- c(earlyStopping, param.earlyStopping);
      winnow <- c(winnow, param.winnow);
      noGlobalPruning <- c(noGlobalPruning, param.noGlobalPruning);
    }
  } 
  result.dataframe <- data.frame(param, costs, minCases, fuzzyThreshold, earlyStopping, winnow, noGlobalPruning, balanced_accuracy.roc.mean, balanced_accuracy.roc.sd,
                                 sensitivity.roc.mean, sensitivity.roc.sd, specificity.roc.mean, specificity.roc.sd);
  result.dataframe <- result.dataframe[ order(-balanced_accuracy.roc.mean), ]; 
  fname <- sprintf("%s/result_c5alt_%s.csv",output.folder.csv, tst.id);
  write.csv(result.dataframe, file=fname, row.names = FALSE);
  # cria um arquivo para cada ponto de corte 
  # outra opcao seria um arquivo para cada parametro
  # depende o que se quer comparar
  
  # coloca valores em uma lista auxiliar para poder fazer os boxplots
  # cada item da lista vira uma caixa diferente no plot (representa uma iteração do cv com um dos parametros)
  aux.list <- list();
  for (test in 1:nrow(param.df))
  {
    if (is.list(result[[test]]))
    {
      bal.acc.values <- balanced_accuracy.roc.list[[test]];
      aux.list[[test]] <- bal.acc.values;
    }
  }
  ylim.inf <- 0.35;
  ylim.sup <- 0.85;
  # gerar boxplots.. uma imagem com vários boxplots, cada um é para um parametro 
  output.folder.roc_curves <- file.path(output.folder, "boxplots");
  dir.create(output.folder.roc_curves, showWarnings = FALSE);
  fname <- sprintf("%s/balanced_accuracy.png",output.folder.roc_curves);
  png(filename=fname, width = 1200, height = 1200, res = 120);
  print(boxplot(aux.list,  ylim=c(ylim.inf,ylim.sup)));   
  dev.off();
}
  
perform.tests.c5alt <- function(n, k, ds.id, subset = c(), transformation = c(), imputation.method = c(), training.only.complete = FALSE, testing.only.complete = FALSE, test.cases.remove = c(), tst.id, verbose = TRUE)
{
  # parametros para comparar e descobrir o melhor atraves de validacao cruzada  
  # parametros que variam
  # costs = 9; minCases = 1; fuzzyThreshold = 1; earlyStopping = 1; winnow = 1; noGlobalPruning = 0
  noGlobalPruning.vector <- c(FALSE);
  fuzzyThreshold.vector <- c(TRUE);
  earlyStopping.vector <- c(TRUE);
  winnow.vector <- c(TRUE);
  minCases.vector <- c(27);
  costs.vector <-c(9);
  # constroi lista combinando os parametros que variam
  # dai so precisa dessa lista para testar 
   param.df <- data.frame(costs = numeric(0), minCases = numeric(0), fuzzyThreshold = logical(), earlyStopping = logical(), winnow = logical(), noGlobalPruning = logical());
  param.df[1,] <- c(9, 240, TRUE, TRUE, FALSE, FALSE);
  param.df[2,] <- c(10, 230, TRUE, TRUE, FALSE, FALSE);
  param.df[3,] <- c(8, 230, TRUE, TRUE, FALSE, TRUE);
 
  # como acessar os parametros
  cutoffs <- c(seq(0.08, 0.13, 0.01));
  set.seed(666);
  
  number.folds <- n * k;
  result <- perform.cv.c5alt(n = n, k =  k, ds.id = ds.id, subset = subset, transformation = transformation, imputation.method = imputation.method, training.only.complete = training.only.complete, testing.only.complete = testing.only.complete, test.cases.remove = test.cases.remove,  cutoffs = cutoffs, param.df = param.df, verbose = verbose);
  output.cv.results.c5alt(result = result, cutoffs = cutoffs, param.df = param.df, number.folds = number.folds, root.path = "output/results/c5ALT", tst.id = tst.id, verbose = verbose); 
}

generalization.predict.c5alt <- function (model, dataset.testing, tst.id, model.id = "orig", subset.id = "unico") {
  pred.class <- predict(model, newdata = dataset.testing, type='class');
  
  notPredictedCases <- length(which(is.na(pred.class)));
  if (notPredictedCases > 0) 
  {
    capture.output({cat("\n\nC5ALT:", 
                        " \nCasos nao previstos:",notPredictedCases,"\n");}, 
                   file="output/log_folder/log_na_test", append=TRUE);
  }
  dataset.testing$a_dm <- factor(dataset.testing$a_dm, levels = c("1", "0"));
  xtab <- table(pred = pred.class, truth = dataset.testing$a_dm);
  conf <- confusionMatrix(xtab, positive = "0");  
  
  if (verbose) 
  {
    cat("\n\nClassificacao:\n");
    print(conf);
  }
  
  # salva resultados
  balanced_accuracy <- conf$byClass[[8]];
  sensitivity <- conf$byClass[[1]];
  specificity <- conf$byClass[[2]];
  auc <- 0;
  
  output.folder <- file.path("output/results");
  dir.create(output.folder, showWarnings = FALSE);
  result.dataframe <- data.frame("C5 (class)", model.id, subset.id, auc, balanced_accuracy, sensitivity, specificity);
  fname <- sprintf("%s/generalization_%s.csv",output.folder, tst.id);
  write.table(result.dataframe, file=fname, fileEncoding = "UTF-8",sep = ",",quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE);
}
