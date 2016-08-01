############################################################################
######################## perform.tests.knn.R ###############################
#
# Algoritmo: K-Nearest Neighbours
#
# Funções para geração de modelos e validação cruzada.
# Também tem uma função para predição do teste de generalização.
#
############################################################################

perform.cv.knn <- function(n,k, ds.id = "mod", subset = c(), transformation = c(), imputation.method = c(), training.only.complete = TRUE, testing.only.complete = TRUE, test.cases.remove = c(), cutoffs = 0.5, param.df = data.frame(neighbor = 10, min.votes = 1), verbose = TRUE)
{
  result <- list();
  for (test in 1:nrow(param.df)) 
  {
    result[[test]] <- 0;
    # coloquei o tryCatch devido a um erro que dá em algumas configurações de parametros
    # nesse caso ele nao preenche o result com uma lista (é feito um teste para saber se está preenchido como lista, ignorando caso contrario)
    test.fail <- FALSE;
    possibleError <-  tryCatch({
      if (verbose)
      {
        cat("\nParam:", test,"de",nrow(param.df),"\n");
      }
      ## ESPECIFICO DO ALGORITMO
      param.neighbor <- param.df$neighbor[test];
      param.min.votes <- param.df$min.votes[test];
      
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
          
          if (length(imputation.method) > 0) {
            cv_id <- sprintf("%simput-",cv_id);
          }      

          cv_id <- sprintf("%strn_comp-",cv_id);

          if (testing.only.complete) 
    		  {
    			  #testing = get.complete.cases(dataset = testing, verbose = FALSE);
            cv_id <- sprintf("%stst_comp-",cv_id);
    		  }       		
          
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
          
          ## ESPECIFICO DO ALGORITMO
          # knn dá um erro se o parametro de minimos votos for igual ou maior que  o numero de vizinhos
          # vou setar todas probabilidades para zero quando isso acontece   
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
          
          training = get.complete.cases(dataset = training, verbose = FALSE);
          index.class <- which(colnames(training) == "a_dm");
          idx.subset.knn <- which(colnames(training) %in% subset);
          set.seed(42);
          pred.aux <- knn(training[, idx.subset.knn], testing[, idx.subset.knn], training$a_dm, k = param.neighbor, l = param.min.votes, prob = TRUE);
          notPredictedCases <- length(which(is.na(pred.aux)));
          if (notPredictedCases > 0) 
          {
            test.fail <- TRUE;
            capture.output({cat("\n\nKNN:", 
                                " \nCasos nao previstos:",notPredictedCases,"\n");}, 
                           file="output/log_folder/log_na_test", append=TRUE);
          }
          prob <- attributes(pred.aux)$prob;
          class_prob <- prob;
          for (j in 1:length(prob))
          {
            # ERRO if (pred.aux[j] == 0) { : missing value where TRUE/FALSE needed
            if (pred.aux[j] == 0)
            {
              class_prob[j] <- prob[j];  
            }
            else
            {
              class_prob[j] <- 1 - prob[j]; 
            }      
          }
          pred.resp <- class_prob;
          
          # Adicionei aqui para facilitar
          fold.count <- fold.count + 1;
          result.roc.cv[[fold.count]] <- roc(testing$a_dm, pred.resp);
          conf[[fold.count]] <- list(); # em cada fold, vai ter uma matriz de confusao para cada cutoff
          for(index.cutoff in 1:length(cutoffs))
          {
            pred.class <- ifelse(pred.resp > cutoffs[index.cutoff] , 0, 1);
            pred.class <- factor(pred.class, levels = c("1", "0"));
            xtab <- table(pred = pred.class, truth = testing$a_dm);
            conf[[fold.count]][[index.cutoff]] <- confusionMatrix(xtab, positive = "0");
          }
        }        
      } 
    }, error = function(e) {e});   
    test.fail <- inherits(possibleError, "error");
    if (!test.fail)
      result[[test]] = list(conf, result.roc.cv);
  }
  return(result);
}

## ESPECIFICO DO ALGORITMO nome da funcao root.path default
output.cv.results.knn <- function(result, cutoffs, param.df, number.folds, root.path = "output/results/knn", tst.id = "cv_mean_test", verbose = TRUE)
{
  dir.create(root.path, showWarnings = FALSE);
  output.folder <- file.path(root.path, tst.id);
  output.folder.csv <- file.path("output/results");
  dir.create(output.folder.csv, showWarnings = FALSE);
  dir.create(output.folder, showWarnings = FALSE);
  
  # gerar uma tabela onde cada linha é um teste e as colunas são os parâmetros do teste e o resultado de cada caso
  # e colocar num csv

  cutoff <- c();
  balanced_accuracy.roc.mean <- c();
  sensitivity.roc.mean <- c();
  specificity.roc.mean <- c();
  auc.mean <- c();
  balanced_accuracy.roc.sd <- c();
  sensitivity.roc.sd <- c();
  specificity.roc.sd <- c();
  auc.sd <- c(); auc.1q <- c(); auc.3q <- c(); 
  
  balanced_accuracy.roc.list <- list();
  sensitivity.roc.list <- list();
  specificity.roc.list <- list();
  auc.list <- list();
  
  param <- c();
  ## ESPECIFICO DO ALGORITMO
  neighbor <- c(); 
  min.votes <- c();  
  tem_resultado <- FALSE;
  for (test in 1:nrow(param.df))
  {      
    if (is.list(result[[test]]))
    {
      tem_resultado <- TRUE;
      ## ESPECIFICO DO ALGORITMO
      param.neighbor <- param.df$neighbor[test];    
      param.min.votes <- param.df$min.votes[test];
      
      balanced_accuracy.roc.list[[test]] <- list();  # precisa para fazer os boxplots     
      for(index.cutoff in 1:length(cutoffs))
      {
        conf <- result[[test]][[1]];
        result.roc.cv <- result[[test]][[2]]; 
        
        # coloca num vetor os resultados de cada fold         
        balanced_accuracy.roc.list[[test]][[index.cutoff]] <- c(rep(0, number.folds));        
        sensitivity.roc.list[[test]] <- c(rep(0, number.folds));
        specificity.roc.list[[test]] <- c(rep(0, number.folds));
        auc.list[[test]] <- c(rep(0, number.folds));
        for (fold in 1:number.folds)
        {
          balanced_accuracy.roc.list[[test]][[index.cutoff]][fold] <- conf[[fold]][[index.cutoff]]$byClass[[8]];
          sensitivity.roc.list[[test]][fold] <- conf[[fold]][[index.cutoff]]$byClass[[1]];
          specificity.roc.list[[test]][fold] <- conf[[fold]][[index.cutoff]]$byClass[[2]];
          auc.list[[test]][fold] <- auc(result.roc.cv[[fold]]);
        }
        
        # calcula a média dos folds da validacao cruzada
        balanced_accuracy.roc.mean <- c(balanced_accuracy.roc.mean, mean(balanced_accuracy.roc.list[[test]][[index.cutoff]])); 
        sensitivity.roc.mean  <- c(sensitivity.roc.mean, mean(sensitivity.roc.list[[test]]));
        specificity.roc.mean  <- c(specificity.roc.mean, mean(specificity.roc.list[[test]]));
        auc.mean <- c(auc.mean, mean(auc.list[[test]]));
        
        # calcula desvio padrao
        balanced_accuracy.roc.sd <- c(balanced_accuracy.roc.sd, sd(balanced_accuracy.roc.list[[test]][[index.cutoff]])); 
        sensitivity.roc.sd  <- c(sensitivity.roc.sd, sd(sensitivity.roc.list[[test]]));
        specificity.roc.sd  <- c(specificity.roc.sd, sd(specificity.roc.list[[test]]));
        auc.sd <- c(auc.sd, sd(auc.list[[test]]));
        
        auc.1q <- c(auc.1q, quantile(auc.list[[test]])[[2]]);
        auc.3q <- c(auc.3q, quantile(auc.list[[test]])[[4]]);
        
        cutoff <- c(cutoff, cutoffs[index.cutoff]);
        param <- c(param, test);
        ## ESPECIFICO DO ALGORITMO
        neighbor <- c(neighbor, param.neighbor);
        min.votes <- c(min.votes, param.min.votes);
      }
    }
  } 
  if (tem_resultado) {
    ## primeiros parametros ESPECIFICO DO ALGORITMO
    result.dataframe <- data.frame(param, min.votes, neighbor, auc.mean, auc.sd, auc.1q, auc.3q, cutoff, balanced_accuracy.roc.mean, balanced_accuracy.roc.sd,
                                   sensitivity.roc.mean, sensitivity.roc.sd, specificity.roc.mean, specificity.roc.sd);
    result.dataframe <- result.dataframe[ order(-auc.mean, -balanced_accuracy.roc.mean), ];
    fname <- sprintf("%s/result_knn_%s.csv",output.folder.csv, tst.id);
    write.csv(result.dataframe, file=fname, row.names = FALSE);
    
    fname <- sprintf("%s/auc.list_%s",output.folder, tst.id);
    save(auc.list, file = fname);
    fname <- sprintf("%s/balanced_accuracy.roc.list_%s",output.folder, tst.id);
    save(balanced_accuracy.roc.list, file = fname);
    fname <- sprintf("%s/sensitivity.roc.list_%s",output.folder, tst.id);
    save(sensitivity.roc.list, file = fname);
    fname <- sprintf("%s/specificity.roc.list_%s",output.folder, tst.id);
    save(specificity.roc.list, file = fname);
    
    if (!exists("is.tuning")) {
      
      # salva curvas plot em arquivos de imagem
      output.folder.roc_curves <- file.path(output.folder, "roc_curves");
      dir.create(output.folder.roc_curves, showWarnings = FALSE);
      for (test in 1:nrow(param.df))
      {
        if (is.list(result[[test]]))
        {
          result.roc.cv <- result[[test]][[2]]; 
          params <- paste("Param_", test, sep = "");
          fname <- sprintf("%s/%s.png",output.folder.roc_curves, params);
          png(filename=fname);
          print(plot(result.roc.cv[[1]]));
          for (fold in 2:number.folds)
          {
            print(plot(result.roc.cv[[fold]], add = TRUE));
          }      
          dev.off();
        }  
      }
      
      # tamanho vertical dos boxplots
      ylim.inf <- 0.35;
      ylim.sup <- 0.85;
      # gerar boxplots.. uma imagem com vários boxplots, cada um é para um parametro 
      output.folder.roc_curves <- file.path(output.folder, "boxplots");
      dir.create(output.folder.roc_curves, showWarnings = FALSE);
      
      fname <- sprintf("%s/auc.png",output.folder.roc_curves);
      png(filename=fname, width = 1200, height = 1200, res = 120);
      print(boxplot(auc.list,  ylim=c(ylim.inf,ylim.sup)));
      dev.off();
      
      # cria um arquivo para cada ponto de corte 
      # outra opcao seria um arquivo para cada parametro
      # depende o que se quer comparar
      for(index.cutoff in 1:length(cutoffs))
      {
        # coloca valores em uma lista auxiliar para poder fazer os boxplots
        # cada item da lista vira uma caixa diferente no plot (representa uma iteração do cv com um dos parametros)
        aux.list <- list();
        for (test in 1:nrow(param.df))
        {
          if (is.list(result[[test]])) {
            bal.acc.values <- balanced_accuracy.roc.list[[test]][[index.cutoff]];
            aux.list[[test]] <- bal.acc.values;
          }
        }
        fname <- sprintf("%s/balanced_accuracy-%0.3f-.png",output.folder.roc_curves, cutoffs[index.cutoff]);
        png(filename=fname, width = 1200, height = 1200, res = 120);
        print(boxplot(aux.list,  ylim=c(ylim.inf,ylim.sup)));   
        dev.off();
      }  
    }
  }
}
  
## ESPECIFICO DO ALGORITMO - nome funcao
check.generalization.knn <- function (training, testing, best.cutoff, best.neighbor, best.min.votes)
{
  index.class <- which(colnames(training) == "a_dm");
  
  ## ESPECIFICO DO ALGORITMO
  pred.aux <-  knn(training[, -index.class], testing[, -index.class], training$a_dm, k = best.neighbor, l = best.min.votes, prob = TRUE);
  prob <- attributes(pred.aux)$prob;
  class_prob <- prob;
  for (j in 1:length(prob))
  {
    if (pred.aux[j] == 0)
    {
      class_prob[j] <- prob[j];  
    }
    else
    {
      class_prob[j] <- 1 - prob[j]; 
    }      
  }
  pred <- class_prob;
  
  pred.final <- ifelse(pred > best.cutoff , 0, 1);
  pred.final <- factor(pred.final, levels = c("1", "0"));
  xtab <- table(pred = pred.final, truth = testing$a_dm);
  conf <- confusionMatrix(xtab, positive = "0");

  result.roc <- roc(testing$a_dm, pred);
  cat("\nAnalise ROC do teste:\n");
  plot(result.roc, print.thres="best", print.thres.best.method="closest.topleft",  lwd=3, bty="l");
  print(auc(result.roc));
  
  cat("\n\nClassificacao com cutoff passado como parametro:\n");
  print(conf);
}

## ESPECIFICO DO ALGORITMO - nome funcao
perform.tests.knn <- function(n,k, ds.id, subset = c(), transformation = c(), imputation.method = c(), training.only.complete = FALSE, testing.only.complete = FALSE, test.cases.remove = c(), tst.id, verbose = TRUE)
{
  param.df <- data.frame(neighbor = numeric(0), min.votes = numeric(0));
  param.df[1,] <- c(475, 0);

  
  # como acessar os parametros
  cutoffs <- c(0.1);
  set.seed(666);
  
  ## ESPECIFICO DO ALGORITMO - chamada funcao
  number.folds <- n * k;
  result <- perform.cv.knn(n=n, k=k, ds.id = ds.id, subset = subset, transformation = transformation, imputation.method = imputation.method, 
                           training.only.complete = training.only.complete, testing.only.complete = testing.only.complete, 
                           test.cases.remove = test.cases.remove,  cutoffs = cutoffs, param.df = param.df, verbose = verbose);  
  output.cv.results.knn(result = result, cutoffs = cutoffs, param.df = param.df, number.folds = number.folds, root.path = "output/results/knn", 
                        tst.id = tst.id, verbose = verbose); 

}

## ESPECIFICO DO ALGORITMO - nome funcao
perform.generalization.knn <- function(dataset.testing, dataset.training, transformation, training.only.complete, testing.only.complete, tst.id, verbose = FALSE)
{
  
  
  res <- scale.numeric(dataset.training, dataset.testing);
  dataset.training <- res[[1]];
  dataset.testing <- res[[2]];
  
  if (length(imputation.method) > 0) {
    imput.parameters <- get.imput.parameters(dataset.training);          
    if (imputation.method == "imput.train") {
      dataset.training <- naive.imput(dataset.training, imput.parameters);
      
    }           
  }
  
  if (length(test.cases.remove) > 0) {
    testing = testing[-test.cases.remove, ];
  }
  
	if (training.only.complete) 
	{
		dataset.training = get.complete.cases(dataset = dataset.training, verbose = FALSE);
	}

	if (testing.only.complete) 
	{
		dataset.testing = get.complete.cases(dataset = dataset.testing, verbose = FALSE);
	}
  
	if (length(transformation) > 0) 
	{
	  if (transformation == "numeric") 
	  {
		res <- transform.to.numeric(dataset.training, dataset.testing);
		dataset.training <- res[[1]];
		dataset.testing <- res[[2]]; 
	  } 
	  else if (transformation == "factor")
	  {
		res <- discretize(dataset.training, dataset.testing);
		dataset.training <- res[[1]];
		dataset.testing <- res[[2]]; 
	  }
	} 
  index.class <- which(colnames(dataset.training) == "a_dm");
  model.knn <- dataset.training;
  save(model.knn, file='output/new_models/model.knn');
  generalization.predict.knn(model.knn, dataset.testing, tst.id);
}

generalization.predict.knn <- function(model, dataset.testing, tst.id, model.id = "orig", subset.id = "unico") {
  best.neighbor <- 475;
  best.min.votes <- 0;
  best.cutoff <- 0.1;
  
  dataset.testing <- dataset.testing[, names(model)];
  
  index.class <- which(colnames(dataset.testing) == "a_dm");
  pred.aux <- knn(model[, -index.class], dataset.testing[, -index.class], model$a_dm, k = best.neighbor, l = best.min.votes, prob = TRUE);
  prob <- attributes(pred.aux)$prob;
  class_prob <- prob;
  for (j in 1:length(prob))
  {
    # ERRO if (pred.aux[j] == 0) { : missing value where TRUE/FALSE needed
    if (pred.aux[j] == 0)
    {
      class_prob[j] <- prob[j];  
    }
    else
    {
      class_prob[j] <- 1 - prob[j]; 
    }      
  }
  pred.resp <- class_prob;
  
  notPredictedCases <- length(which(is.na(pred.resp)));
  if (notPredictedCases > 0) 
  {
    capture.output({cat("\n\nKNN:", 
                        " \nCasos nao previstos:",notPredictedCases,"\n");}, 
                   file="output/log_folder/log_na_test", append=TRUE);
  }
  result.roc <- roc(dataset.testing$a_dm, pred.resp);
  
  pred.class <- ifelse(pred.resp > best.cutoff , 0, 1);
  pred.class <- factor(pred.class, levels = c("1", "0"));
  xtab <- table(pred = pred.class, truth = dataset.testing$a_dm);
  conf <- confusionMatrix(xtab, positive = "0");  
  if (!exists("verbose"))
    verbose = FALSE;
  if (verbose) 
  {
    cat("\n\nAnalise ROC do teste:\n");
    plot(result.roc, print.thres=best.cutoff, lwd=3, bty="l");
    print(auc(result.roc));
    cat("\n\nClassificacao:\n");
    print(conf);
  }
  
  # salva resultados
  balanced_accuracy <- conf$byClass[[8]];
  sensitivity <- conf$byClass[[1]];
  specificity <- conf$byClass[[2]];
  auc <- auc(result.roc);
  
  output.folder <- file.path("output/results");
  dir.create(output.folder, showWarnings = FALSE);
  result.dataframe <- data.frame("K-NN", model.id, subset.id, auc, balanced_accuracy, sensitivity, specificity);
  fname <- sprintf("%s/generalization_%s.csv",output.folder, tst.id);
  write.table(result.dataframe, file=fname, fileEncoding = "UTF-8",sep = ",",quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE);
  
  # salva curvas plot em arquivos de imagem
  output.folder.roc_curves <- file.path(output.folder, "roc_curves");
  dir.create(output.folder.roc_curves, showWarnings = FALSE);
  fname <- sprintf("%s/generalization_%s_knn.png",output.folder.roc_curves, tst.id);
  png(filename=fname);
  print(plot(result.roc));
  dev.off();
}
