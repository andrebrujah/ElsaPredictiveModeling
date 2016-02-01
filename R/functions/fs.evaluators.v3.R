############################################################################
##################### functions/fs.evaluators.v3.R #########################
# 
# Script contendo método utilizado na seleção automática de variáveis
# para avaliar um subconjunto passado como parâmetro. Devido a  restrição de
# parâmetros o método utiliza dados que devem estar pré-carregadas em 
# memória.
#
############################################################################

evaluator <- function(subset) {
  #k-fold cross validation
  if (!exists("n"))
    n <- 3;
  if (!exists("k"))
    k <- 10;
    
  if (!exists("training.only.complete")) 
    training.only.complete = TRUE;
  
  if (!exists("testing.only.complete"))
    testing.only.complete = TRUE;
      
  if (!exists("ds.id"))
    ds.id <- "tuning.ds";
    
  if (!exists("algorithm"))
    algorithm <- "glm";
  
  if (!exists("transformation")) {
    if (algorithm == "nb") {
      transformation <- "factor";
    } else {
      transformation <- c();
    }  
  }
  
  cv_id <- "";
  results.rep <- c();
  for (rep in 1:n) {
    results = sapply(1:k, function(i) {
      test_failed <- FALSE;
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
      training.namefile <- sprintf("%s/training-%s%s-%s",output.folder, cv_id, rep, i);
      testing.namefile <- sprintf("%s/testing-%s%s-%s",output.folder, cv_id, rep, i);
      load(file = training.namefile);
      load(file = testing.namefile);        
      index.class <- which(colnames(training) == "a_dm");
      # quando pega o BD factor, nao existe variavel a_imc1, porem ela pode estar no subset e gerar erro
      # precisa remover do subset
      idx.aimc1.subset <- which(subset == "a_imc1");
      idx.aimc1.train <- which(colnames(training) == "a_imc1");
      if (length(idx.aimc1.subset) > 0 && length(idx.aimc1.train) == 0) {
        subset <- subset[-idx.aimc1.subset];
      }
      if (algorithm == "glm") {
        best.epsilon <- 0.02;
        best.maxit <- 5;
        set.seed <- 42;
        model <- glm(as.simple.formula(subset, "a_dm"), family=binomial(link="logit"), data=training,  control=list(epsilon = best.epsilon, maxit = best.maxit));
        set.seed <- 43;
        pred.resp <- predict(model, testing, type='response');
      }
      
      if (algorithm == "nnet") {
        best.size <- 1;
        best.skip <- 1;
        best.decay <- 3;
        best.cutoff <- 0.1;
        set.seed <- 42;
        model <- nnet(as.simple.formula(subset, "a_dm"), data = training, size=best.size, skip = best.skip, decay = best.decay, MaxNWts=70000, maxit = 500, trace=FALSE);  
        set.seed <- 43;
        pred.resp <- predict(model, newdata = testing, type='raw'); 
      }  

      if (algorithm == "knn") {
        best.neighbor <- 440;
        best.min.votes <- 0;
        idx.subset.knn <- which(colnames(training) %in% subset);
        possibleError <- tryCatch({
          set.seed <- 42;
          pred.aux <- knn(training[, idx.subset.knn], testing[, idx.subset.knn], training$a_dm, k = best.neighbor, l = best.min.votes, prob = TRUE);
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
          pred.resp <- class_prob;
        }, error = function(e) {e});   
        test_failed <- inherits(possibleError, "error");
      }

      if (algorithm == "nb") {
        best.laplace <- 0.000001;  
        set.seed <- 42;
        model <- naiveBayes(as.simple.formula(subset, "a_dm"), data = training, laplace = best.laplace);
        set.seed <- 43;
        pred.aux <-  predict(model, newdata = testing, type='raw');
        pred.resp <- pred.aux[,"0"];
      }
      
      if (algorithm == "rf") {
        best.ntree <- 900;
        set.seed <- 42;
        model <- randomForest(as.simple.formula(subset, "a_dm"), data = training, ntree=best.ntree); 
        set.seed <- 43;
        pred.aux <-  predict(model, newdata = testing, type='prob');
        pred.resp<- pred.aux[,"0"]; 
      }
      
      if(!test_failed) {
        result.roc.cv <- roc(testing$a_dm, pred.resp);
        auc.roc <- auc(result.roc.cv);
      } else {
        auc.roc <- 0;
      }
      return(auc.roc)
    })
    results.rep <- c(results.rep, mean(results));
  }
  print(subset)
  print(mean(results.rep))
  return(mean(results.rep))
}