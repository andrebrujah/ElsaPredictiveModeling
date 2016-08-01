############################################################################
################## functions/pre.process.functions.R #######################
# 
# Funções para pré-processamento dos dados: padronização, discretização e 
# binarização. 
# Contém versões que tratam os conjuntos de treinamento e teste juntos e 
# versões que fazem o tratamento separadamente. Além de funções auxiliares.
#
############################################################################

scale.numeric <- function (training, testing, target.name = "a_dm")
{
  index.numeric <- which(sapply(training, is.numeric));  # Usar os parametros para testar no conjunto de teste:
  index.class <- which(colnames(training) == target.name);
  if (length(index.numeric) > 0)
  {
    preProc  <- preProcess(training[,index.numeric], method = c("center", "scale"));
    training[,index.numeric] <- predict(preProc, training[,index.numeric]);
    testing[,index.numeric] <- predict(preProc, testing[,index.numeric]);
  }
  result <- list(training, testing, preProc, index.numeric);
  return(result);
}

scale.train <- function (training, target.name = "a_dm")
{
  index.numeric <- which(sapply(training, is.numeric));  # Usar os parametros para testar no conjunto de teste:
  index.class <- which(colnames(training) == target.name);
  if (length(index.numeric) > 0)
  {
    preProc  <- preProcess(training[,index.numeric], method = c("center", "scale"));
    training[,index.numeric] <- predict(preProc, training[,index.numeric]);
  }
  names.numeric <- names(training)[index.numeric];
  result <- list(training, preProc, names.numeric);
  return(result);
}

scale.test <- function(testing, scale.preProc, train.numeric.names) 
{
  train.numeric.idx <- c();
  for (name in train.numeric.names) {
    num.idx <- which(colnames(testing)==name);
    if (length(num.idx) > 0)
      train.numeric.idx <- c(train.numeric.idx, num.idx);
  }
  
  testing.scaled <- testing;
  if (length(train.numeric.idx) > 0)
  {
    testing.scaled[,train.numeric.idx] <- predict(scale.preProc, testing[,train.numeric.idx]);
  }
  return (testing.scaled);
}

remove.numerical.redundants <- function (dataset, categoric.names.file = "data/AnaliseVariaveis/var_categoricas_mod.txt", 
                             numeric.names.file = "data/AnaliseVariaveis/var_numericas_mod.txt",
                             verbose = TRUE)
{
  eliminate.redundant.index <- c();
  if (length(categoric.names.file) > 0 && length(categoric.names.file) == length(numeric.names.file))
  {
    eliminate.redundant.index <- numerical.redundant.index(names(dataset), categoric.names.file, numeric.names.file);
    if (length(eliminate.redundant.index) > 0)
    {
      dataset <- dataset[,-eliminate.redundant.index];
    }
  }
  if (verbose)
  {
    cat("\nTotal of variables removed: ", length(eliminate.redundant.index)," from ",ncol(dataset),"\n");
  }
  return (dataset);
}
# http://www.rcreditscoring.com/binning-continuous-variables-in-r-the-basics/
discretize <- function(training, testing, categoric.names.file = "data/AnaliseVariaveis/var_categoricas_mod.txt", 
                       numeric.names.file = "data/AnaliseVariaveis/var_numericas_mod.txt", target.name = "a_dm", verbose = FALSE) 
{
  if (length(categoric.names.file) > 0 && length(categoric.names.file) == length(numeric.names.file))
  {
    eliminate.redundant.index <- numerical.redundant.index(names(training), categoric.names.file, numeric.names.file);
    if (length(eliminate.redundant.index) > 0)
    {
      training <- training[,-eliminate.redundant.index];
      testing <- testing[,-eliminate.redundant.index];
    }
  }
  
  index.class <- which(names(training) == target.name);

  disc.cm <- list();
  for (i in 1:ncol(training))
  {
    if (verbose)
    {
      cat ("processing col",i,"of", ncol(training),"\n");
    }
    
    if (is.numeric(training[,i]))
    {
      dataset.aux <- data.frame(x = training[,i], y = training[,index.class]);
      dataset.aux <- dataset.aux[complete.cases(dataset.aux), ];
      cm <- disc.Topdown(dataset.aux, method=3);
      # labels = FALSE para funcionar com KNN que tenta ler a coluna criada como um inteiro e da erro
      training[,i] <- cut(training[,i], unlist(cm$cutp), ordered_result = TRUE, include.lowest = TRUE, labels = FALSE);
      training[,i] <- as.ordered(training[,i]);
      # usa pontos de corte do treinamento para fazer a discretizacao dos dados de validacao
      testing[,i] <- cut(testing[,i], unlist(cm$cutp), ordered_result = TRUE, include.lowest = TRUE, labels = FALSE);
      testing[,i] <- as.ordered(testing[,i]);
      disc.cm[[i]] <- cm;
    } 
    else
    {
      disc.cm[[i]] <- 0;
    }
  }
  
  result <- list(training, testing, disc.cm);
  return(result);
}

discretize.train <- function(training, categoric.names.file = "data/AnaliseVariaveis/var_categoricas_mod.txt", 
                       numeric.names.file = "data/AnaliseVariaveis/var_numericas_mod.txt", target.name = "a_dm", verbose = FALSE) 
{
  if (length(categoric.names.file) > 0 && length(categoric.names.file) == length(numeric.names.file))
  {
    eliminate.redundant.index <- numerical.redundant.index(names(training), categoric.names.file, numeric.names.file);
    if (length(eliminate.redundant.index) > 0)
    {
      training <- training[,-eliminate.redundant.index];
    }
  }
  index.class <- which(names(training) == target.name);
  
  disc.cm <- list();
  for (i in 1:ncol(training))
  {
    if (verbose)
    {
      cat ("processing col",i,"of", ncol(training),"\n");
    }
    
    if (is.numeric(training[,i]))
    {
      dataset.aux <- data.frame(x = training[,i], y = training[,index.class]);
      dataset.aux <- dataset.aux[complete.cases(dataset.aux), ];
      cm <- disc.Topdown(dataset.aux, method=3);
      # labels = FALSE para funcionar com KNN que tenta ler a coluna criada como um inteiro e da erro
      training[,i] <- cut(training[,i], unlist(cm$cutp), ordered_result = TRUE, include.lowest = TRUE, labels = FALSE);
      training[,i] <- as.ordered(training[,i]);
      disc.cm[[i]] <- cm;
    } 
    else
    {
      disc.cm[[i]] <- 0;
    }
  }
  
  result <- list(training, disc.cm);
  return(result);
}

discretize.test <- function(testing, discretization.cm, categoric.names.file = "data/AnaliseVariaveis/var_categoricas_mod.txt", 
                               numeric.names.file = "data/AnaliseVariaveis/var_numericas_mod.txt") 
{
  if (length(categoric.names.file) > 0 && length(categoric.names.file) == length(numeric.names.file))
  {
    eliminate.redundant.index <- numerical.redundant.index(names(testing), categoric.names.file, numeric.names.file);
    if (length(eliminate.redundant.index) > 0)
    {
      testing <- testing[,-eliminate.redundant.index];
    }
  }
  testing.disc <- testing;
  for (i in 1:ncol(testing))
  {
    if (is.numeric(testing[,i]))
    {
      # coloca valores fora do intervalo para dentro do intervalo
      min.cutp <- min(unlist(discretization.cm[[i]]$cutp));
      max.cutp <- max(unlist(discretization.cm[[i]]$cutp));
      idx.menores <- which(testing[,i] <= min.cutp); # right=TRUE intervalo fechado a direita e aberto a esquerda
      idx.maiores <- which(testing[,i] > max.cutp);
      if (length(idx.menores) > 0) {
        testing[idx.menores,i] <- min(testing[-idx.menores,i]);
      }
      if (length(idx.maiores) > 0) {
        testing[idx.maiores,i] <- max(testing[-idx.maiores,i]);
      }
      
      testing.disc[,i] <- cut(testing[,i], unlist(discretization.cm[[i]]$cutp), ordered_result = TRUE, include.lowest = TRUE, labels = FALSE);
      testing.disc[,i] <- as.ordered(testing.disc[,i]);

    } 
  }
  return (testing.disc)
}


transform.to.numeric <- function(training, testing, target.name = "a_dm", remove.first = FALSE, 
                                 categoric.names.file = "data/AnaliseVariaveis/var_categoricas_mod.txt", 
                                 numeric.names.file = "data/AnaliseVariaveis/var_numericas_mod.txt", verbose = FALSE)
{
  if (length(categoric.names.file) > 0 && length(categoric.names.file) == length(numeric.names.file))
  {
    eliminate.redundant.index <- categorical.redundant.index(names(training), categoric.names.file, numeric.names.file);
    if (length(eliminate.redundant.index) > 0)
    {
      training <- training[,-eliminate.redundant.index];
      testing <- testing[,-eliminate.redundant.index];
    }
  }
  
  # categoricas para numericas é mais facil, basta criar variaveis dummies
  index.class <- which(colnames(training) == target.name);
  # quando o factor tem NA, diz que é uma nova categoria
  training.result <- dummy.data.frame(training[-index.class], sep=".");
  testing.result <- dummy.data.frame(testing[-index.class], sep=".");
  
  # checar se training e testing tem as mesmas categorias, caso falte alguma categoria
  # precisa criar a dummy variable correspondente manualmente com valor 0;
  dataset.aux <- rbind.data.frame(training, testing);
  dataset.aux <- dummy.data.frame(dataset.aux[-index.class], sep=".");
  names.in.aux.notin.training <- names(dataset.aux[which(!(names(dataset.aux) %in% names(training.result)))]);
  names.in.aux.notin.testing <- names(dataset.aux[which(!(names(dataset.aux) %in% names(testing.result)))]);
  # adiciona colunas faltando com valor 0
  if (length(names.in.aux.notin.testing) > 0)
  {
    testing.result[,names.in.aux.notin.testing] <- 0;
  }
  if (length(names.in.aux.notin.training) > 0)
  {
    training.result[,names.in.aux.notin.training] <- 0;
  }
  # reordena colunas para os dataframes ficarem iguais
  testing.result <- testing.result[names(training.result)];
  
  # recalcula indice da variavel alvo
  index.class <- which(colnames(training.result) == target.name);
  
  # OPCIONAL: remove variavel referente ao primeiro nivel do factor (nível de referencia)
  if (remove.first)
  {
    index.dummies.del <- c();
    for (i in 1:ncol(training))
    {
      if (is.factor(training[,i]))
      {
        varname <- names(training)[i];
        dummy.varname <- paste(var.name, ".", levels(training[,i])[1], sep = "");
        aux <- which(colnames(dataset.result) == dummy.varname);
        index.dummies.del <- c(index.dummies.del, i);
      }
    }
    if (length(index.dummies.del) > 0)
    {
      training.result <- training.result[,-index.dummies.del];
      testing.result <- testing.result[,-index.dummies.del];
    }    
  }  
  training.result <- cbind(a_dm = training$a_dm, training.result);
  testing.result <- cbind(a_dm = testing$a_dm, testing.result);
  result <- list(training.result, testing.result);
  return(result);
}

transform.to.numeric.train <- function(training, target.name = "a_dm", remove.first = FALSE, 
                                 categoric.names.file = "data/AnaliseVariaveis/var_categoricas_mod.txt", 
                                 numeric.names.file = "data/AnaliseVariaveis/var_numericas_mod.txt", verbose = FALSE)
{
  if (length(categoric.names.file) > 0 && length(categoric.names.file) == length(numeric.names.file))
  {
    eliminate.redundant.index <- categorical.redundant.index(names(training), categoric.names.file, numeric.names.file);
    if (length(eliminate.redundant.index) > 0)
    {
      training <- training[,-eliminate.redundant.index];
      #testing <- testing[,-eliminate.redundant.index];
    }
  }
  
  # categoricas para numericas é mais facil, basta criar variaveis dummies
  index.class <- which(colnames(training) == target.name);
  # quando o factor tem NA, diz que é uma nova categoria
  training.result <- dummy.data.frame(training[-index.class], sep=".");
  #testing.result <- dummy.data.frame(testing[-index.class], sep=".");
  
  # checar se training e testing tem as mesmas categorias, caso falte alguma categoria
  # precisa criar a dummy variable correspondente manualmente com valor 0;
  #dataset.aux <- rbind.data.frame(training, testing);
  #dataset.aux <- dummy.data.frame(dataset.aux[-index.class], sep=".");
  #names.in.aux.notin.training <- names(dataset.aux[which(!(names(dataset.aux) %in% names(training.result)))]);
  #names.in.aux.notin.testing <- names(dataset.aux[which(!(names(dataset.aux) %in% names(testing.result)))]);
  # adiciona colunas faltando com valor 0
#   if (length(names.in.aux.notin.testing) > 0)
#   {
#     testing.result[,names.in.aux.notin.testing] <- 0;
#   }
  # ATENCAO: se conjunto de teste tiver um factor que nao esta no conjunto de teste de treino, valor no conjunto de teste precisa ser tratado deve ser tratado

  # reordena colunas para os dataframes ficarem iguais
  # testing.result <- testing.result[names(training.result)];
  
  # recalcula indice da variavel alvo
  index.class <- which(colnames(training.result) == target.name);
  
  training.result <- cbind(a_dm = training$a_dm, training.result);
  # testing.result <- cbind(a_dm = testing$a_dm, testing.result);
  return(training.result);
}
transform.to.numeric.test <- function(testing, training.names, target.name = "a_dm", remove.first = FALSE, 
                                       categoric.names.file = "data/AnaliseVariaveis/var_categoricas_mod.txt", 
                                       numeric.names.file = "data/AnaliseVariaveis/var_numericas_mod.txt", verbose = FALSE)
{
  if (length(categoric.names.file) > 0 && length(categoric.names.file) == length(numeric.names.file))
  {
    eliminate.redundant.index <- categorical.redundant.index(training.names, categoric.names.file, numeric.names.file);
    if (length(eliminate.redundant.index) > 0)
    {
      #training <- training[,-eliminate.redundant.index];
      testing <- testing[,-eliminate.redundant.index];
    }
  }
  
  index.class <- which(colnames(testing) == target.name);
  # quando o factor tem NA, diz que é uma nova categoria
  #training.result <- dummy.data.frame(training[-index.class], sep=".");
  testing.result <- dummy.data.frame(testing[-index.class], sep=".");
  
  # checar se training e testing tem as mesmas categorias, caso falte alguma categoria
  # precisa criar a dummy variable correspondente manualmente com valor 0;
  names.in.train.notin.testing <- setdiff(training.names, names(testing.result));
  if (length(names.in.train.notin.testing) > 0)
  {
    testing.result[,names.in.train.notin.testing] <- 0;
  }
  # ATENCAO: se conjunto de teste tiver um factor que nao esta no conjunto de teste de treino, valor no conjunto de teste precisa ser tratado
  # remove colunas
  names.in.test.notin.train <- setdiff(names(testing.result), training.names);
  remove.idx <- which(names(testing.result) %in% names.in.test.notin.train)
  if (length(remove.idx) > 0)
  {
    testing.result <- testing.result[,-remove.idx];
  }
 
  # reordena colunas para os dataframes ficarem iguais
  testing.result <- testing.result[training.names];
  
  # recalcula indice da variavel alvo
  index.class <- which(colnames(testing.result) == target.name);
  testing.result <- cbind(a_dm = testing$a_dm, testing.result);
  return(testing.result);
}
