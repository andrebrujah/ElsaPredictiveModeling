############################################################################
################# functions/remove.high.na.variables.R #####################
# 
# Funções auxiliares para tratamento de valores faltantes. Contém funções para
# remoção de casos com dados faltantes ou variáveis que contenham mais que X% 
# de dados faltantes.
#
############################################################################

count.na.each.columns <-  function (dataset, verbose=TRUE) 
{
  nrow <- nrow(dataset);
  ncol <- ncol(dataset);
  na.count <- c(rep(0,ncol));
  for (var in 1:ncol)
  {
    na.count[var] <- length(which(is.na(dataset[,var])));
  }
  
  na.count.perc <- na.count * 100 / nrow;
  return (na.count.perc);
}
count.na.columns <-  function (dataset, thres.perc, verbose=TRUE) 
{
  nrow <- nrow(dataset);
  thres.num <- nrow * thres.perc/100;
  
  # conta número de NA de cada variável
  ncol <- ncol(dataset);
  na.count <- c(rep(0,ncol));
  for (var in 1:ncol)
  {
    na.count[var] <- length(which(is.na(dataset[,var])));
  }
  
  index.to.remove <- which(na.count > thres.num);
  
  if (verbose)
  {
    cat("Total variables with more than ",thres.perc,"% of missing value is:",length(index.to.remove), "of", ncol,"\n");  
  }
  return (length(index.to.remove));
}

# receives a dataframe and return the same dataframe without the columns which NAs values counts more than a thres.perc% passing as parameter
# recebe um dataframe e retorna o mesmo dataframe sem as variáveis que possuem mais de "thres.perc"% (passado como argumento) de NAs
remove.na.columns <- function (dataset, thres.perc, verbose=TRUE)
{
  nrow <- nrow(dataset);
  thres.num <- nrow * thres.perc/100;
  
  # conta número de NAs das variáveis
  ncol <- ncol(dataset);
  na.count <- c(rep(0,ncol));
  for (var in 1:ncol)
  {
    na.count[var] <- length(which(is.na(dataset[,var])));
  }
  
  index.to.remove <- which(na.count > thres.num);
  
  if (verbose)
  {
    cat("Total variables to remove with",thres.perc,"% of threshold is:",length(index.to.remove), "of", ncol,"\n");  
  }
  
  
  if(length(index.to.remove) > 0)
  {
    dataset.cut <- dataset[,-index.to.remove];
  }
  else
  {
    dataset.cut <- dataset;
  }
  return(dataset.cut);
}

get.complete.cases <- function(dataset, verbose=TRUE)
{
  dataset.complete <- dataset[complete.cases(dataset),];
  if (verbose)
  {
    total.complete.cases <- nrow(dataset.complete);
    cat("Total of complete cases:",total.complete.cases," of ",nrow(dataset),"\n");
    cat("Total by class\n");
    summ <- summary(dataset.complete$a_dm);
    
    if (length(summ) > 0)
    {
      cat(names(summ)[1],":", summ[[1]]," (",round(summ[[1]]*100/total.complete.cases),"%)\n");
      cat(names(summ)[2],":", summ[[2]]," (",round(summ[[2]]*100/total.complete.cases),"%)\n");
    }
    else
    {
      cat("0 complete cases!");
    }
  }  
  return(dataset.complete);
}

print.complete.cases <- function (dataset.cut)
{
  # Total of variables by class:
  index.factor <- which(sapply(dataset.cut, is.factor));
  index.number <- which(sapply(dataset.cut, is.numeric));
  cat("Total of factor  variables:",length(index.factor),"\n");
  cat("Total of numeric variables:",length(index.number),"\n\n");
  dataset.cut.complete <- dataset.cut[complete.cases(dataset.cut), ];
  total.complete.cases <- nrow(dataset.cut.complete);
  cat("Total of complete cases:",total.complete.cases,"\n");
  cat("Total by class\n");
  summ <- summary(dataset.cut.complete$a_dm);
  if (length(summ) > 0)
  {
    cat(names(summ)[1],":", summ[[1]]," (",round(summ[[1]]*100/total.complete.cases),"%)\n");
    cat(names(summ)[2],":", summ[[2]]," (",round(summ[[2]]*100/total.complete.cases),"%)\n");
  }
  else
  {
    cat("0 complete cases!");
  }  
}