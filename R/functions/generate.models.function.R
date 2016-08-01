############################################################################
####################### generate.models.function.R #########################
#
# Método utilizado para criação dos modelos utilizados no teste de 
# generalização. Os modelos são salvos em disco ao final. 
# A validação desses modelos é realizada em outro script.
#
############################################################################

generate.models <- function (train.orig) {

  #load subsets:
  load(file="output/feature_selection_artigo/fs.glm");
  fs.glm <- fs;
  
  load(file="output/feature_selection_artigo/fs.nb");
  fs.nb <- fs;
  
  load(file="output/feature_selection_artigo/fs.knn");
  fs.knn <- fs;
  
  load(file="output/feature_selection_artigo/fs.nnet");
  fs.nnet <- fs;
  
  load(file="output/subset_orig");
  
  # pre-process
  # scale
  res <- scale.train(train.orig);
  train.orig <- res[[1]];
  scale.preProc <- res[[2]];
  train.numeric.names <- res[[3]];
  
  save(scale.preProc, file='output/models.artigo/scale.preProc');
  save(train.numeric.names, file='output/models.artigo/train.numeric.names');
  
  # GLM
  formula <- as.simple.formula(fs.glm, "a_dm");
  subset <- c("a_dm", fs.glm);
  train <- train.orig[,subset];
  best.epsilon <- 0.01;
  model.glm <- glm(formula, family=binomial(link="logit"), data=train, control=list(epsilon = best.epsilon, maxit = 50)); 
  
  # KNN
  subset <- c("a_dm", fs.knn);
  train <- train.orig[,subset];
  # completar dados para algoritmos RF e KNN 
  train.comp <- train[complete.cases(train), ];
  best.neighbor <- 475;
  best.min.votes <- 0;
  model.knn <- train.comp;
  
  # NB
  subset.idx <- 1;
  formula <- as.simple.formula(fs.glm, "a_dm");
  subset <- c("a_dm", fs.glm);
  train <- train.orig[,subset];
  best.laplace <- 0.001; 
  model.nb <- naiveBayes(formula, data = train, laplace = best.laplace);
  
  # NNET
  formula <- as.simple.formula(fs.nnet, "a_dm");
  subset <- c("a_dm", fs.nnet);
  train <- train.orig[,subset];
  best.size <- 175;
  best.skip <- FALSE;
  best.decay <- 2;
  model.nnet <- nnet(formula, data = train, size=best.size, skip = best.skip, decay = best.decay, MaxNWts=70000, maxit = 500, trace=FALSE);  
  
  # RF
  formula <- as.simple.formula(subset_orig, "a_dm");
  subset <- c("a_dm", subset_orig);
  train <- train.orig[,subset];
  train.comp <- train[complete.cases(train), ];
  best.ntree <- 4300;
  model.rf <- randomForest(formula, data = train.comp, ntree=best.ntree); 
  
  
  save(model.glm, file='output/models.artigo/model.glm');
  save(model.knn, file='output/models.artigo/model.knn');
  save(model.nb, file='output/models.artigo/model.nb');
  save(model.nnet, file='output/models.artigo/model.nnet');
  save(model.rf, file='output/models.artigo/model.rf');
}



