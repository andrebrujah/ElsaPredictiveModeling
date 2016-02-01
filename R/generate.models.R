############################################################################
########################### generate.models.R ##############################
#
# Script utilizado para criação dos modelos utilizados no teste de 
# generalização. Os modelos são salvos em disco ao final. 
# A validação desses modelos é realizada em outro script.
#
############################################################################
rm(list=ls());
source('R/load.libraries.R');
source('R/load.perform.tests.functions.R');

dataset.orig <- import.data(foldertoremove="data/var/vars_to_remove/");
dataset.orig$a_dm <- factor(dataset.orig$a_dm, levels = c("1", "0")); # Positive class = '0'

### seleciona manualmente variavels
dataset.orig <- subset.var.from.file(dataset.orig, var.filename = "data/AnaliseVariaveis/fatores_literatura/subconjuntos/sem_laboratoriais.txt");
# Para primeira etapa dos experimentos vou testar somente com os dados completos e sem imputação.
## imput "0" on missing data of the following variables
na.columns <- c("a_chdlight", "a_chdhard", "a_prvdcc");
dataset.imp <- dataset.orig;
for (na.column in na.columns){
  dataset.imp[which(!complete.cases(dataset.orig[,na.column])), na.column] <- "0";
}
dataset.orig <- dataset.imp;

set.seed(666);
cv.partition  <- createDataPartition(dataset.orig$a_dm, p = 0.70, list = FALSE);
train.orig <- dataset.orig[cv.partition, ];
train.orig <- get.complete.cases(train.orig);

load(file="output/feature_selection_v3/subsets.list");

# pre-process
# scale
res <- scale.train(train.orig);
train.orig <- res[[1]];
scale.preProc <- res[[2]];
train.numeric.names <- res[[3]];

save(scale.preProc, file='output/models.v6/scale.preProc');
save(train.numeric.names, file='output/models.v6/train.numeric.names');

# discretizacao
res.disc <- discretize.train(train.orig);
train.orig.disc <- res.disc[[1]];
discretization.cm <- res.disc[[2]];
save(discretization.cm, file='output/models.v6/discretization.cm');

model.c5alt <- list();
model.c5alt.disc <- list(); 
model.c5alt.bin <- list();

model.c5rulesalt <- list();
model.c5rulesalt.disc <- list(); 
model.c5rulesalt.bin <- list();

model.glm <- list();
model.glm.disc <- list(); 
model.glm.bin <- list();

model.knn <- list();
model.knn.disc <- list(); 
model.knn.bin <- list();

model.nb <- list();
model.nb.disc <- list(); 
model.nb.bin <- list();

model.nbalt <- list();
model.nbalt.disc <- list(); 
model.nbalt.bin <- list();

model.nnet <- list();
model.nnet.disc <- list(); 
model.nnet.bin <- list();

model.rf <- list();
model.rf.disc <- list(); 
model.rf.bin <- list();

# a binarizacao tem que ser feita dentro do laço, pois será diferente para cada subset
# assim, vai ter um parametro para cada subset
train.bin.names.lst <- list();
# train.bin.imp.names.lst <- list();
for (subset.idx in c(1:5, 7)) {
  print(subset.idx);
  formula <- as.simple.formula(subsets.list[[subset.idx]], "a_dm");
  subset <- c("a_dm", subsets.list[[subset.idx]]);
  subset.disc <- intersect(subset, names(train.orig.disc));
  formula.disc <- as.simple.formula(intersect(subsets.list[[subset.idx]], subset.disc),"a_dm");
  train <- train.orig[,subset];
  train.disc <- train.orig.disc[,subset.disc];
  
  # binarizacao
  train.bin <- transform.to.numeric.train(train);
  train.bin.names.lst[[subset.idx]] <- names(train.bin);
  # imputacao + binarizacao
#   train.bin.imp <- transform.to.numeric.train(train.imp);
#   train.bin.imp.names.lst[[subset.idx]] <-names(train.bin.imp);
  
  # completar dados para algoritmos RF e KNN 
  # tem que ser feito dentro do laço para poder selecionar as variaveis antes de pegar os casos completos
  train.comp <- train[complete.cases(train), ];
  train.disc.comp <- train.disc[complete.cases(train.disc), ];
  train.bin.comp <- train.bin[complete.cases(train.bin), ];
  
  # C5 ALT
  # C5 (Class)	Discretização	costs = 8; minCases = 230; fuzzyThreshold = 1; earlyStopping = 1; winnow = 0; noGlobalPruning = 1
  best.fuzzyThreshold <- TRUE;
  best.earlyStopping <- TRUE;
  best.winnow <- FALSE;
  best.noGlobalPruning <- TRUE;
  best.minCases <- 230;
  best.misclassification.cost <- 8;
  costs <- matrix(c(
    NA, 1,
    best.misclassification.cost, NA    
  ), 2, 2, byrow=TRUE);
  rownames(costs) <- colnames(costs) <- c("0", "1");
  model.c5alt[[subset.idx]]           <- C5.0(formula, data = train, trials = 1, costs = costs, control = C5.0Control(minCases = best.minCases, fuzzyThreshold = best.fuzzyThreshold, earlyStopping = best.earlyStopping, winnow = best.winnow, noGlobalPruning = best.noGlobalPruning)); 
  model.c5alt.disc[[subset.idx]]      <- C5.0(formula.disc, data = train.disc, trials = 1, costs = costs, control = C5.0Control(minCases = best.minCases, fuzzyThreshold = best.fuzzyThreshold, earlyStopping = best.earlyStopping, winnow = best.winnow, noGlobalPruning = best.noGlobalPruning)); 
  model.c5alt.bin[[subset.idx]]       <- C5.0(a_dm ~ ., data = train.bin, trials = 1, costs = costs, control = C5.0Control(minCases = best.minCases, fuzzyThreshold = best.fuzzyThreshold, earlyStopping = best.earlyStopping, winnow = best.winnow, noGlobalPruning = best.noGlobalPruning)); 

  # C5RULES ALT
  # C5-Rules (Class)	-	costs = 11; minCases = 260; fuzzyThreshold = 0; earlyStopping = 1; winnow = 0; noGlobalPruning = 0
  best.noGlobalPruning <- FALSE;
  best.fuzzyThreshold <- FALSE;
  best.earlyStopping <- TRUE;
  best.winnow <- FALSE;
  best.minCases <- 260;
  best.misclassification.cost <-11;
  costs <- matrix(c(
    NA, 1,
    best.misclassification.cost, NA    
  ), 2, 2, byrow=TRUE);
  rownames(costs) <- colnames(costs) <- c("0", "1");
  model.c5rulesalt[[subset.idx]]          <- C5.0(formula, data = train, trials = 1, costs = costs, rules = TRUE, control = C5.0Control(minCases = best.minCases, fuzzyThreshold = best.fuzzyThreshold, earlyStopping = best.earlyStopping, winnow = best.winnow, noGlobalPruning = best.noGlobalPruning)); 
  model.c5rulesalt.disc[[subset.idx]]     <- C5.0(formula.disc, data = train.disc, trials = 1, costs = costs, rules = TRUE, control = C5.0Control(minCases = best.minCases, fuzzyThreshold = best.fuzzyThreshold, earlyStopping = best.earlyStopping, winnow = best.winnow, noGlobalPruning = best.noGlobalPruning)); 
  model.c5rulesalt.bin[[subset.idx]]     <- C5.0(a_dm ~ ., data = train.bin, trials = 1, costs = costs, rules = TRUE, control = C5.0Control(minCases = best.minCases, fuzzyThreshold = best.fuzzyThreshold, earlyStopping = best.earlyStopping, winnow = best.winnow, noGlobalPruning = best.noGlobalPruning)); 
  
  # GLM
  # Regressão Logística	-	maxit = 5; epsilon = 0,01; cutoff = 0,11
  best.epsilon <- 0.01;
  best.maxit <- 5;
  model.glm[[subset.idx]]           <- glm(formula, family=binomial(link="logit"), data=train, control=list(epsilon = best.epsilon, maxit = best.maxit)); 
  model.glm.disc[[subset.idx]]      <- glm(formula.disc, family=binomial(link="logit"), data=train.disc, control=list(epsilon = best.epsilon, maxit = best.maxit)); 
  model.glm.bin[[subset.idx]]       <- glm(a_dm ~ ., family=binomial(link="logit"), data=train.bin, control=list(epsilon = best.epsilon, maxit = best.maxit)); 

  # KNN
  # K-NN	-	min.votes = 1; neighbor = 495; cutoff = 0,1
  best.neighbor <- 495;
  best.min.votes <- 1;
  model.knn[[subset.idx]]           <- train.comp;
  model.knn.disc[[subset.idx]]      <- train.disc.comp;
  model.knn.bin[[subset.idx]]       <- train.bin.comp;

  # NB
  # Naive Bayes	Discretização	laplace = 1e-05; cutoff = 0,08
  best.laplace <- 1E-5; 
  model.nb[[subset.idx]] <- naiveBayes(formula, data = train, laplace = best.laplace);
  model.nb.disc[[subset.idx]] <- naiveBayes(formula.disc, data = train.disc, laplace = best.laplace);
  model.nb.bin[[subset.idx]] <- naiveBayes(a_dm ~ ., data = train.bin, laplace = best.laplace);

  # NB-ALT
  # Naive Bayes (Class)	-	laplace = 0,1
  best.laplace <- 0.1;
  model.nbalt[[subset.idx]] <- naiveBayes(formula, data = train, laplace = best.laplace);
  model.nbalt.disc[[subset.idx]] <- naiveBayes(formula.disc, data = train.disc, laplace = best.laplace);
  model.nbalt.bin[[subset.idx]] <- naiveBayes(a_dm ~ ., data = train.bin, laplace = best.laplace);

  # NNET
  # Redes Neurais	-	size = 0; decay = 2; skip = 1; cutoff = 0,11
  best.size <- 0;
  best.skip <- TRUE;
  best.decay <- 2;
  model.nnet[[subset.idx]] <- nnet(formula, data = train, size=best.size, skip = best.skip, decay = best.decay, MaxNWts=70000, maxit = 500, trace=FALSE);  
  model.nnet.disc[[subset.idx]] <- nnet(formula.disc, data = train.disc, size=best.size, skip = best.skip, decay = best.decay, MaxNWts=70000, maxit = 500, trace=FALSE);  
  model.nnet.bin[[subset.idx]] <- nnet(a_dm ~ ., data = train.bin, size=best.size, skip = best.skip, decay = best.decay, MaxNWts=70000, maxit = 500, trace=FALSE);  

  # RF
  # Random Forest	Binarização	ntree = 2250; cutoff = 0,13
  best.ntree <- 2250;
  model.rf[[subset.idx]] <- randomForest(formula, data = train.comp, ntree=best.ntree); 
  model.rf.disc[[subset.idx]] <- randomForest(formula.disc, data = train.disc.comp, ntree=best.ntree); 
  model.rf.bin[[subset.idx]] <- randomForest(a_dm ~ ., data = train.bin.comp, ntree=best.ntree); 
}
save(train.bin.names.lst, file='output/models.v6/train.bin.names.lst');


save(model.c5alt, file='output/models.v6/model.c5alt');
save(model.c5alt.disc, file='output/models.v6/model.c5alt.disc');
save(model.c5alt.bin, file='output/models.v6/model.c5alt.bin');

save(model.c5rulesalt, file='output/models.v6/model.c5rulesalt');
save(model.c5rulesalt.disc, file='output/models.v6/model.c5rulesalt.disc');
save(model.c5rulesalt.bin, file='output/models.v6/model.c5rulesalt.bin');

save(model.glm, file='output/models.v6/model.glm');
save(model.glm.disc, file='output/models.v6/model.glm.disc');
save(model.glm.bin, file='output/models.v6/model.glm.bin');

save(model.knn, file='output/models.v6/model.knn');
save(model.knn.disc, file='output/models.v6/model.knn.disc');
save(model.knn.bin, file='output/models.v6/model.knn.bin');

save(model.nb, file='output/models.v6/model.nb');
save(model.nb.disc, file='output/models.v6/model.nb.disc');
save(model.nb.bin, file='output/models.v6/model.nb.bin');

save(model.nbalt, file='output/models.v6/model.nbalt');
save(model.nbalt.disc, file='output/models.v6/model.nbalt.disc');
save(model.nbalt.bin, file='output/models.v6/model.nbalt.bin');

save(model.nnet, file='output/models.v6/model.nnet');
save(model.nnet.disc, file='output/models.v6/model.nnet.disc');
save(model.nnet.bin, file='output/models.v6/model.nnet.bin');

save(model.rf, file='output/models.v6/model.rf');
save(model.rf.disc, file='output/models.v6/model.rf.disc');
save(model.rf.bin, file='output/models.v6/model.rf.bin');


