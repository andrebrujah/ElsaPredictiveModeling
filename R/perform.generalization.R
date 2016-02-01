############################################################################
####################### perform.generalization.R ###########################
#
# Script utilizado para realizar no teste de generalização. Ele avalia os 
# modelos gerados anteriormente no conjunto independente de testes.
#
############################################################################

rm(list=ls());
source('R/load.libraries.R');
source('R/load.perform.tests.functions.R');
source('R/load.generalization.models.R');

dataset.orig <- import.data(foldertoremove="data/var/vars_to_remove/");
dataset.orig$a_dm <- factor(dataset.orig$a_dm, levels = c("1", "0")); # Positive class = '0'
dataset.orig <- subset.var.from.file(dataset.orig, var.filename = "data/AnaliseVariaveis/fatores_literatura/subconjuntos/sem_laboratoriais.txt");
na.columns <- c("a_chdlight", "a_chdhard", "a_prvdcc");
dataset.imp <- dataset.orig;
for (na.column in na.columns){
  dataset.imp[which(!complete.cases(dataset.orig[,na.column])), na.column] <- "0";
}
dataset.orig <- dataset.imp;

set.seed(666);
cv.partition  <- createDataPartition(dataset.orig$a_dm, p = 0.70, list = FALSE);
test.orig <- dataset.orig[-cv.partition, ];
test.orig <- get.complete.cases(test.orig);

load(file="output/feature_selection_v3/subsets.list"); # subsets.list.unique com 8 subsets de variaveis

# pre-process
# scale
test <- scale.test(test.orig, scale.preProc, train.numeric.names);

# binarizacao - é feito dentro do laco, existe um train.bin.names para cada subset.idx
load(file='output/models.v6/train.bin.names.lst');

# discretizacao 
load(file='output/models.v6/discretization.cm');
test.disc <- discretize.test(testing = test, discretization.cm);
tst.id <- "tst";
model.id <- "normal";
model.id.disc <- "disc";
model.id.bin <- "bin";

model.id.list <- c("normal", "disc", "bin");
save(model.id.list, file = "output/models.v6/model.id.list");
for (subset.idx in c(1:5, 7)) {
  train.bin.names <- train.bin.names.lst[[subset.idx]];
  test.bin <- transform.to.numeric.test(test, train.bin.names);
  verbose = TRUE;
  
  generalization.predict.c5alt(model.c5alt[[subset.idx]], test, tst.id, model.id, subset.idx);
  generalization.predict.c5alt(model.c5alt.disc[[subset.idx]], test.disc, tst.id, model.id.disc, subset.idx);
  generalization.predict.c5alt(model.c5alt.bin[[subset.idx]], test.bin, tst.id, model.id.bin, subset.idx);

  generalization.predict.c5rulesalt(model.c5rulesalt[[subset.idx]], test, tst.id, model.id, subset.idx);
  generalization.predict.c5rulesalt(model.c5rulesalt.disc[[subset.idx]], test.disc, tst.id, model.id.disc, subset.idx);
  generalization.predict.c5rulesalt(model.c5rulesalt.bin[[subset.idx]], test.bin, tst.id, model.id.bin, subset.idx);

  generalization.predict.glm(model.glm[[subset.idx]], test, tst.id, model.id, subset.idx);
  generalization.predict.glm(model.glm.disc[[subset.idx]], test.disc, tst.id, model.id.disc, subset.idx);
  generalization.predict.glm(model.glm.bin[[subset.idx]], test.bin, tst.id, model.id.bin, subset.idx);

  tryCatch({generalization.predict.knn(model.knn[[subset.idx]], test, tst.id, model.id, subset.idx);}, error = function(e) {print(e);});  
  tryCatch({generalization.predict.knn(model.knn.disc[[subset.idx]], test.disc, tst.id, model.id.disc, subset.idx);}, error = function(e) {print(e);}); 
  tryCatch({generalization.predict.knn(model.knn.bin[[subset.idx]], test.bin, tst.id, model.id.bin, subset.idx);}, error = function(e) {print(e);}); 

  generalization.predict.nb(model.nb[[subset.idx]], test, tst.id, model.id, subset.idx);
  generalization.predict.nb(model.nb.disc[[subset.idx]], test.disc, tst.id, model.id.disc, subset.idx);
  generalization.predict.nb(model.nb.bin[[subset.idx]], test.bin, tst.id, model.id.bin, subset.idx);

  generalization.predict.nbalt(model.nbalt[[subset.idx]], test, tst.id, model.id, subset.idx);
  generalization.predict.nbalt(model.nbalt.disc[[subset.idx]], test.disc, tst.id, model.id.disc, subset.idx);
  generalization.predict.nbalt(model.nbalt.bin[[subset.idx]], test.bin, tst.id, model.id.bin, subset.idx);

  generalization.predict.nnet(model.nnet[[subset.idx]], test, tst.id, model.id, subset.idx);
  generalization.predict.nnet(model.nnet.disc[[subset.idx]], test.disc, tst.id, model.id.disc, subset.idx);
  generalization.predict.nnet(model.nnet.bin[[subset.idx]], test.bin, tst.id, model.id.bin, subset.idx);

  generalization.predict.rf(model.rf[[subset.idx]], test, tst.id, model.id, subset.idx);
  generalization.predict.rf(model.rf.disc[[subset.idx]], test.disc, tst.id, model.id.disc, subset.idx);
  generalization.predict.rf(model.rf.bin[[subset.idx]], test.bin, tst.id, model.id.bin, subset.idx);
}




