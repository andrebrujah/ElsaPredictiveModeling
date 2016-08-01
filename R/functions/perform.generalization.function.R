############################################################################
################### perform.generalization.function.R ######################
#
# Método utilizado para realizar no teste de generalização. Ele avalia os 
# modelos gerados anteriormente no conjunto independente de testes.
#
############################################################################

perform.generalization <- function (test.orig)
{
  # pre-process
  # scale
  load(file='output/models.artigo/scale.preProc');
  load(file='output/models.artigo/train.numeric.names');
  test <- scale.test(test.orig, scale.preProc, train.numeric.names);
  
  tst.id <- "tst";
  model.id <- "normal";

  load(file='output/models.artigo/model.glm');
  load(file='output/models.artigo/model.knn');
  load(file='output/models.artigo/model.nb');
  load(file='output/models.artigo/model.nnet');
  load(file='output/models.artigo/model.rf');
  
  verbose = TRUE;
  generalization.predict.glm(model.glm, test, tst.id, model.id, "fs.lr");
  tryCatch({generalization.predict.knn(model.knn, test, tst.id, model.id, "fs.knn");}, error = function(e) {print(e);});  
  generalization.predict.nb(model.nb, test, tst.id, model.id, "fs.lr");
  generalization.predict.nnet(model.nnet, test, tst.id, model.id, "fs.ann");
  generalization.predict.rf(model.rf, test, tst.id, model.id, "original" );
  
}





