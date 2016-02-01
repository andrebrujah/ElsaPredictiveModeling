############################################################################
###################### load.generalization.models.R ########################
#
# Script utilizado para carregar modelos previamente gerados para realizar
# teste de generalização generalização.
# Também carrega em memória os subconjuntos de variáveis e parâmetros para 
# realizar pré-processamento.
#
############################################################################

load(file="output/feature_selection_v3/subsets.list");
load(file='output/models.v6/scale.preProc');
load(file='output/models.v6/train.numeric.names');
load(file='output/models.v6/train.bin.names.lst');
load(file='output/models.v6/discretization.cm');

load(file='output/models.v6/model.c5alt');
load(file='output/models.v6/model.c5alt.disc');
load(file='output/models.v6/model.c5alt.bin');

load(file='output/models.v6/model.c5rulesalt');
load(file='output/models.v6/model.c5rulesalt.disc');
load(file='output/models.v6/model.c5rulesalt.bin');

load(file='output/models.v6/model.glm');
load(file='output/models.v6/model.glm.disc');
load(file='output/models.v6/model.glm.bin');

load(file='output/models.v6/model.knn');
load(file='output/models.v6/model.knn.disc');
load(file='output/models.v6/model.knn.bin');

load(file='output/models.v6/model.nb');
load(file='output/models.v6/model.nb.disc');
load(file='output/models.v6/model.nb.bin');

load(file='output/models.v6/model.nbalt');
load(file='output/models.v6/model.nbalt.disc');
load(file='output/models.v6/model.nbalt.bin');

load(file='output/models.v6/model.nnet');
load(file='output/models.v6/model.nnet.disc');
load(file='output/models.v6/model.nnet.bin');

load(file='output/models.v6/model.rf');
load(file='output/models.v6/model.rf.disc');
load(file='output/models.v6/model.rf.bin');