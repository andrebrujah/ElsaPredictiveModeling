############################################################################
################# load.perform.tests.functions.R ##########################
#
# Script para carregamento em memória dos scripts contendo as funções
# utilizadas na geração e avaliação dos modelos preditivos.
#
############################################################################

source('R/functions/import.data.R');
source('R/functions/remove.high.na.variables.R');
source('R/functions/remove.variables.in.folder.R');
source('R/functions/eliminate.numerical.categorical.redundants.R');
source('R/functions/pre.process.functions.R');

source('R/functions/perform.tests.glm.R');
source('R/functions/perform.tests.nnet.R');
source('R/functions/perform.tests.nb.R');
source('R/functions/perform.tests.rf.R');
source('R/functions/perform.tests.knn.R');

source('R/functions/perform.tests.all.R');

