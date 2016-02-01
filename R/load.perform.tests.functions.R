############################################################################
################# load.perform.tests.functions.R ##########################
#
# Script para carregamento em memória dos scripts contendo as funções
# utilizadas na geração e avaliação dos modelos preditivos.
#
############################################################################

# scripts de pre-processamento
source('R/functions/remove.high.na.variables.R');
source('R/functions/remove.variables.in.folder.R');
source('R/functions/import.data.R');
source('R/functions/eliminate.numerical.categorical.redundants.R');
source('R/functions/pre.process.functions.R');

# carrega scripts que geram e avaliam os modelos
source('R/functions/perform.tests.glm.R');
source('R/functions/perform.tests.nnet.R');
source('R/functions/perform.tests.nb.R');
source('R/functions/perform.tests.rf.R');
source('R/functions/perform.tests.knn.R');

# a versao alt nao faz a classificacao através de análise ROC e cutoffs, mas sim diretamente pelo algoritmo
source('R/functions/perform.tests.c5alt.R');
source('R/functions/perform.tests.c5rulesalt.R');
source('R/functions/perform.tests.nbalt.R');

# script que chama todos outros testes
source('R/functions/perform.tests.all.R');

