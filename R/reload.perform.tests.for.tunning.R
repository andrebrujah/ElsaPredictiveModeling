############################################################################
############### reload.perform.tests.for.tunning.R #########################
#
# Script para carregamento em memória dos scripts contendo as funções
# utilizadas na geração e avaliação dos modelos preditivos na etapa de
# afinação de parâmetros. Esses scripts sobrescrevem o método que define
# os parâmetros que serão utilizados nos testes para cada algoritmo.
#
############################################################################

source('R/functions/perform.tests.glm.params.R');
source('R/functions/perform.tests.nnet.params.R');
source('R/functions/perform.tests.nb.params.R');
source('R/functions/perform.tests.rf.params.R');
source('R/functions/perform.tests.knn.params.R');
