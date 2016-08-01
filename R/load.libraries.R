############################################################################
############################ load.libraries.R ##############################
#
# Script utilizado para carregar pacotes R utilizados nos testes. 
# Caso pacote não esteja instalado no repositório local, ele pode ser
# instalado através do método "install.packages('nome_do_pacote')"
#
############################################################################

# importacao dos dados
library(foreign); 
# transformacao de dados
library(dummies);
library(discretization);

# tecnicas de aprendizagem de maquina
library(caret);
library(e1071); # Naive Bayes
library(class); # k-nn 
library(nnet); # Artificial Neural Network
# problema com rJava no ubuntu: http://stackoverflow.com/questions/12872699/error-unable-to-load-installed-packages-just-now
# solucao foi configurar corretamente LD_LIBRARY_PATH e depois fazer um SUDO R CMD javareconf
# export LD_LIBRARY_PATH=/usr/lib/jvm/java-6-oracle/jre/lib/amd64:/usr/lib/jvm/java-6-oracle/jre/lib/amd64/server
# sudo R CMD javareconf
library(randomForest); # Random Forest

# avaliacao de modelos
library(pROC);# roc, coords http://mkseo.pe.kr/stats/?p=790&ckattempt=1 

library(FSelector);
