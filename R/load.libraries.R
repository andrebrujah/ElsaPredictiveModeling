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
library(e1071);
library(C50);
library(class); # k-nn 
library(nnet);
library(RWeka); # classification rules
library(randomForest);

# avaliacao de modelos
library(pROC);# roc, coords http://mkseo.pe.kr/stats/?p=790&ckattempt=1 

library(FSelector);
