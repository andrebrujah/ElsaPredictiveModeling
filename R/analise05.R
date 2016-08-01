# No.PubliELSA/Requisição: 	14_0173
# Titulo da Proposta:       Comparison of data mining techniques for prediction of undiagnosed diabetes
# Finalidade do programa:   Etapa de teste de generalização: Nesta etapa são criados modelos utilizando todo conjunto de 
#                           validação cruzada para treinamento e um conjunto de testes nao utilizado até então para avaliar 
#                           o desempenho de cada algoritmo configurado com os resultados obtidos anteriormente com dados 
#                           totalmente novos. 
# Nº do programa:           analise05.R
# Autor:                    André Rodrigues Olivera
# Centro:                   ELSA-Brasil
# Data:                     01/06/2016
# Comentários:              Os parâmetros dos algoritmos, subconjunto de variáveis e transformações utilizadas para geração
#                           dos modelos foram descobertos nas etapas anteriores e são definidos manualmente no método "generate.models"
#                           A avaliação dos modelos e cálculo das métricas são feitas no método "perform.generalization". 
#                           É preciso configurar o melhor ponto de corte para a classificação.

rm(list=ls());
source('R/load.libraries.R');
source('R/load.perform.tests.functions.R');
source('R/functions/generate.models.function.R');
source('R/functions/perform.generalization.function.R');

dataset.orig <- import.data(foldertoremove="data/var/vars_to_remove/");
dataset.orig$a_dm <- factor(dataset.orig$a_dm, levels = c("1", "0")); # Positive class = '0'

### seleciona manualmente variavels
dataset.orig <- subset.var.from.file(dataset.orig, var.filename = "data/AnaliseVariaveis/fatores_literatura/subconjuntos/sem_laboratoriais.txt");

# deve ser mesmo seed e configuracoes utilizados na geração dos datasets para as validações cruzadas
set.seed(666);
cv.partition  <- createDataPartition(dataset.orig$a_dm, p = 0.70, list = FALSE);
train.orig <- dataset.orig[cv.partition, ];
train.orig <- get.complete.cases(train.orig);

test.orig <- dataset.orig[-cv.partition, ];
test.orig <- get.complete.cases(test.orig);
save(test.orig, file="output/test_aux/test.orig");

generate.models(train.orig);
perform.generalization(test.orig);

load(file='output/models.artigo/model.glm');
sink("output/models.artigo/model.glm.txt");
print(model.glm);
sink();

