# No.PubliELSA/Requisição: 	14_0173
# Titulo da Proposta:       Comparison of data mining techniques for prediction of undiagnosed diabetes
# Finalidade do programa:   Etapa de Seleção automática de variáveis: Geração dos melhores (melhor média de AUC na validação cruzada)
#                           subconjuntos de variáveis. Validação cruzada de k=10 folds repetida por n=3 vezes. 
#                           Estratégia de busca = Forward Search.
# Nº do programa:           analise03.R
# Autor:                    André Rodrigues Olivera
# Centro:                   ELSA-Brasil
# Data:                     01/06/2016
# Comentários:              Antes desta etapa, os dados já foram importados, particionados e pré-processados. Os dados utilizados
#                           são os mesmos da etapa de afinação de parâmetros. 
#                           A definição dos algoritmos e parâmetros estão no arquivo: 'R/functions/fs.evaluators.v2.R'

rm(list=ls());
source('R/load.libraries.R');
source('R/load.perform.tests.functions.R');
source('R/functions/fs.evaluators.v2.R');

load(file="output/subset_orig");

n <- 3;
k <- 10;
training.only.complete = TRUE;
testing.only.complete = TRUE;
ds.id <- "tuning.ds";

algorithms <- c("glm", "nb", "knn", "nnet");
df.resultado <- data.frame(algorithm = character(), auc = numeric(), subset = character());
idx <- 1;
for (algorithm in algorithms) {
  if (algorithm == "nb") {
    transformation <- "factor";
  } else {
    transformation <- c();
  }   
  fs <- forward.search(subset_orig, evaluator);
  fname <- paste("output/feature_selection_artigo/fs.", algorithm, sep = "");
  save(fs, file=fname);
  idx <- idx + 1;
  best.auc.mean <- evaluator(fs);
  df.resultado <- rbind(df.resultado, data.frame(algorithm, best.auc.mean, paste(sort(fs), collapse=" ")));
}
write.csv(file = "output/feature_selection_artigo/resultado.csv", df.resultado);
