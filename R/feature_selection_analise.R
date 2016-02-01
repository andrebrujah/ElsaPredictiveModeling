############################################################################
###################### feature_selection_analise.R #########################
#
# Script utilizado na etapa de seleção automática de variáveis para avaliar
# os subconjuntos gerados pelos proprios algoritmos que geraram eles. 
# Salva em disco alguns relatórios para análise e gera o subconjunto union. 
# Também salva em disco a lista com os subconjuntos para ser utilizadas nas
# demais etapas.
#
############################################################################
rm(list=ls());
source('R/load.libraries.R');
source('R/load.perform.tests.functions.R');
source('R/functions/fs.evaluators.v2.R');

dataset.orig <- import.data(foldertoremove="data/var/vars_to_remove/");
dataset.orig$a_dm <- factor(dataset.orig$a_dm, levels = c("1", "0")); # Positive class = '0'
### seleciona manualmente variavels
dataset.orig <- subset.var.from.file(dataset.orig, var.filename = "data/AnaliseVariaveis/fatores_literatura/subconjuntos/sem_laboratoriais.txt");
index.class <- which(colnames(dataset.orig) == "a_dm");
varsubset.orig <- colnames(dataset.orig)[-index.class];

## Carrega subconjuntos de variáveis e salva em lista de subconjuntos
algorithms <- c("glm", "nnet", "knn", "nb");
fs.df <- data.frame(alg = character(0), type = character(0), nvar = numeric(), subset = character(0), stringsAsFactors = FALSE);
idx.fs <- 1;
idx.union <- 1;
subsets.list.union <- list();
subsets.list <- list();
idx.subset <- 1;
for (algorithm in algorithms) {
    fname <- paste("output/feature_selection_v3/fs.", algorithm, sep = "");
    load(file=fname); # fs
    subsets.list.union[[idx.union]] <- fs;
    idx.union <- idx.union + 1;

    subsets.list[[idx.subset]] <- fs;
    idx.subset <- idx.subset + 1;
}
union <- Reduce(union, subsets.list.union);
fs.df[idx.fs, 1] <- "union";
fs.df[idx.fs, 2] <- "union";
fs.df[idx.fs, 3] <- length(union);
fs.df[idx.fs, 4] <- paste(sort(union), collapse=" ");  
idx.fs <- idx.fs + 1;

subsets.list[[idx.subset]] <- union;
idx.subset <- idx.subset + 1;

fs.df[idx.fs, 1] <- "original";
fs.df[idx.fs, 2] <- "original";
fs.df[idx.fs, 3] <- length(varsubset.orig);
fs.df[idx.fs, 4] <- paste(sort(varsubset.orig), collapse=" "); 
idx.fs <- idx.fs + 1;

subsets.list[[idx.subset]] <- varsubset.orig;
idx.subset <- idx.subset + 1;

write.csv(fs.df, file="output/feature_selection_v3/feature.selection.subsets.semrf.csv");
save(subsets.list, file="output/feature_selection_v3/subsets.list");

### valida conjuntos criados em todos algoritmos

n <- 3;
k <- 10;
training.only.complete = TRUE;
testing.only.complete = TRUE;
ds.id <- "tuning.ds";

algorithms <- c("glm", "nnet", "knn", "nb");
results.df <- data.frame(alg.tst = character(0), alg.fs = character(0), fs.type = character(0), auc = numeric(0), stringsAsFactors = FALSE);
idx.fs <- 1;
for (algorithm.test in algorithms) {
  for (algorithm.fs in algorithms) {
    if (algorithm.test == "nb") {
      transformation <- "factor";
    } else {
      transformation <- c();
    }

    fname <- paste("output/feature_selection_v3/fs.", algorithm.fs, sep = "");
    load(file=fname); # fs
    algorithm <- algorithm.test;
    auc <- evaluator(fs);
    results.df[idx.fs, 1] <- algorithm.test;
    results.df[idx.fs, 2] <- algorithm.fs;
    results.df[idx.fs, 3] <- "fs";
    results.df[idx.fs, 4] <- auc;
    idx.fs <- idx.fs + 1;
  }
  algorithm <- algorithm.test;
  auc <- evaluator(union);
  results.df[idx.fs, 1] <- algorithm.test;
  results.df[idx.fs, 2] <- "union";
  results.df[idx.fs, 3] <- "union";
  results.df[idx.fs, 4] <- auc;
  idx.fs <- idx.fs + 1;
  
  algorithm <- algorithm.test;
  auc <- evaluator(varsubset.orig);
  results.df[idx.fs, 1] <- algorithm.test;
  results.df[idx.fs, 2] <- "original";
  results.df[idx.fs, 3] <- "original";
  results.df[idx.fs, 4] <- auc;
  idx.fs <- idx.fs + 1;
}
write.csv(results.df, file="output/feature_selection_v3/feature.selection.results.semrf.csv");

