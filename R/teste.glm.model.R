# 
# Script contendo exemplo de como utilizar o modelo de Regressão Logística na prática.
# Pode ser facilmente traduzido para outras linguagens. 
# 
source('R/load.libraries.R');
source('R/load.perform.tests.functions.R');
load(file='output/models.artigo/model.glm');
load(file="output/test_aux/test.orig");
load(file='output/models.artigo/scale.preProc');

#attributes(scale.preProc)
#scale.preProc[["mean"]]
a_rcq.mean <- 0.8889311;
#scale.preProc[["std"]]
a_rcq.std <- 0.08615528;

# normalizacao manual
test.man <- test.orig;
test.man$a_rcq <- (test.man$a_rcq - a_rcq.mean) / a_rcq.std;

# dicotomizacao manual
rcta82        <- test.man$rcta8 == 2;
hfda071       <- test.man$hfda07 == 1;        
hfda111       <- test.man$hfda11 == 1;
diea1331      <- test.man$diea133 == 1;
diea1332      <- test.man$diea133 == 2;
a_imc22       <- test.man$a_imc2 == 2;
a_imc23       <- test.man$a_imc2 == 3;
a_imc24       <- test.man$a_imc2 == 4;
a_rcq         <- test.man$a_rcq;
a_escolar2    <- test.man$a_escolar == 2;
a_escolar3    <- test.man$a_escolar == 3
a_escolar4    <- test.man$a_escolar == 4;
a_gidade2     <- test.man$a_gidade == 2;
a_gidade3     <- test.man$a_gidade == 3;
a_gidade4     <- test.man$a_gidade == 4;
a_medanthipert1 <- test.man$a_medanthipert == 1;
a_binge1        <- test.man$a_binge == 1;
a_ativfisica2   <- test.man$a_ativfisica == 2;
a_ativfisica3   <- test.man$a_ativfisica == 3;

# aplicacao manual do modelo:
variables <- names(model.glm[["coefficients"]]); # variaveis do modelo
variables <- variables[-1]; # remove intercept
# previsao <- -1.6929 + 0.1826 * rcta82 + -0.1386 * hfda071 ....

# soma intercept com somatorio ponderado de cada entrada com o coeficiente
result.sum <- model.glm[["coefficients"]][[1]]; #intercept
for (var in variables) {
  result.sum <- result.sum + get(var) * model.glm[["coefficients"]][[var]];
}
pred.resp.man <- (1+ tanh(result.sum/2))/2; #sigmoid function
# Classificacao
pred.class.man <- ifelse(pred.resp.man > 0.11, 0, 1);
pred.class.man <- factor(pred.class.man, levels = c("1", "0"));

# VERIFICACAO
# aplicacao normal do modelo
load(file='output/models.artigo/train.numeric.names');
test <- scale.test(test.orig, scale.preProc, train.numeric.names);
pred.resp <- predict(model.glm, newdata = test, type="response");
pred.class <- ifelse(pred.resp > 0.11, 0, 1);
pred.class <- factor(pred.class, levels = c("1", "0"));
pred.resp.man[1]
pred.resp[1]
pred.resp.man[2]
pred.resp[2]

xtab <- table(pred = pred.class, truth = test$a_dm);
confusionMatrix(xtab, positive = "0");
