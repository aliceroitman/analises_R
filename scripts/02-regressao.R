#puxando a instalação dos pacotes
source("requirements.R")

#abrindo pacotes que vamos usar nesse script
library(performance) 
library(car) 

# carregando os dados
comp<-read.table("competicao.txt", h=T) # dados completos com 0s
# adicionando colunas com valor total da biomassa (biomassa*vivas)
comp$n_folha.total <- comp$n_vivas * comp$n_folha
comp$n_raiz.total <- comp$n_vivas * comp$n_raiz
comp$i_folha.total <- comp$i_vivas * comp$i_folha
comp$i_raiz.total <- comp$i_vivas * comp$i_raiz

#### Testando correlação entre variáveis preditoras ####
# para conjunto de dados com 0s (comp)
(dados<-comp[,c(4, 5, 6, 17, 18)]) # crio um data frame só com os dados que usaremos para rodar a correlação entre eles
(cor(dados, method= "pearson")) #ivg e tx germin (0.76), folha e raiz (0.98);

#### função ####
# crio uma função já com os testes de pressupostos pra deixar o script mais limpo
regressão <- function (formula, dados){
  modelo <- lm(as.formula(formula), data = dados)
  
  #testando os pressupostos
  normalidade <- check_normality(modelo)
  heterosced <- check_heteroscedasticity(modelo)
  ncvT <- ncvTest(modelo)
  outl <- check_outliers(modelo)
  
  print(normalidade)
  print(heterosced)
  print(ncvT)
  print(outl)
  
  texto_normalidade <- capture.output(print(normalidade))
  texto_heterosced <- capture.output(print(heterosced))
  texto_ncvT <- capture.output(print(ncvT))
  texto_outl <- capture.output(print(outl))
  
  par(mfrow=c(2,2))
  plot(modelo)
  
  return(summary(modelo))
}

#### seleção dos melhores modelos ####
#foliar comp: p-valor: 0.006081, r²=0.3909
regressão ("i_folha.total~n_ivg+nativa", comp)

#radicular comp: p-valor: 0.008685, r²=0.3684
regressão ("i_raiz.total~n_ivg+nativa", comp)
