#puxando a instalação dos pacotes
source("requirements.R")

#abrindo pacotes que vamos usar nesse script
library(ggplot2)
library(performance)
library(emmeans)

#carregando dados
germinacao<-read.table("germinacao.txt", h=T) 

#testar correlação entre variáveis
cor(germinacao[, c("ivg", "taxa_germinacao")], method = "pearson")
# para testar outras basta trocar as colunas escolhidas

###ANALISES
# dados normais -> ANOVA
# função para rodar ANOVA já com testes de pressupostos
anova <- function(resposta, dados = germinacao) {   #abre a função, define que parametro a ser definido é a variável resposta. tb defini pra usar o dt=f germinacao como default
  formula_str <- paste0(resposta, " ~ especie") #faz a formula para o modelo, sempre usando a coluna especie como preditora
  modelo <- aov(as.formula(formula_str), data = dados)
  
  # testa os pressupostos primeiro
  normalidade <- check_normality(modelo)
  homoced <- check_heteroscedasticity(modelo)
  
  print(normalidade)
  print(homoced)
  
  texto_normalidade <- capture.output(print(normalidade))
  texto_homoced <- capture.output(print(homoced))
  
  #faço a função parar caso os pressupostos não sejam atingidos
  normalidade_ok <- !any(grepl("Warning", texto_normalidade, ignore.case = TRUE))
  homoced_ok <- !any(grepl("Warning", texto_homoced, ignore.case = TRUE))
  
  if (!normalidade_ok || !homoced_ok) {
    message("Pressupostos violados — análise interrompida antes do summary/Tukey.")
    return(invisible(NULL))
  }
  
  cat("\n--- Summary ANOVA ---\n")
  print(summary(modelo))
  
  cat("\n--- Tukey HSD ---\n")
  print(TukeyHSD(modelo))
  
  return(invisible(modelo))
}

# dados binomiais -> GLM
# função para rodar GLMs já com testes de pressupostos
GLMs <- function (resposta, dados, familia, pesos = NULL) {
  formula_str <- paste0(resposta, " ~ especie") #faz a formula para o modelo, sempre usando a coluna especie como preditora
  modelo <- glm(as.formula(formula_str), data = dados, family = familia, weights= pesos)
  par(mfrow=c(2,2))
  plot(modelo) 
  par(mfrow = c(1, 1)) #volta a visualização ao normal depois

  cat("\n--- Summary GLM ---\n")
  print(summary(modelo))
  
  cat("\n--- Emmeans (Tukey) ---\n")
  print(emmeans(modelo, list(pairwise ~ especie), adjust = "Tukey"))
  
  return(invisible(modelo))
}

#monto também uma função pra fazer os plots
boxplot <- function(resposta, y_label, dados = germinacao, arquivo = NULL) {
  p <- ggplot(dados, aes(x = trat, y = .data[[resposta]])) +
    geom_boxplot(width = 0.8,
                 fill = "gray60",
                 outlier.size = 1.90,
                 col = "black", size = 1.05) +
    labs(y = y_label,
         x = "Species") +
    scale_x_discrete(labels = c("a_e_plana" = "E. pla", "c_repens" = "C. rep", "a_jubata" = "A. jub",
                                "a_laevis" = "A. lae", "a_lanata" = "A. lan", "p_olyroides" = "P. oly",
                                "p_plicatulum" = "P. pli"),
                     limits = c("a_e_plana", "a_jubata", "a_laevis", "a_lanata", "c_repens",
                                "p_olyroides", "p_plicatulum")) +
    theme(axis.title.x = element_text(size = 32, vjust = 0, margin = margin(t = 0, r = 0, b = 5, l = 0)),
          axis.title.y = element_text(size = 30, vjust = +1, margin = margin(t = 0, r = 15, b = 0, l = 10)),
          axis.text.y = element_text(size = 28, color = "#333333"),
          axis.text.x = element_text(size = 26, color = "#333333"))
  
  # se não for especificado um nome de arquivo, usa o nome da coluna + .png
  #if (is.null(arquivo)) {
  #  arquivo <- paste0(resposta, ".png")
  #}
  #ggsave(arquivo, plot = p, width = 280, height = 250, units = "mm", dpi = 300)
  #deixei essas linhas comentadas pq nao tenho interesse de salvar de novo os plots que ja estao feitos
  return(p)
}

## ANOVAS ##
# os dados tem que ter distribuição normal, pra isso roda o teste shapiro
# IVG (GRI)
shapiro.test(germinacao$ivg) #não normal, transformo com raiz quadrada:
shapiro.test(sqrt(germinacao$ivg)) #normal, então usamos sqrt
anova ("sqrt(ivg)")
boxplot ("ivg", "Germination Rate Index")

# biomassa foliar
biom_nativas<-na.omit(germinacao) # primeiro crio df excluindo NAs
shapiro.test(biom_nativas$folha) #não normal, opto por transformar com log:
shapiro.test(log(biom_nativas$folha)) #o mais normal das transformaçoes
anova ("log(folha)", biom_nativas)
boxplot ("folha", "Aboveground biomass of isolated species\n (g/individual)")

#biomassa radicular
shapiro.test(biom_nativas$raiz) #não normal, transformo com raiz quadrada:
shapiro.test(sqrt(biom_nativas$raiz)) 
anova ("sqrt(raiz)", biom_nativas)
boxplot ("raiz", "Radicular biomass of isolated species\n (g/individual)")


## GLMs ##
# taxa de germinacao
GLMs("taxa_germinacao", germinacao, familia=quasibinomial)
boxplot ("taxa_germinacao", "Germination rate (%)")

# taxa de sobrevivencia
# excluo linhas que tem 0 vivas
germinacao2<-filter(germinacao,vivas!=0)
GLMs("(vivas/total)", germinacao2, familia=binomial, pesos = germinacao2[["total"]])
boxplot ("taxa_sobrevivencia", "Survival rate (%)")
