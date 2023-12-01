##CRIANDO GRAFOS ALEATÓRIOS
require(igraph)
pref_matrix <- cbind(c(0.8, 0.1), c(0.1, 0.7))
g2 <- sample_(sbm(n = 20, pref.matrix = pref_matrix,block.sizes = c(10, 10)))
g1 <- erdos.renyi.game(100,250,'gnm', directed=T)
plot(g2)

#MEDIDAS
#AI
GRAU_AI <- degree(matrixnetwork, normalized = FALSE, loops = FALSE)
DENSIDADE_AI <- edge_density(matrixnetwork, loops = FALSE) 
FORÇA_AI <- strength (matrixnetwork,   vids = V(matrixnetwork),   mode = c("all"),   loops = FALSE,   weights = NULL )
CENTRALIDADE_AI <- estimate_betweenness(matrixnetwork, vids = V(matrixnetwork), directed = FALSE,  weights = NULL, cutoff = -1)
GLOBAL_EF_AI <- global_efficiency(matrixnetwork, weights = NULL, directed = FALSE)
LOCAL_AI <- local_efficiency( matrixnetwork, vids = V(matrixnetwork),    weights = NULL,   directed = TRUE,   mode = c("all"))
GLOBAL_CLU_AI <- average_local_efficiency(  matrixnetwork,  weights = NULL,   directed = TRUE,   mode = c("all"))
#BI
GRAU_BI <- degree(matrixnetwork, normalized = FALSE, loops = FALSE)
DENSIDADE_BI <- edge_density(matrixnetwork, loops = FALSE) 
FORÇA_BI <- strength (matrixnetwork,   vids = V(matrixnetwork),   mode = c("all"),   loops = FALSE,   weights = NULL )
CENTRALIDADE_BI <- estimate_betweenness(matrixnetwork, vids = V(matrixnetwork), directed = FALSE,  weights = NULL, cutoff = -1)
GLOBAL_EF_BI <- global_efficiency(matrixnetwork, weights = NULL, directed = FALSE)
LOCAL_BI <- local_efficiency( matrixnetwork, vids = V(matrixnetwork),    weights = NULL,   directed = TRUE,   mode = c("all"))
GLOBAL_CLU_BI <- average_local_efficiency(  matrixnetwork,  weights = NULL,   directed = TRUE,   mode = c("all"))
#RANDOM
GRAU_R <- degree(g2, normalized = FALSE, loops = FALSE)
DENSIDADE_R <- edge_density(g2, loops = FALSE) 
FORÇA_R <- strength (g2,   vids = V(g2),   mode = c("all"),   loops = FALSE,   weights = NULL )
CENTRALIDADE_R <- estimate_betweenness(g2, vids = V(g2), directed = FALSE,  weights = NULL, cutoff = -1)
GLOBAL_EF_R <- global_efficiency(g2, weights = NULL, directed = FALSE)
LOCAL_R <- local_efficiency( g2, vids = V(g2),    weights = NULL,   directed = TRUE,   mode = c("all"))
GLOBAL_CLU_R <- average_local_efficiency(  g2,  weights = NULL,   directed = TRUE,   mode = c("all"))

##Estatisticas##
mean(degree(matrixnetwork, normalized = FALSE, loops = FALSE)) # calcula a media do grau
sd(degree(matrixnetwork, normalized = FALSE, loops = FALSE))
mean(DENSIDADE)
sd(DENSIDADE)
#NORMALIDADE
shapiro.test (FORÇA)
#AI-ACASO
wilcox.test(GRAU_AI, GRAU_R)
wilcox.test(FORÇA_AI,FORÇA_R)
wilcox.test(CENTRALIDADE_AI, CENTRALIDADE_R)
wilcox.test(LOCAL_AI,LOCAL_R)
wilcox.test(GLOBAL_CLU_AI,GLOBAL_CLU_R)


#GRAFICOS PARA MEDIDAS
hist(GRAU)

#CONDICAO (AI OR BI) X ACASO
####TESTAR POSSIBILIDADES ESTATISTICAS
###Forma 1 one sample
apply(degree_cool,  2,
      function(x) unlist(t.test(x)[c("statistic", "p.value")]))
apply(strength_coll,  2,
      function(x) unlist(t.test(x)[c("statistic", "p.value")]))
apply(centr_Betw_coll,  2,
      function(x) unlist(t.test(x)[c("statistic", "p.value")]))
apply(local_eff_cool,  2,
      function(x) unlist(t.test(x)[c("statistic", "p.value")]))
apply(global_eff_cool,  2,
      function(x) unlist(wilcox.test(x)[c("statistic", "p.value")])) #Wilcoxon fazer por que o valor ? constante
apply(localaverage_eff_cool,  2,
      function(x) unlist(wilcox.test(x)[c("statistic", "p.value")])) #Wilcoxon fazer
#T one sample forma 2

t.test(degree_cool[,1], mu = 0)
t.test(degree_cool[,2],mu = 0)
t.test(degree_cool[,3], mu = 0)
t.test(degree_cool[,4], mu = 0)
t.test(degree_cool[,5], mu = 0)
t.test(degree_cool[,6], mu = 0)
t.test(degree_cool[,7], mu = 0)
