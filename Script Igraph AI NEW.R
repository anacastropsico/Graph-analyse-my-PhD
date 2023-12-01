## Abrir arquivo  com dados de ?reas ## PS: DEIXAR NO BANCO APENAS OS DADOS DAS ?REAS
library(haven) #Pacote que abre arquivos formato SPSS
require(corrplot) #pacote para correla??o
require(readxl)
require(haven)
#pacote para abrir arquivo em formato notas
aibi_imuno_AI_2<- read_sav("E:/Ana Paula/Doutorado PPGNeC 2021-2024/2023/PAPER AIBI IEG'S/aibi imuno AI norm 2.sav") #edite pra o lugar que o arquivo est?
View(aibi_imuno_AI_2)

##CORRELA??O ##
summary(aibi_imuno_AI_2) #Algumas estatisticas bases da ?rea
matcor <- cor(aibi_imuno_AI_2, method = "spearman") #Faz a correla??o
print(matcor, digits = 2) #Exibe as correla??es
testRes = cor.mtest(aibi_imuno_AI_2, method = "spearman") #Colocar o n?vel de significancia pra as correa??es
## GR?FICO CORRELA??O ##
col <- colorRampPalette(c("#4477AA","#77AADD", "#FFFFFF", "#EE9988","#BB4444")) #Atribui cores ao gr?fico
corrplot(matcor, p.mat = testRes$p, method="color", sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
         insig = 'label_sig', pch.col = 'white',col=col(200), diag=FALSE,  tl.col = "black")
mean(matcor)

#############################################################################################
#==================================================#
#================Gerando o modelo==================#
#==================================================#
##Abrindo arquivo ##
AI <- read.csv("E:/Ana Paula/Doutorado PPGNeC 2021-2024/2023/PAPER AIBI IEG'S/R/dados/AI_2.txt", row.names=1, sep="")
View(AI) #Abrir arquivo
## Gerando a matriz##
require(igraph) #Puxa o pacote
links= as.matrix(AI) #Transforma o banco numa matriz pass?vel de ser lida

matrixnetwork = graph.adjacency(links, mode="undirected", weighted = TRUE, add.colnames=NULL, diag=FALSE)
plot(matrixnetwork)
plot(matrixnetwork, edge.label=round(E(matrixnetwork)$weight, 3))
#MODE:?especifica como igraph deve interpretar a matriz fornecida
#Directed: O grafo ser direcionado e um elemento da matriz dar os pesos das arestas
#Undirected: Primeiro verificamos se a matriz é simetrica. a um erro se não. Entao, apenas o superior e usado para criar um grafico n?o direcionado ponderado.
#WEIGHTED: Este argumento especifica se um grafico ponderado deve ser criado a partir de uma matriz de adjac?ncia
#TRUE: um grafo ponderado ? criado e o nome do atributo de aresta ser? weight
#NULL: Um grafo n??o ponderado ? criado e os elementos da matriz de adjac?ncia fornecem o n?mero de arestas entre os v?rtices
#diag=FALSE : Retira escala vertical da analise
E(matrixnetwork) #Taman?o da rede por links
V(matrixnetwork) #Tamanho da rede por nodes

### MEDIDAS DE CENTRALIDADE  ###

# GRAU #
degree(matrixnetwork, normalized = FALSE, loops = FALSE)
mean(degree(matrixnetwork, normalized = FALSE, loops = FALSE)) # calcula a media do grau
sd(degree(matrixnetwork, normalized = FALSE, loops = FALSE))

# Betweenness centrality #
centr_betw(matrixnetwork, directed = FALSE, normalized = TRUE)$res
mean(centr_betw(matrixnetwork, directed = FALSE, normalized = TRUE)$res)
sd(centr_betw(matrixnetwork, directed = FALSE, normalized = TRUE)$res)
estimate_betweenness(matrixnetwork, directed = FALSE,  weights =NULL,  cutoff = -1) #O vértice e a aresta intermediária são (aproximadamente) definidos pelo número de geodésicas (caminhos mais curtos) passando por um vértice ou aresta

#Strength
strength (matrixnetwork,   vids = V(matrixnetwork),   mode = c("all"),   loops = FALSE,   weights = NULL )
mean(strength (matrixnetwork,   vids = V(matrixnetwork),   mode = c("all"),   loops = FALSE,   weights = NULL )
)
sd(strength (matrixnetwork,   vids = V(matrixnetwork),   mode = c("all"),   loops = FALSE,   weights = NULL )
)
###########EFICIENCYA######### Essas funções calculam a eficiência local global ou média de uma rede ou a eficiência local de cada vértice da rede. Veja abaixo as definições.

global_efficiency(matrixnetwork, weights = TRUE, directed = FALSE)
local_efficiency( matrixnetwork, vids = V(matrixnetwork),    weights = NULL,   directed = TRUE,   mode = c("all"))
average_local_efficiency(  matrixnetwork,  weights = NULL,   directed = TRUE,   mode = c("all"))

####### DETEC??O DE CLUSTER  ########
# Decteção de cluster com base em edge_betweennes
ceb <-cluster_edge_betweenness(matrixnetwork, weights = NULL, directed = FALSE,   edge.betweenness = TRUE,
                               merges = TRUE,  bridges = TRUE, modularity = TRUE,  membership = TRUE ) #Calcula a comunidade parti??o e cria

modularity(ceb)
membership(ceb)

#Detec??o com base em learding
ceb <-cluster_leading_eigen( matrixnetwork, weights = NULL) #apenas undirected                       
#Comunidade a partir otimizacao de motif
ceb <- cluster_louvain(matrixnetwork, weights = NULL, resolution = 1) #apenas undirected

dendPlot(ceb, mode = "dendrogram") ###Ver argumentos depois sobre dendoplot e louvain
communities(ceb)

## AJUSTES GR?FICO ##
vertex.size=degree(matrixnetwork) *8.2 #Coloca Degree no tamanho das bolinhas 

###Cores Centralidade Normalizada### Para insers?o de centr_betw como avaliador de cada ?rea
centralidade<-centr_betw(matrixnetwork, directed = FALSE, normalized = FALSE) # Calculate eigen centrality and check the distribution We're attaching the
# result of eigen_centrality() straight onto the vertices as verticy-attributes
V(matrixnetwork)$eb <-centralidade$res  #$vector hist(matrixnetwork$eb) #atribui propriedade Ao vertex
# You could use the scales package, or define this normalisation function:
normalize <- function(x){(x-min(x))/(max(x)-min(x))} #Normaliza
(V(matrixnetwork)$eb_index <- round(normalize(V(matrixnetwork)$eb) * 90) + 10) #Coloca em escala de 100%
#eb_index should now be a category between 1 and 100 for your centralities
# Build a color-mapping with 10 categories and set the color of each
# node to the corresponding color of each centrality-measure category
V(matrixnetwork)$color <- colorRampPalette(c("#4477AA","#BB4444"))(100)[V(matrixnetwork)$eb_index]
# Look at what we did
table(V(matrixnetwork)$color)
plot(matrixnetwork, vertex.label=NA, vertex.size=20) #plotmais simples

###color bar
color.bar <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), title='') {
  scale = (length(lut)-1)/(max-min)
  
  plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
  axis(2, ticks, las=1)
  for (i in 1:(length(lut)-1)) {
    y = (i-1)/scale + min
    rect(0,y,10,y+1/scale, col=lut[i], border=NA)
  }
}
color.bar(colorRampPalette(c("#4477AA","#BB4444"))(100), 0, 100)

layout(matrix(c(1,2), nrow=1), widths=c(2,1))
plot(matrixnetwork)
color.bar(colorRampPalette(c("#4477AA","#BB4444"))(100), 0, 100)
## ATRIBUI??O DE PESOS AS EDGES DO GR?FICO ###

E(matrixnetwork)$width <- ifelse(E(matrixnetwork)$weight >= 0.9, 5,  # width 3 for weight >= 0.9
                                 ifelse(E(matrixnetwork)$weight >= 0.8, 3,  # width 2 for weight >= 0.8
                                        ifelse(E(matrixnetwork)$weight >= 0.7, 1,  # width 1 for weight >= 0.7
                                               0)))  # Default width (0) for other cases

# Plot the network to visualize the edge widths
plot(matrixnetwork)


##plot editavel 
tkplot( matrixnetwork, layout=layout_in_circle, vertex.size=degree(matrixnetwork) *6.2, 
        vertex.label.color="black", 
                vertex.label.degreen="-pi/2", vertex.label.dist = 2, edge.arrow.width=NA,
        vertex.frame.color=NA, edge.arrow.width=NA) #abrir aqui para editar expessura das arestas
#plot final
plot( ceb, matrixnetwork, layout=layout_in_circle, vertex.size=degree(matrixnetwork) *6.2, 
      vertex.label.color="black", vertex.label.degreen="-pi/2", vertex.label.dist = 5, edge.arrow.width=NA,
      vertex.frame.color=NA,V(matrixnetwork)$color <- colorRampPalette(c("#4477AA","#BB4444"))(100)[V(matrixnetwork)$eb_index])

# ceb acrescentar CEB se quiser ve  cluster dentro do gr?fico, PS: quando colocar inserir comando de cores: edge.width=abs(E(matrixnetwork)$weight)*8, edge.arrow.width=NA, V(matrixnetwork)$color <- colorRampPalette(c("#4477AA","#BB4444"))(100)[V(matrixnetwork)$eb_index])
edge.width=abs(E(matrixnetwork)$weight)*8, edge.arrow.width=NA, V(matrixnetwork)$color <- colorRampPalette(c("#4477AA","#BB4444"))(100)[V(matrixnetwork)$eb_index])
###########################################################################

######################
######BOOTSTRAP#######
######################
library(boot)
##BOOT 2 FORMAS##

Nperm=100
set.seed(123)
bootdegree <- lapply(seq_len(Nperm), function(x){  
  randomnet <- rewire(matrixnetwork, with=each_edge(0.5)) #rewire vertices with constant probability
  E(randomnet)$weight <- sample(E(matrixnetwork)$weight) #shuffle initial weights and assign them randomly to edges
  return(degree(randomnet))
})
set.seed(123)
bootstrength <- lapply(seq_len(Nperm), function(x){  
  randomnet <- rewire(matrixnetwork, with=each_edge(0.5)) #rewire vertices with constant probability
  E(randomnet)$weight <- sample(E(matrixnetwork)$weight) #shuffle initial weights and assign them randomly to edges
  return(strength(randomnet))
})
set.seed(123)
bootcentr_Betw <- lapply(seq_len(Nperm), function(x){  
  randomnet <- rewire(matrixnetwork, with=each_edge(0.5)) #rewire vertices with constant probability
  E(randomnet)$weight <- sample(E(matrixnetwork)$weight) #shuffle initial weights and assign them randomly to edges
  return(centr_betw(randomnet)$res)
})
set.seed(123)
bootlocal_efficiency <- lapply(seq_len(Nperm), function(x){  
  randomnet <- rewire(matrixnetwork, with=each_edge(0.5)) #rewire vertices with constant probability
  E(randomnet)$weight <- sample(E(matrixnetwork)$weight) #shuffle initial weights and assign them randomly to edges
  return(local_efficiency(randomnet))
})
set.seed(123)
bootlocalaverage_eff <- lapply(seq_len(Nperm), function(x){  
  randomnet <- rewire(matrixnetwork, with=each_edge(0.5)) #rewire vertices with constant probability
  E(randomnet)$weight <- sample(E(matrixnetwork)$weight) #shuffle initial weights and assign them randomly to edges
  return(average_local_efficiency(randomnet))
})
set.seed(123)
bootglobal <- lapply(seq_len(Nperm), function(x){  
  randomnet <- rewire(matrixnetwork, with=each_edge(0.5)) #rewire vertices with constant probability
  E(randomnet)$weight <- sample(E(matrixnetwork)$weight) #shuffle initial weights and assign them randomly to edges
  return(global_efficiency(randomnet))
})
# Converter em colunas

degree_cool <- do.call("rbind", bootdegree)
strength_coll <- do.call("rbind", bootstrength)
centr_Betw_coll <- do.call("rbind", bootcentr_Betw) #ver como tirar apenas o $res boot_centr_Betw[[2]]$res
global_eff_cool <- do.call("rbind", bootglobal)
local_eff_cool <- do.call("rbind", bootlocal_efficiency)
localaverage_eff_cool <- do.call("rbind", bootlocalaverage_eff)

#################
# Random network#
#################
Randon_network <- sample_gnm(7, 17)
Randon_network$name <- "Random graph, Gnm, 7, 17"
V(Randon_network)$name <- letters[1:vcount(Randon_network)]
E(Randon_network)$weight <- sample(1:5, ecount(Randon_network), replace = TRUE)
plot(Randon_network)
Randon_network <- sample_gnp (7,0.8, directed = FALSE, loops = FALSE) #Nova fun??o, aplicar essa
set.seed(123)
Rbootdegree <- lapply(seq_len(Nperm), function(x){  
  randomnet <- rewire(Randon_network, with=each_edge(0.5)) #rewire vertices with constant probability
  E(randomnet)$weight <- sample(ecount(Randon_network), replace = TRUE) #shuffle initial weights and assign them randomly to edges
  return(degree(randomnet))
})
set.seed(123)
Rbootstrength <- lapply(seq_len(Nperm), function(x){  
  randomnet <- rewire(Randon_network, with=each_edge(0.5)) #rewire vertices with constant probability
  E(randomnet)$weight <- sample(ecount(Randon_network), replace = TRUE) #shuffle initial weights and assign them randomly to edges
  return(strength(randomnet))
})
set.seed(123)
Rbootcentr_Betw <- lapply(seq_len(Nperm), function(x){  
  randomnet <- rewire(Randon_network, with=each_edge(0.5)) #rewire vertices with constant probability
  E(randomnet)$weight <- sample(ecount(Randon_network), replace = TRUE) #shuffle initial weights and assign them randomly to edges
  return(centr_betw(randomnet)$res)
})
set.seed(123)
Rbootlocal_efficiency <- lapply(seq_len(Nperm), function(x){  
  randomnet <- rewire(Randon_network, with=each_edge(0.5)) #rewire vertices with constant probability
  E(randomnet)$weight <- sample(ecount(Randon_network), replace = TRUE) #shuffle initial weights and assign them randomly to edges
  return(local_efficiency(randomnet))
})
set.seed(123)
Rbootlocalaverage_eff <- lapply(seq_len(Nperm), function(x){  
  randomnet <- rewire(Randon_network, with=each_edge(0.5)) #rewire vertices with constant probability
  E(randomnet)$weight <- sample(ecount(Randon_network), replace = TRUE) #shuffle initial weights and assign them randomly to edges
  return(average_local_efficiency(randomnet))
})
set.seed(123)
Rbootglobal <- lapply(seq_len(Nperm), function(x){  
  randomnet <- rewire(Randon_network, with=each_edge(0.5)) #rewire vertices with constant probability
  E(randomnet)$weight <- sample(ecount(Randon_network), replace = TRUE) #shuffle initial weights and assign them randomly to edges
  return(global_efficiency(randomnet))
})


# Converter em colunas
  
  Rdegree_cool <- do.call("rbind", Rbootdegree)
  Rstrength_coll <- do.call("rbind", Rbootstrength)
  Rcentr_Betw_coll <- do.call("rbind", Rbootcentr_Betw) #ver como tirar apenas o $res boot_centr_Betw[[2]]$res
  Rglobal_eff_cool <- do.call("rbind", Rbootglobal)
  Rlocal_eff_cool <- do.call("rbind", Rbootlocal_efficiency)
  Rlocalaverage_eff_cool <- do.call("rbind", Rbootlocalaverage_eff)
  
  
  ##Tcompara??o duas amostras
  
  #Degree
  t.test(degree_cool[,1], Rdegree_cool[,1])
  t.test(degree_cool[,2], Rdegree_cool[,2])
  t.test(degree_cool[,3], Rdegree_cool[,3])
  t.test(degree_cool[,4], Rdegree_cool[,4])
  t.test(degree_cool[,5], Rdegree_cool[,5])
  t.test(degree_cool[,6], Rdegree_cool[,6])
  t.test(degree_cool[,7], Rdegree_cool[,7])

#Strength
t.test(strength_coll[,1], Rstrength_coll[,1])
t.test(strength_coll[,2], Rstrength_coll[,2])
t.test(strength_coll[,3], Rstrength_coll[,3])
t.test(strength_coll[,4], Rstrength_coll[,4])
t.test(strength_coll[,5], Rstrength_coll[,5])
t.test(strength_coll[,6], Rstrength_coll[,6])
t.test(strength_coll[,7], Rstrength_coll[,7])

#Centrality #RESOLVER PROBLEMA
t.test(centr_Betw_coll[,1], Rcentr_Betw_coll[,1])
t.test(centr_Betw_coll[,2], Rcentr_Betw_coll[,2])
t.test(centr_Betw_coll[,3], Rcentr_Betw_coll[,3])
t.test(centr_Betw_coll[,4], Rcentr_Betw_coll[,4])
t.test(centr_Betw_coll[,5], Rcentr_Betw_coll[,5])
t.test(centr_Betw_coll[,6], Rcentr_Betw_coll[,6])
t.test(centr_Betw_coll[,7], Rcentr_Betw_coll[,7])

#Global Efficiency #fazer wilcoxon pois valores s?o constantes
t.test(global_eff_cool, Rglobal_eff_cool)
mean(local_eff_cool[1,])
mean(local_eff_cool[2,])
#Local Efficiency
t.test(local_eff_cool[,1], Rlocal_eff_cool[,1])
t.test(local_eff_cool[,2], Rlocal_eff_cool[,2])
t.test(local_eff_cool[,3], Rlocal_eff_cool[,3])
t.test(local_eff_cool[,4], Rlocal_eff_cool[,4])
t.test(local_eff_cool[,5], Rlocal_eff_cool[,5])
t.test(local_eff_cool[,6], Rlocal_eff_cool[,6])
t.test(local_eff_cool[,7], Rlocal_eff_cool[,7])

#Local average Efficiency # fazer wilcoxon pois valore s?o constantes
t.test(localaverage_eff_cool, Rlocalaverage_eff_cool)


######
####compara??o AI BI

#Degree
t.test(degree_cool[,1], BIdegree_cool[,1])
t.test(degree_cool[,2], BIdegree_cool[,2])
t.test(degree_cool[,3], BIdegree_cool[,3])
t.test(degree_cool[,4], BIdegree_cool[,4])
t.test(degree_cool[,5], BIdegree_cool[,5])
t.test(degree_cool[,6], BIdegree_cool[,6])
t.test(degree_cool[,7], BIdegree_cool[,7])

#Strength
t.test(strength_coll[,1], BIstrength_coll[,1])
t.test(strength_coll[,2], BIstrength_coll[,2])
t.test(strength_coll[,3], BIstrength_coll[,3])
t.test(strength_coll[,4], BIstrength_coll[,4])
t.test(strength_coll[,5], BIstrength_coll[,5])
t.test(strength_coll[,6], BIstrength_coll[,6])
t.test(strength_coll[,7], BIstrength_coll[,7])

#Centrality #RESOLVER PROBLEMA
t.test(centr_Betw_coll[,1], BIcentr_Betw_coll[,1])
t.test(centr_Betw_coll[,2], BIcentr_Betw_coll[,2])
t.test(centr_Betw_coll[,3], BIcentr_Betw_coll[,3])
t.test(centr_Betw_coll[,4], BIcentr_Betw_coll[,4])
t.test(centr_Betw_coll[,5], BIcentr_Betw_coll[,5])
t.test(centr_Betw_coll[,6], BIcentr_Betw_coll[,6])
t.test(centr_Betw_coll[,7], BIcentr_Betw_coll[,7])
#Global Efficiency #fazer wilcoxon pois valores s?o constantes
t.test(global_eff_cool, Rglobal_eff_cool)
mean(local_eff_cool[1,])
mean(local_eff_cool[2,])
#Local Efficiency
t.test(local_eff_cool[,1], BIlocal_eff_cool[,1])
t.test(local_eff_cool[,2], BIlocal_eff_cool[,2])
t.test(local_eff_cool[,3], BIlocal_eff_cool[,3])
t.test(local_eff_cool[,4], BIlocal_eff_cool[,4])
t.test(local_eff_cool[,5], BIlocal_eff_cool[,5])
t.test(local_eff_cool[,6], BIlocal_eff_cool[,6])
t.test(local_eff_cool[,7], BIlocal_eff_cool[,7])

#Local average Efficiency # fazer wilcoxon pois valore s?o constantes
t.test(localaverage_eff_cool, BIlocalaverage_eff_cool)