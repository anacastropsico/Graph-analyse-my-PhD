

E(matrixnetwork)$color[as.logical(links[,5])]<-'red' #Atribuir cor as arestas
edge.width=strength(matrixnetwork) *2.3 #Defino a espessura do n? usando strenght e jogo dentro do gr?fico
edge.width=E(matrixnetwork)$weight*2.3#Defino a espessura do n? usando correla??es e jogo dentro do gr?fico
edge.width=abs(E(matrixnetwork)$weight)*8

E(matrixnetwork)[strength(matrixnetwork)] $width
E(matrixnetwork)$width <- ifelse(E(matrixnetwork)$Links > 0.85,'1.1','0.5')

tamanhos<-E(matrixnetwork)$width <- 3*E(matrixnetwork)$weight
w <- E(matrixnetwork)$width <- 3*E(matrixnetwork)$weight
E(matrixnetwork)$width <- 3*abs(w)
widht1<-E(matrixnetwork)$width <- ifelse(w > 0.9, '2')
widht2<-E(matrixnetwork)$width <- ifelse(w > 0.8, '1')
E(matrixnetwork)$color <- ifelse(w > 0.7,'0.5')
E(matrixnetwork)$color <- ifelse(w > 0,  '0.5')
E(matrixnetwork)$color <- ifelse(w > 1, '2', '0.5')
edge.width=E(matrixnetwork)$width *0.7

plot(matrixnetwork, layout = layout_in_circle)

plot(matrixnetwork,edge.width = E(matrixnetwork)$friendship, edge.arrow.size = E(matrixnetwork)$friendship)

V(matrixnetwork)[estimate_betweenness(matrixnetwork, cutoff = -1)>=4] $color<- 'gray' #chamei estimate Betweenness como condi??o
V(matrixnetwork)[CordoN <=0.4]$color<- 'gray' #Chamei estimate betweenes pelo nome que atribui a ela.
vertex.color=CordoN *100 #mesma coisa do de baixo, por?m com nome atribuida 
vertex.color=estimate_betweenness(matrixnetwork,cutoff = -1) *100 #Cor do n? a partir do estimate beweeness, colocar dentro do comando do gr?fico
vertex.label.color=degreen(matrixnetwork) black)
vertex.size=degree(matrixnetwork) *8.2  #Tamanho do n? a partir do degreen, define-se o hub tbm, copiar para dentro do grpafico

g <-plot(matrixnetwork, layout=layout_in_circle, vertex.size=degree(matrixnetwork) *4.2, 
     vertex.label.color="black", 
     vertex.label.degreen="-pi/2", vertex.label.dist = 5, edge.arrow.width=NA,
     vertex.frame.color=NA, 
     edge.width=abs(E(matrixnetwork)$weight)*8,
     V(matrixnetwork)$color <- colorRampPalette(c("#4477AA","#BB4444"))(100)[V(matrixnetwork)$eb_index]
) #

plot( matrixnetwork, layout=layout_in_circle, vertex.size=degree(matrixnetwork) *4.2, 
  vertex.label.color="black", 
  edge.width=E(matrixnetwork)$weight *0.7,
  vertex.label.degreen="-pi/2", vertex.label.dist = 5, edge.arrow.width=NA,
  vertex.frame.color=NA, 
  edge.width=abs(E(matrixnetwork)$weight)*8, edge.arrow.width=NA, 
)


plot (matrixnetwork, layout=layout_in_circle)
#vertex.color=ifelse(V(matrixnetwork)$ccc == "red", "blue")) #edge.width=ifelse(E(matrixnetwork)$DG, 0.9))
#================================###
###Cores Centralidade Normalizada###
#================================###
# Calculate eigen centrality and check the distribution We're attaching the
# result of eigen_centrality() straight onto the vertices as verticy-attributes
V(matrixnetwork)$eb <- estimate_betweenness(matrixnetwork,cutoff = 0) #$vector hist(matrixnetwork$eb) #atribui propriedade Ao vertex
frame.color 
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
tkplot( matrixnetwork, layout=layout_in_circle, vertex.size=degree(matrixnetwork) *4.2, 
       vertex.label.color="black", 
       edge.width=E(matrixnetwork)$weight *0.7,
       vertex.label.degreen="-pi/2", vertex.label.dist = 5, edge.arrow.width=NA,
       vertex.frame.color=NA, 
       edge.width=abs(E(matrixnetwork)$weight)*8, edge.arrow.width=NA, 
      )
##Outra forma de atribuir cores


dist.from.NYT <- distances(net, v=V(net)[media=="NY Times"], to=V(net), weights=NA)



# Set colors to plot the distances:

oranges <- colorRampPalette(c("dark red", "gold"))

col <- oranges(max(dist.from.NYT)+1)

col <- col[dist.from.NYT+1]



plot(net, vertex.color=col, vertex.label=dist.from.NYT, edge.arrow.size=.6, 
     
     vertex.label.color="white")

# Random network
g <- erdos.renyi.game(100,250,'gnm', directed=F)


#GRAFICOS PARA MEDIDAS
hist(GRAU)