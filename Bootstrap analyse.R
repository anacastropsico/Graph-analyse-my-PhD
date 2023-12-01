###DADOSEXEMPLO##
relations <- c(1, 5, 3, 8, 2, 9, 3, 3, 5, 6, 9, 6, 4, 7, 2, 4, 7, 5, 7, 6, 1, 4, 2, 5, 3, 6, 
               4, 7, 5, 6, 4, 5, 3, 8, 7, 9, 2, 5, 1, 5, 3, 6, 5, 9, 3, 5, 7, 3, 9, 3, 5, 3)
matrix(relations, 7, 7)
matrixnet<-matrix(relations, 7, 7)
matcor <- cor(matrixnet, method = "spearman")

matrixnetwork = graph.adjacency(matricor, mode="undirected", weighted = TRUE, add.colnames=NULL, diag=FALSE)
plot(matrixnetwork)
matrix

##boot randon

#####randon Rede 100x######
N <- 7L
set.seed(2023)
R <- 100L
RANDON_PERMUT1<- vector("list", length = R)
for(i in seq.int(R)) {
  indices <- sample(N, replace = TRUE)
  RANDON_PERMUT1[[i]] <- sample_gnp (7,0.6, directed = FALSE, loops = FALSE)
}

# BOOT 1
N <- 7L
set.seed(2023)
R <- 100L
bootlinks <- vector("list", length = R)
for(i in seq.int(R)) {
  indices <- sample(N, replace = FALSE)
  bootlinks[[i]] <- links[indices, ]
}
matrixboot= as.matrix(BIboot[[50]])
matrixnetwork2 = graph.adjacency(matrixboot, mode="undirected", weighted = TRUE, add.colnames=NULL, diag=FALSE)
plot(matrixnetwork2)

plot (matrixnetwork2)
degree(matrixnetwork2, normalized = FALSE, loops = FALSE)

g_boot_list <- lapply(BIboot, graph_from_adjacency_matrix, mode = "undirected", weighted = TRUE, add.colnames=NULL, diag=FALSE)
plot (g_boot_list[[100]])
degree(g_boot_list[[100]], normalized = FALSE, loops = FALSE)
#############BOTPERMUTATION FORMA 2 ####################
N <- 7L
set.seed(2023)
R <- 100L
BIPERMUT<- vector("list", length = R)
for(i in seq.int(R)) {
  indices <- sample(N, replace = TRUE)
  BIPERMUT[[i]] <- permute(matrixnetwork, sample(vcount(matrixnetwork)))  
}

plot (BIPERMUT[[1]])
degree(BIPERMUT[[1]], normalized = FALSE, loops = FALSE)

#################FORMA 3 #######################
N <- 7L
set.seed(2023)
R <- 100L
BIboot2 <- vector("list", length = R)
for(i in seq.int(R)) {
  indices <- sample(N, replace = TRUE)
  BIboot2[[i]] <- matrixnetwork
}

plot (BIboot2[[1]])
degree(BIboot2[[1]], normalized = FALSE, loops = FALSE)


#####

# Bootstrapping com 100 replicacoes para CONDICOES (AI-BI)
##forma1
R<-100L
boot_degree <- vector("list", R)
for(i in seq.int(R)) {
  boot_degree[[i]] <- degree(AIPERMUT[[i]], normalized = FALSE, loops = FALSE)
}

#faz por linha
sapply(boot_degree, 
       function(x) unlist(shapiro.test(x)[c("statistic", "p.value")]))
lapply(boot_degree, shapiro.test)


## Converter em colunas leg?veis##
dmat <- do.call("rbind", boot_degree2) #op??o 1



#Exemplars of tests#
t.test(boot_degree2, mu = 0)
t.test(boot_degree[["t"]][,1], Rboot_degree[["t"]][,1])#onesample parametric
wilcox.test(boot_degree[["t"]][,1],mu = 0) #onesample not parametric
wilcox.test(boot_degree[["t"]][,1], Rboot_degree[["t"]][,1]) #not paired test T not parametric 
boot_degree_df <- do.call(rbind, boot_degree2) |> as.data.frame() #op??o 2