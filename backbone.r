library(igraph)
library(Matrix)

## original function from 'disparityfilter' package.
get.backbone <- function (graph, alpha = 0.05, directed = FALSE) {
  G = graph
  edgelist = get.data.frame(G)
  colnames(edgelist) = c("from", "to", "weight")
  nodes = unique(c(edgelist[, 1], edgelist[, 2]))
  N = length(nodes)
  backbone = NULL
  cat("Disparity Filter\n")
  cat("alpha =", alpha, "\n")
  cat("\nOriginal graph\n")
  print(G)
  for (i in 1:N) {
    nei = edgelist[edgelist$from == nodes[i], ]
    nei = rbind(nei, edgelist[edgelist$to == nodes[i], ])
    k_i = length(edgelist$to[edgelist$to == nodes[i]]) + 
      length(edgelist$to[edgelist$from == nodes[i]])
    if (k_i > 1) {
      for (j in 1:k_i) {
        
        p_ij = as.numeric(nei$weight[j])/sum(as.numeric(nei$weight))
        alpha_ij = (1 - p_ij)^(k_i - 1)
        if (alpha_ij < alpha) {
          backbone = rbind(backbone, c(nei$from[j], nei$to[j], 
                                       nei$weight[j]))
        }
      }
    }
  }
  colnames(backbone) = c("from", "to", "weight")
  backbone = unique(backbone[, c("from", "to", "weight")])
  G_backbone = graph.data.frame(backbone, directed = directed)
  cat("\nBackbone graph\n")
  print(G_backbone)
  return(G_backbone)
}

## Hip function using sparse matrices (results should be identical to get.backbone, but much faster).
backbone <- function(g){
  mat = get.adjacency(g, attr='weight')
  if(!is.directed(g)) mat[lower.tri(mat)] = 0 # prevents counting edges double in symmetric matrix (undirected graph)
  weightsum = rowSums(mat) + colSums(mat)
  k = rowSums(mat>0) + colSums(mat>0)
  
  edgelist_ids = get.edgelist(g, names=F)
  alpha_ij = getAlpha(mat, weightsum, k, edgelist_ids) # alpha for edges from i to j
  alpha_ji = getAlpha(mat, weightsum, k, edgelist_ids, transpose=T)
  alpha_ij[alpha_ji < alpha_ij] = alpha_ji[alpha_ji < alpha_ij]
  alpha_ij
}

getAlpha <- function(mat, weightsum, k, edgelist_ids, transpose=F){
  if(transpose) mat = t(mat)
  mat = mat / weightsum
  alpha = ((1 - mat)^(k-1))
  if(transpose) alpha = t(alpha)
  alpha[edgelist_ids]
}


## experimental: calculate backbone for only in/out degree (haven't tested this yet)
backbone.indegree <- function(g){
  mat = t(get.adjacency(g, attr='weight'))
  weightsum = rowSums(mat)
  k = rowSums(mat > 0)
  edgelist_ids = get.edgelist(g, names=F)
  getAlpha(mat, weightsum, k, edgelist_ids)
}

backbone.outdegree <- function(g){
  mat = t(get.adjacency(g, attr='weight'))
  weightsum = colSums(mat)
  k = colSums(mat > 0)
  edgelist_ids = get.edgelist(g, names=F)
  getAlpha(mat, weightsum, k, edgelist_ids, transpose=T)
}


