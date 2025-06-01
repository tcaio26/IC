library(data.tree)
source('funções/shouldyoucut.R')
source('funções/killchildren.R')
source('funções/decision.R')

sculptskeleton = function(t, Nmin, copy = F, print=F){
  if(copy) tree = Clone(t)
  else tree = t
  nodes = Traverse(tree, 'level')
  levels = sapply(nodes, function(x) x$level)
  k = tree$height
  for(l in rev(1:(k-1))){
    nds = nodes[levels==l]
    to_trim = nds[sapply(nds, function(x) shouldyoucut(x, 'dom', Nmin))]
    if(print) print(paste(length(to_trim)*2, 'nodos serão removidos',sep=' '))
    lapply(to_trim, FUN = killchildren)
  }
  if(copy) return(tree)
}