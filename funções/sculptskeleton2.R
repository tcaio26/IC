library(skeleton)
sculptskeleton2 = function(root, Nmin, copy=F, declare = F){
  if(copy) skel = Clone(root)
  else skel = root
  nodes = Traverse(skel,'level')
  levels = sapply(nodes, function(node) node$level)
  d = skel$height
  for(l in (d-1):1){
    selected = nodes[levels==l]
    to_prune = selected[sapply(selected, function(node) shouldyoucut2(node, Nmin))]
    if(declare) print(paste0("Level ", l, ', ',length(to_prune),' nodes to prune'))
    lapply(to_prune, function(parent) parent$children = NULL)
  }
  if(copy) return(skel)
}