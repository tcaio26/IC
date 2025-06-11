library(skeleton)
amostra = readLines('amostra_a_4_100k.txt')
a_teste = substr(amostra,1,5000)

alfabeto = sort(unique(unlist(strsplit(a_teste,''))))

a = 0.05
y = 0.01

Nmin = ceiling(log(a, 1-y) - log(length(alfabeto), 1-y))

#importando função de probabilidades enquanto eu não coloco no pacote
symbolcounts = function(str, pattern, alphabet){
  positions = match_str(str, pattern)
  if(all(positions == 'no matches')) return(numeric(length(alphabet)))
  vec = string_to_vec(str)[positions]
  sapply(as.numeric(alphabet), function(s) sum(vec==s))
}

####startskel
startskel2 = function(string, alphabet, Nmin){
  root = Node$new('r')
  root$context = ''
  root$counts = symbolcounts(string, '', alphabet)
  root$n = nchar(string)
  
  genskel2(root, string, alphabet, Nmin)
  
  lapply(Traverse(root), function(node) node$transitions = (node$counts>0))
  
  return(root)
}

#genskel
genskel2 = function(parent, string, alphabet, Nmin){
  for(u in alphabet){
    node = parent$AddChild(paste0(u, parent$name))
    node$context = paste0(u, parent$context)
    node$counts = symbolcounts(string, node$context, alphabet)
    node$n = sum(node$counts)
    if(node$n >= Nmin) genskel2(node, string, alphabet, Nmin)
  }
}

#sculpt
shouldyoucut2 = function(parent, Nmin){
  if(isLeaf(parent)) return(FALSE) #no children to prune
  if(all(sapply(parent$children, function(node) node$n < Nmin))) return(TRUE) #no significant leaves
  if(any(!sapply(parent$children, isLeaf))) return(FALSE) #shouldn't happen, but avoid cutting non-leaves
  valid_leaves = parent$children[sapply(parent$children, function(node) node$n >= Nmin)]
  if(all(
    sapply(valid_leaves, function(leaf) all(parent$transitions==leaf$transitions))
    )) return(TRUE) #no information added by leaves
  return(FALSE)
}

sculptskeleton2 = function(root, Nmin, copy=F, declare = F){
  if(copy) skel = Clone(root)
  else skel = root
  nodes = Traverse(root,'level')
  levels = sapply(nodes, function(node) node$level)
  d = root$height
  for(l in (d-1):1){
    selected = nodes[levels==l]
    to_prune = selected[sapply(selected, function(node) shouldyoucut2(node, Nmin))]
    if(declare) print(paste0("Level ", l, ', ',length(to_prune),' nodes to prune'))
    lapply(to_prune, function(parent) parent$children = NULL)
  }
  if(copy) return(skel)
}

teste = startskel2(a_teste, c('0','1','2','3'), 100)
print(teste, 'context','n','transitions')
(sculptskeleton2(teste, 100, copy = T, declare=T))





teste_sculpt = Clone(teste)
nodes = Traverse(teste_sculpt,'level')
levels = sapply(nodes, function(node) node$level)
d = teste_sculpt$height
for(l in (d-1):1){
  selected = nodes[levels==l]
  to_prune = selected[sapply(selected, function(node) shouldyoucut2(node, 100))]
  print(paste0(length(to_prune),' nodes to be pruned'))
  lapply(to_prune, function(parent) parent$children = NULL)
}
print(teste_sculpt, 'context', 'n', 'transitions')
