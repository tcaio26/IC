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

teste = startskel2(amostra, c('0','1','2','3'), Nmin)
print(teste, 'context','n','transitions')
(skel_teste = sculptskeleton2(teste, Nmin, copy=T, declare=T))

##extraindo transições
extractTransitions = function(skeleton){
  contexts = Traverse(skel_teste, filterFun = isLeaf)
  d = max(nchar(contexts))
  transitions = lapply(contexts, function(leaf) leaf$transitions)
  names(transitions) = sapply(contexts, function(leaf) leaf$context)
  pasts = apply(expand.grid(replicate(d, alfabeto, simplify = FALSE)), 1, paste0, collapse = "")
  full_transitions = probabilities = replicate(length(alfabeto)^d, rep(0,length(alfabeto)), simplify = FALSE)
  getMaxContext = function(contexts, string){
    candidates = contexts[sapply(contexts, function(str) grepl(paste0(str,'$'), string))]
    if(length(candidates)>0) return(candidates[[which.max(nchar(candidates))]])
    return(NULL)
  }
  for(w in 1:length(pasts)){
    full_transitions[[w]]=transitions[[getMaxContext(names(transitions),pasts[w])]]
  }
  names(full_transitions)=pasts
  return(full_transitions)
}

contexts = Traverse(skel_teste, filterFun = isLeaf)
transitions = lapply(contexts, function(leaf) leaf$transitions)
names(transitions) = sapply(contexts, function(leaf) leaf$context)
d=3
pasts = apply(expand.grid(replicate(d, alfabeto, simplify = FALSE)), 1, paste0, collapse = "")
full_transitions = probabilities = replicate(length(alfabeto)^d, rep(0,length(alfabeto)), simplify = FALSE)
getMaxContext = function(contexts, string){
  candidates = contexts[sapply(contexts, function(str) grepl(paste0(str,'$'), string))]
  if(length(candidates)>0) return(candidates[[which.max(nchar(candidates))]])
  return(NULL)
}
for(w in 1:length(pasts)){
  full_transitions[[w]]=transitions[[getMaxContext(names(transitions),pasts[w])]]
}
names(full_transitions)=pasts

#transformando em matriz
M = matrix(0, nrow = length(full_transitions), ncol = length(full_transitions),
           dimnames = list(names(full_transitions), names(full_transitions)))

d = nchar(names(full_transitions)[[1]])
for(w in names(full_transitions)){
  possible_transitions = paste0(substr(w,2,d),alfabeto)
  M[w,possible_transitions] = full_transitions[[w]]
}

######## função geral
generate_skeleton2 = function(string, Nmin,
                              alphabet = sort(unique(unlist(strsplit(string,'')))),
                              alpha = 0.05, sensibility = 0.01){
  #checagem de inputs
  if(missing(Nmin)) Nmin = ceiling(log(alpha, 1-sensibility) - log(length(alphabet), 1-sensibility))
  root = startskel2(string, alphabet, Nmin)
  sculptskeleton2(root, Nmin)
  
}