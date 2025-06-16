library(skeleton)
extractTransitions = function(skeleton){
  contexts = Traverse(skeleton, filterFun = isLeaf)
  d = max(sapply(contexts, function(leaf) nchar(leaf$context)))
  transitions = lapply(contexts, function(leaf) leaf$transitions)
  names(transitions) = sapply(contexts, function(leaf) leaf$context)
  pasts = apply(expand.grid(replicate(d, alfabeto, simplify = FALSE)), 1, paste0, collapse = "")
  full_transitions = probabilities = replicate(length(alfabeto)^d, rep(0,length(alfabeto)), simplify = FALSE)
  for(w in 1:length(pasts)){
    full_transitions[[w]]=transitions[[getMaxContext(names(transitions),pasts[w])]]
  }
  names(full_transitions)=pasts
  return(full_transitions)
}