library(skeleton)
generate_skeleton2 = function(string, Nmin,
                              alphabet = sort(unique(unlist(strsplit(string, '')))),
                              alpha = 0.05, sensibility = 0.01){
  #checagem de inputs
  
  
  #esqueleto
  if(missing(Nmin)) Nmin = ceiling(log(alpha, 1-sensibility) - log(length(alphabet), 1-sensibility))
  root = startskel2(string, alphabet, Nmin)
  sculptskeleton2(root, Nmin)
  
  #matriz de transições
  transitions = extractTransitions(root)
  
  M = matrix(0, nrow = length(full_transitions), ncol = length(full_transitions),
             dimnames = list(names(full_transitions), names(full_transitions)))
  d = nchar(names(full_transitions)[[1]])
  for(w in names(full_transitions)){
    possible_transitions = paste0(substr(w,2,d),alfabeto)
    M[w,possible_transitions] = full_transitions[[w]]
  }
  
  return(list(skel = root, transitions = M))
}