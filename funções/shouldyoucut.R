library(data.tree)

shouldyoucut = function(nodo, par, Nmin){
  if(isLeaf(nodo)) return(FALSE) #pula folhas
  if(!all(sapply(nodo$children, isLeaf))) return(FALSE) #só quero cortar pais de folhas
  n1 = (nodo$children[[1]][['n']]) >= Nmin
  n2 = (nodo$children[[2]][['n']]) >= Nmin
  d1 = nodo$children[[1]][[par]]
  d2 = nodo$children[[2]][[par]]
  decision(n1,n2,d1,d2)
}