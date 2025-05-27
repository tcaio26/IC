library(data.tree)

killchildren = function(nodo){
  nodo$children = NULL
}
