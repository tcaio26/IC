#FUNÇÃO DE CORTAR ÁRVORE
library(data.tree)
set.seed(247005)
#arvore pra teste
recbuild = function(nodo, corte, bign){
  N = nodo$n
  u = runif(1)
  n_l = round(N*u)
  n_r = N-n_l
  
  l = nodo$AddChild(paste0(nodo$name, 'l'))
  l$n = n_l
  l$p = rbeta(1, n_l, n_r)
  l$x = sample(c(1,0),1,prob=c(l$p, 1-l$p))
  if(n_l>corte) recbuild(l, corte, bign)
  
  r = nodo$AddChild(paste0(nodo$name, 'r'))
  r$n = n_r
  r$p = rbeta(1, n_r, n_l)
  r$x = sample(c(1,0),1,prob=c(r$p, 1-r$p))
  if(n_r>corte) recbuild(r, corte, bign)
}
arv_teste = function(n_start, corte){
  raiz = Node$new('raiz')
  raiz$x = 1
  raiz$n = n_start
  
  recbuild(raiz, corte, n_start)
  return(raiz)
}

t = arv_teste(20, 10)
ToDataFrameTree(t, 'n','p','x')

nodos = Traverse(t, 'level')

checknodo = function(nodo, par){
  if(isLeaf(nodo)) return(FALSE)
  p1 = nodo$children[[1]][[par]]
  p2 = nodo$children[[2]][[par]]
  if(identical(p1,p2)) return(TRUE) #deleta as crianças
  else return(FALSE)
}

killchildren = function(nodo){
  nodo$children = NULL
}

killchildren(t$raizl)
t
