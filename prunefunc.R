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

t = arv_teste(50, 10)
ToDataFrameTree(t, 'n','p','x')
saveRDS(t, 'arvore_teste.rds')

#FUNÇÕES TESTE

t = readRDS('arvore_teste.rds')
ToDataFrameTree(t, 'n', 'p', 'x')

checknodo = function(nodo, par){
  if(isLeaf(nodo)) return(FALSE) #pula folhas
  if(!all(sapply(nodo$children, isLeaf))) return(FALSE) #só quero cortar pais de folhas
  p1 = nodo$children[[1]][[par]]
  p2 = nodo$children[[2]][[par]]
  if(identical(p1,p2)) return(TRUE) #corta folhas com parâmetro idênticos
  else return(FALSE) #pula
}

killchildren = function(nodo){
  nodo$children = NULL
}

sculptskeleton = function(t, par, copy = F, print=F){
  if(copy) tree = Clone(t)
  else tree = t
  nodes = Traverse(tree, 'level')
  levels = sapply(nodes, function(x) x$level)
  k = tree$height
  for(l in rev(1:(k-1))){
    nds = nodes[levels==l]
    to_trim = nds[sapply(nds, function(x) checknodo(x, par))]
    if(print) print(paste(length(to_trim)*2, 'nodos serão removidos',sep=' '))
    lapply(to_trim, FUN = killchildren)
  }
  if(copy) return(tree)
}
t_skel = sculptskeleton(t, 'x', copy = T, print = T)
ToDataFrameTree(t, 'x')
ToDataFrameTree(t_skel, 'x')


#FUNÇÕES REAIS

shouldyoucut = function(nodo, par, Nmin){
  if(isLeaf(nodo)) return(FALSE) #pula folhas
  if(!all(sapply(nodo$children, isLeaf))) return(FALSE) #só quero cortar pais de folhas
  n1 = (nodo$children[[1]][['n']]) >= Nmin
  n2 = (nodo$children[[2]][['n']]) >= Nmin
  d1 = nodo$children[[1]][[par]]
  d2 = nodo$children[[2]][[par]]
  decision(n1,n2,d1,d2)
}


killchildren = function(nodo){
  nodo$children = NULL
}


sculptskeleton = function(t, Nmin, copy = F, print=F){
  if(copy) tree = Clone(t)
  else tree = t
  nodes = Traverse(tree, 'level')
  levels = sapply(nodes, function(x) x$level)
  k = tree$height
  for(l in rev(1:(k-1))){
    nds = nodes[levels==l]
    to_trim = nds[sapply(nds, function(x) shouldyoucut(x, 'x', Nmin))]
    if(print) print(paste(length(to_trim)*2, 'nodos serão removidos',sep=' '))
    lapply(to_trim, FUN = killchildren)
  }
  if(copy) return(tree)
}

teste = sculptskeleton(t, 10, copy = T, print = T)
ToDataFrameTree(t, 'n', 'x')
ToDataFrameTree(teste, 'n', 'x') #FUNCIONANDO, lembrar de trocar par por 'dom' depois
