amostra = sample(c(0,1),1000,T) #entradas
sensib = 0.01
alpha = 0.05

Nmin = log(alpha, base = 1-sensib)

match = function(str, p){ 
  pos = stringr::str_locate_all(str, p)[[1]][, "start"]
  return(pos)
} #funções

calc_gamma = function(x, sensib){
  if((x[1]!=0)&&(x[2]!=0)) return(1)
  else return((1-sensib)^max(x))
}

new_skel_node = function(sample, sensib,
                         index = 1:length(sample), father, symbol){
  stopifnot(is.numeric(index))
  if(any(index>length(sample))) stop()
  if(!missing(father) && missing(symbol)) stop()
  
  s = sample[index]
  counts = c(length(s)-sum(s), sum(s))
  structure(
    list(
      counts = counts,
      gamma = calc_gamma(counts, sensib)
    ),
    class = 'Skel Node',
    context = ifelse(missing(father), 'r', 
                     paste0(attr(father, 'context'),as.character(symbol))),
    index = index
  )
}

nodotest = new_skel_node(amostra, sensib)

need_children = function(node, alpha, Nmin){
  stopifnot(class(node)=="Skel Node")
  
  (node$gamma>=alpha && sum(node$counts)>=Nmin)
}

need_children(nodotest, alpha, Nmin)

make_children = function(node, sample, symbol, ...){
  i = attr(node, 'index')
  s = i[which(sample[i]==symbol)]+1
  s = s[s<=max(i)]
  
  new_skel_node(sample, sensib, index = s, father = node, symbol = symbol)
}

moleque = make_children(nodotest, amostra, 0)

entre = function(x,a,b,inc.L=F,inc.U=F){
  L = ifelse(inc.L,x>=a,x>a)
  U = ifelse(inc.U,x<=b,x<b)
  return(L&U)
}

generate_skeleton = function(sample, Nmin=59, sensibility, alpha){
  use_nmin = !missing(Nmin)
  #checks
  if(!is.numeric(sample)&&!all(is.character(sample),length(sample)==1)){
    stop("Sample must be a numeric vector or a string")
  }
  if(is.character(sample)) sample = as.numeric(unlist(strsplit(sample,'')))
  if(!all(unique(sample)%in%c(0,1))) stop("Sample must be comprised of 0 and 1 values")
  if(length(sample)<2) stop("Sample must be at least 2 symbols long")
  
  if(!(missing(alpha)||(is.double(alpha) && entre(alpha,0,1)))) stop("alpha must be a numeric value between 0 and 1")
  if(!(missing(sensibility)||(is.double(sensibility) && entre(sensibility,0,0.5)))) stop("sensibility must be a numeric value between 0 and 0.5")
  if(!(missing(Nmin)||(is.numeric(Nmin) && Nmin>0))) stop("Nmin must be a positive integer") 
  
  if(all(missing(sensibility), missing(alpha), missing(Nmin))){
    warning("No tolerance arguments provided, proceeding with default Nmin = 59")
    use_nmin=T
  }
  
  if(xor(missing(sensibility),missing(alpha))){
    warning("Provided only alpha or sensibility. Provide both or none. Proceeding with ", Nmin)
    use_nmin=T
  }
  if(!missing(Nmin)&&length(sample)<=Nmin) stop("Sample length must be larger than Nmin, preferably a lot larger")
  
  #generating function
  if(!use_nmin) Nmin = ceiling(log(alpha, 1-sensibility))
  if(Nmin>=length(sample)) stop("Nmin too large for sample")
  #tree = genskel(...)
  
  #output
  
  return(Nmin)
}


generate_skeleton('101010', Nmin=20)
generate_skeleton(c(1,0,1,2))
generate_skeleton(sample(c(0,1),1000,T),alpha=0.2)

#função que faz mais sentido para ser recursiva:
genskel = function(sample, index=1:length(sample), context){
  s = sample[index]
  n = length(s)
  dom = ifelse(sum(s)%in%c(0,n), s[1], -1)
  structure(
    list(
      index = index,
      context = context,
      n = n,
      dom = dom
    ),
    class = 'Skel Node'
  )
}



library(data.tree)

divtree <- function(start, Nmin) { #do deepseek se já não estava óbvio
  # Create root node
  raiz <- Node$new('r')
  raiz$n <- start
  
  # Internal recursive function to build the tree
  build_tree <- function(node, current_val, Nmin) {
    if (current_val > Nmin) {
      # Create left child
      left <- node$AddChild('left')
      left$n <- current_val / 2
      build_tree(left, current_val / 2, Nmin)
      
      # Create right child
      right <- node$AddChild('right')
      right$n <- current_val / 2
      build_tree(right, current_val / 2, Nmin)
    }
  }
  
  # Start building the tree
  build_tree(raiz, start, Nmin)
  return(raiz)
}

genskel = function(node, sample, index, Nmin, prob = F){
    #resultado 0
    l = node$AddChild(paste0(node$name,'0'))
    s = index[which(sample[index]==0)]+1
    s = s[s<=max(index)]
    n = length(s)
    l$index = s
    l$n = length(l$index)
    l$context = paste0(node$context,'0')
    l$dom = ifelse(sum(sample[s])%in%c(0,l$n), sample[s][1], -1)
    if(prob) l$p = sum(sample[s])/n
    if(length(l$index)>=Nmin && l$dom <0){
      genskel(l, sample, l$index, Nmin, prob)
    }
    
    #resultado 1
    r = node$AddChild(paste0(node$name,'1'))
    s = index[which(sample[index]==1)]+1
    s = s[s<=max(index)]
    n = length(s)
    r$index = s
    r$n = length(r$index)
    r$context = paste0(node$context,'1')
    r$dom = ifelse(sum(sample[s])%in%c(0,l$n), sample[s][1], -1)
    if(prob) r$p = sum(sample[s])/n
    if(length(r$index)>=Nmin && r$dom < 0){
      genskel(r, sample, r$index, Nmin, prob)
    }
}

startskel = function(sample, Nmin, prob = F){
  raiz = Node$new("r")
  raiz$index = 1:length(sample)
  raiz$n = length(sample)
  raiz$context = ''
  raiz$dom = ifelse(sum(sample)%in%c(0,raiz$n), sample[1], -1)
  if(prob) raiz$p = sum(sample)/raiz$n
  
  genskel(raiz, sample, raiz$index, Nmin, prob = prob)
  return(raiz)
}


#teste com 1000 elementos da amostra da CEMAV
df = read.csv('parametros_geradores_amostra.csv', colClasses = c('character','numeric'))

st = readLines('amostra_skel_100k.txt')
nchar(st)

n = 2000

s = as.numeric(unlist(strsplit(st, '')))[1:n]

teste = startskel(s, 229, prob = T)
ToDataFrameTree(teste, 'n', 'context', 'dom', 'p')
print(teste, attributes = 'p')



genskel_mod = function(node, sample, index, Nmin, prob = F){
  #resultado 0
  l = node$AddChild(paste0(node$name,'0'))
  s = index[which(sample[index]==0)]+1
  s = s[s<=max(index)]
  n = length(s)
  l$index = s
  l$n = length(l$index)
  l$context = paste0(node$context,'0')
  l$dom = ifelse(sum(sample[s])%in%c(0,l$n), sample[s][1], -1)
  if(prob) l$p = sum(sample[s])/n
  if(length(l$index)>=Nmin){
    genskel_mod(l, sample, l$index, Nmin, prob)
  }
  
  #resultado 1
  r = node$AddChild(paste0(node$name,'1'))
  s = index[which(sample[index]==1)]+1
  s = s[s<=max(index)]
  n = length(s)
  r$index = s
  r$n = length(r$index)
  r$context = paste0(node$context,'1')
  r$dom = ifelse(sum(sample[s])%in%c(0,l$n), sample[s][1], -1)
  if(prob) r$p = sum(sample[s])/n
  if(length(r$index)>=Nmin){
    genskel_mod(r, sample, r$index, Nmin, prob)
  }
}

startskel_mod = function(sample, Nmin, prob = F){
  raiz = Node$new("r")
  raiz$index = 1:length(sample)
  raiz$n = length(sample)
  raiz$context = ''
  raiz$dom = ifelse(sum(sample)%in%c(0,raiz$n), sample[1], -1)
  if(prob) raiz$p = sum(sample)/raiz$n
  
  genskel_mod(raiz, sample, raiz$index, Nmin, prob = prob)
  return(raiz)
}

verificacao_str = c(0,1,0,0,0,1,1,0)
verificacao = startskel_mod(verificacao_str, 1, prob = T)
d =ToDataFrameTree(verificacao, 'n', 'p')
d[d$n>0,]
