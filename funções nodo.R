amostra = sample(c(0,1),1000,T) #entradas
sensib = 0.1
alpha = 0.05

Nmin = log(alpha, base = sensib)

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
