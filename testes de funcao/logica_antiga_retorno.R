library(skeleton)
amostra = readLines('amostra_a_4_100k.txt')
a_teste = strsplit(amostra, '')[[1]] |> as.factor()
A = c('0','1','2','3')


####startskel de volta com lógica de vetor
startskel3 = function(sample, alphabet, Nmin, sep = '|', contextsep = sep){
  root = Node$new('r')
  root$context = ''
  root$index = 1:length(sample)
  root$counts = table(factor(sample[root$index], levels = alphabet))
  root$n = length(sample)
  
  genskel3(root, 1, sample, alphabet, Nmin, sep)
  
  lapply(Traverse(root), function(node){
    transitions = (node$counts>0)
    if(sum(transitions)==0) transitions = c(T,T,T,T) #não assumimos nada para nodos não observados.
    node$transitions = transitions
    })
  
  return(root)
}

#genskel
genskel3 = function(parent, order, sample, alphabet, Nmin, sep = '|', contextsep = sep){
  possible_index = parent$index[parent$index>order]
  
  for(u in alphabet){
    node = parent$AddChild(paste0(u, sep, parent$name))
    node$context = paste0(u, contextsep, parent$context)
    node$index = possible_index[which(sample[possible_index-order]==u)]
    node$counts = table(factor(sample[node$index], levels = alphabet))
    node$n = sum(node$counts)
    if(node$n >= Nmin) genskel3(node, order+1, sample, alphabet, Nmin, sep)
  }
}


a = 0.05
y = 0.01

Nmin = ceiling(log(a, 1-y) - log(length(A), 1-y))
teste = startskel3(a_teste, c('0','1','2','3'), Nmin)
print(teste, 'context','n','transitions')
(skel_teste = sculptskeleton2(teste, Nmin, copy=T, declare=T)) |> print('context','n','transitions')
