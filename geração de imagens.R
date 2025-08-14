library(skeleton)
library(tibble)
library(plot.matrix)
set.seed(247005)
skel_contexts = list(
  'sol sol' = c(1,1,0,1), 'tempestade sol sol' = c(0,1,0,0),
  'nuvem chuva' = c(1,0,1,0), 'tempestade' = c(1,1,0,0) #transições permitidas
)
separator = ' '
alphabet = c('sol','nuvem','chuva','tempestade')

c = max(sapply(names(skel_contexts), function(x) length(strsplit(x, separator)[[1]]))) #parâmetros, alguns serão automáticos
d = 3 #testando com 3, mudar p 5
A = c('sol','nuvem','chuva','tempestade')
a = length(A)

#criar todos os sufixos de tamanho d possível
suffixes = apply(expand.grid(replicate(d, A, simplify = FALSE)), 1, paste, sep = separator, collapse = separator)
length(suffixes) #deve ser a^d
suffixes = paste0(separator, suffixes) #garante que não vai pegar símbolos no meio.
###### FUNÇÕES AUXILIARES
#para uma lista/vetor de contexto e uma string s, retorne o maior contexto que seja parte do final de s.
#usando o skel_contexts mesmo
getMaxContext = function(contexts, string){
  candidates = contexts[sapply(contexts, function(str) grepl(paste0(str,'$'), string))]
  if(length(candidates)>0) return(candidates[[which.max(nchar(candidates))]])
  return(NULL)
}

#teste com separador e símbolos
getMaxContext(paste0('_',names(skel_contexts)), 'a_a_a_b_c_dd_a_a')

#para um contexto com transições proibidas e permitidas, como garantir que gere probs validas? idealmente entre 0.05 e 0.95
generate_probs = function(n, lower.bound = 0.05){
  probs = numeric(n)
  for(i in 1:n){
    l = 0.05
    u = 1-l*(n-i)-sum(probs) #garante que as probs sobrando poderão ser pelo menos 0.05
    p = runif(1, l, u)
    if(i == n) p = 1-sum(probs)
    probs[i] = p
  }
  return(probs)
}

#associar probabilidades de forma aleatória
probabilities = replicate(a^d, rep(0,a), simplify = FALSE)

for(s in 1:length(suffixes)){
  cont = getMaxContext(names(skel_contexts), suffixes[s])
  if(is.null(cont)) transitions = c(1,1) 
  else transitions = skel_contexts[[cont]]
  
  if(sum(transitions)==1) probabilities[[s]] = transitions
  else probabilities[[s]][which(transitions==1)] = generate_probs(sum(transitions))
}
names(probabilities) = suffixes

lobstr::obj_size(probabilities)

#transformar uma lista nomeada em matriz de probabilidades
M = matrix(0, nrow = length(probabilities), ncol = length(probabilities),
           dimnames = list(names(probabilities), names(probabilities)))

d = length(strsplit(names(probabilities)[1], separator)[[1]])-1 #espaço vazio no início
for(w in names(probabilities)){
  possible_transitions = paste(
    paste(strsplit(w, separator)[[1]][c(1,3:(d+1))], collapse = separator), A, sep = separator
  )
  M[w,possible_transitions] = probabilities[[w]]
}

Q = ifelse(M>0,T,F)

pdf('esqueleto_2.pdf', width = 6, height = 6)
par(c(0,4,4,2))
plot(Q, asp=T, main='',xlab='',ylab='', key = NULL, 
     axis.col=NULL, axis.row=NULL, border = NULL)
dev.off()
