library(skeleton)
library(tibble)
set.seed(247005)
skel_contexts = list(
  '00' = c(1,1,0,1), '300' = c(0,1,0,0), '12' = c(1,0,1,0), '3' = c(1,1,0,0) #transições permitidas
)


c = max(nchar(skel_contexts)) #parâmetros, alguns serão automáticos
d = 5 #testando com 3, mudar p 5
A = c('0','1','2','3')
a = length(A)


#criar todos os sufixos de tamanho d possível
suffixes = apply(expand.grid(replicate(d, A, simplify = FALSE)), 1, paste0, collapse = "")
length(suffixes) #deve ser a^d

###### FUNÇÕES AUXILIARES
#para uma lista/vetor de contexto e uma string s, retorne o maior contexto que seja parte do final de s.
#usando o skel_contexts mesmo
getMaxContext = function(contexts, string){
  candidates = contexts[sapply(contexts, function(str) grepl(paste0(str,'$'), string))]
  if(length(candidates)>0) return(candidates[[which.max(nchar(candidates))]])
  return(NULL)
}

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
  if(is.null(cont)) transitions = c(1,1,1,1) 
  else transitions = skel_contexts[[cont]]
  
  if(sum(transitions)==1) probabilities[[s]] = transitions
  else probabilities[[s]][which(transitions==1)] = generate_probs(sum(transitions))
}
names(probabilities) = suffixes

lobstr::obj_size(probabilities)

#transformar uma lista nomeada em matriz de probabilidades
M = matrix(0, nrow = length(probabilities), ncol = length(probabilities),
           dimnames = list(names(probabilities), names(probabilities)))

d = nchar(names(probabilities)[[1]])
for(w in names(probabilities)){
  possible_transitions = paste0(substr(w,2,d),A)
  M[w,possible_transitions] = probabilities[[w]]
}

#reverso
probabilities = replicate(a^d, rep(0,a), simplify = FALSE)
names(probabilities) = rownames(M)
for(w in rownames(M)){
  probabilities[[w]] = as.vector(unname(M[w,paste0(substr(w,2,d),A)]))
}
lobstr::obj_size(probabilities)

#adaptar simulação binária usada
sim_cemav = function(n, probabilidades, amostra_inicial = c(),
                     alphabet = unique(string_to_vec(amostra_inicial)), text = T, show_process=F){
  d = nchar(names(probabilidades)[1])
  if(show_process) print(paste("ordem:", d))
  if(length(amostra_inicial)==0){
    if(missing(alphabet)) stop("if no initial sample is given, an alphabet must be provided")
    amostra_inicial = paste(sample(alphabet, d*2, T), collapse = '')
  }
  if(missing(alphabet)) alphabet = sort(alphabet)
  if(!missing(amostra_inicial) && !all(unique(string_to_vec(amostra_inicial))%in%alphabet)){
    stop("starting sample contains a symbol not present in the alphabet.")
  }
  l = nchar(amostra_inicial)
  if(show_process) print(paste("amostra inicial:", amostra_inicial))
  
  amostra = amostra_inicial
  
  for(i in (l):(n+l)){
    shortPast = substr(amostra, i-d+1, i)
    
    if(is.null(probabilidades[[shortPast]])) stop(glue::glue("SEM CONTEXTO PARA {shortPast}"))
    
    x = sample(alphabet, 1, prob = probabilidades[[shortPast]])
    amostra = paste0(amostra, x)
    
    if(show_process) if(i %in% round((1:100)*(n+l)/100, digits = 0))print(glue::glue("{round((i)*100/(n+l))}%"))
  }
  amostra = substr(amostra, l+1,n+l)
  
  if(text) return(amostra)
  else return(strsplit(amostra, ''))
}
t = '120120210300'
amostra = sim_cemav(1e5, probabilities, alphabet = c('0','1','2','3'), show_process = T)

cat(amostra, file = 'amostra_a_4_1M.txt')

