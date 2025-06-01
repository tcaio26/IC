source('func_conversao.R')
source('funcao_match.R')
library(data.tree)


startskel = function(sample_txt, Nmin, prob = F, debug = F){
  sample_vec = string_to_vec(sample_txt)
  raiz = Node$new("r")
  raiz$index = 1:length(sample_vec)
  raiz$n = length(sample_vec)
  raiz$context = ''
  raiz$dom = -1
  if(prob) raiz$p = sum(sample_vec)/raiz$n
  
  genskel(raiz, sample_vec, sample_txt, Nmin, prob = prob, debug)
  return(raiz)
}


genskel = function(node, sample_vec, sample_txt, Nmin, prob = F, debug = F){
  #resultado 0
  l = node$AddChild(paste0('0',node$name))
  w = paste0('0',node$context)
  if(debug) print(paste0('contexto ',w))
  i = match_str(sample_txt, w)
  if(any(i=='no matches')){
    l$context = w
    l$index = numeric(0)
    l$n = 0
    l$dom = 0
    if(prob) l$p = numeric(0)
    if(debug) print(paste0('nodo ',w,' vazio, OK'))
  }
  else{
    n = length(i)
    l$context = w
    l$index = i
    l$n = length(i)
    l$dom = ifelse(sum(sample_vec[i])%in%c(0,n), (sample_vec[i][1] - (1-sample_vec[i][1])), 0) #0 vira -1, 1 continua
    if(prob) l$p = sum(sample_vec[i])/n
    if(debug) print('OK')
    if(n>=Nmin && l$dom==0){
      genskel(l, sample_vec, sample_txt, Nmin, prob, debug)
    } 
  }
  
  
  #resultado 1
  r = node$AddChild(paste0('1',node$name))
  w = paste0('1',node$context)
  i = match_str(sample_txt, w)
  if(any(i=='no matches')){
    r$context = w
    r$index = numeric(0)
    r$n = 0
    r$dom = 0
    if(prob) r$p = numeric(0)
    if(debug) print(paste0('nodo ',w,' vazio, OK'))
  }
  else{
    n = length(i)
    r$context = w
    r$index = i
    r$n = length(i)
    r$dom = ifelse(sum(sample_vec[i])%in%c(0,n), (sample_vec[i][1] - (1-sample_vec[i][1])), 0)
    if(prob) r$p = sum(sample_vec[i])/n
    if(debug) print('OK')
    if(n>=Nmin && r$dom==0){
      genskel(r, sample_vec, sample_txt, Nmin, prob, debug)
    } 
  }
}

t = startskel('01000110', 1, T, T)
df = ToDataFrameTree(t, 'context','n','p', 'dom')
df = df[df$n>0,]
df
df[order(nchar(df$context)),]
# 
# library(rbenchmark)
# 
# amostra = readLines('amostra_skel_100k.txt')
# 
# amostra1000 = vec_to_string(string_to_vec(amostra)[1:1000])
# amostra10000 = vec_to_string(string_to_vec(amostra)[1:10000])
# 
# benchmark(
#   startskel(amostra1000, 100, T), #0.146, relativo 1
#   startskel(amostra10000, 100, T), #2.495, relativo 17.089
#   startskel(amostra, 100, T), #68.477, relativo 469.021
#   replications = 10, order = c(1,2,3)
# )
# 
# library(microbenchmark)
# tam = numeric(100)
# tempo = numeric(100)
# amostra = sample(c(0,1),1e5,T)
# s = sample(seq(1e3,1e5,1e3))
# for(i in 1:100){
#   tam[i] = s[i]
#   a = vec_to_string(amostra[1:(s[i])])
#   t = microbenchmark(startskel(a, 100, T), times = 5)
#   tempo[i] = t$time[4]
#   print(paste('rep',i,'tam',s[i], sep=' '))
# }
# df_bench = data.frame(tamanho_string = tam, tempo = tempo)
# pdf('tempo_genskel_sem_prune.pdf')
# ggplot2::ggplot(data = df_bench, ggplot2::aes(x=tam, y = tempo))+
#   ggplot2::geom_point(color = 'darkred')+
#   ggplot2::theme_bw()+
#   ggplot2::labs(title='Tempo por tamanho de string, com Nmin=100, sem corte pós árvore')
# dev.off()

source('funções/startskel.R')
source('funções/sculptskeleton.R')
teste = readLines('amostra_skel_100k.txt')

arvore = startskel(teste, 1000, prob = T)
ToDataFrameTree(arvore, 'n', 'p', 'dom')
skel_teste = sculptskeleton(arvore, 1000, copy = T)

ToDataFrameTree(skel_teste, 'context', 'n', 'p','dom')
