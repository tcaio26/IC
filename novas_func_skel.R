source('func_conversao.R')
source('funcao_match.R')
library(data.tree)


startskel = function(sample_txt, Nmin, prob = F){
  sample_vec = string_to_vec(sample_txt)
  raiz = Node$new("r")
  raiz$index = 1:length(sample_vec)
  raiz$n = length(sample_vec)
  raiz$context = ''
  raiz$dom = -1
  if(prob) raiz$p = sum(sample_vec)/raiz$n
  
  genskel(raiz, sample_vec, sample_txt, Nmin, prob = prob)
  return(raiz)
}


genskel = function(node, sample_vec, sample_txt, Nmin, prob = F){
  #resultado 0
  l = node$AddChild(paste0('0',node$name))
  w = paste0('0',node$context)
  i = match(sample_txt, w)
  n = ifelse(i==0, 0, length(i))
  l$context = w
  l$index = i
  l$n = length(i)
  l$dom = ifelse(sum(sample_vec[i])%in%c(0,n), sample_vec[i][1], -1)
  if(prob) l$p = sum(sample_vec[i])/n
  if(length(i)>=Nmin){
    genskel(l, sample_vec, sample_txt, Nmin, prob)
  }
  
  
  #resultado 1
  r = node$AddChild(paste0('1',node$name))
  w = paste0('1',node$context)
  i = match(sample_txt, w)
  n = ifelse(i==0, 0, length(i))
  r$context = w
  r$index = i
  r$n = length(i)
  r$dom = ifelse(sum(sample_vec[i])%in%c(0,n), sample_vec[i][1], -1)
  if(prob) r$p = sum(sample_vec[i])/n
  if(length(i)>=Nmin){
    genskel(r, sample_vec, sample_txt, Nmin, prob)
  }
}

t = startskel('01000110', 1, T)
df = ToDataFrameTree(t, 'context','n','p')
df = df[df$n>0,]
df[order(nchar(df$context)),]
