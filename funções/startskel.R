#inicia uma arvore
library(data.tree)
source('funções/genskel.R')
source('funções/string_to_vec.R')
source('funções/vec_to_string.R')
source('funções/match_str.R')

startskel = function(sample_txt, Nmin, prob = F, debug = F){
  sample_vec = string_to_vec(sample_txt)
  raiz = Node$new("r")
  raiz$index = 1:length(sample_vec)
  raiz$n = length(sample_vec)
  raiz$context = ''
  raiz$dom = 0
  if(prob) raiz$p = sum(sample_vec)/raiz$n
  
  genskel(raiz, sample_vec, sample_txt, Nmin, prob = prob, debug)
  return(raiz)
}