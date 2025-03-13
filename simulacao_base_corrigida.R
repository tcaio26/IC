#setup

library(VLMC)
library(stringr)

contextos = c(
  '000', '100', '10', '001', '101', '011', '111' #"mais recente" na direita, deve ser lido ao contrário <-
)

probabilidades = c(
  0.4, 0.3, 0, 0.6, 0.5, 0.8, 0 #é impossível obter o contexto "101", já que p[1|10]=0, mas achei bom manter por enquanto
)

#função

sim_cemav_bin = function(contextos, probabilidades, n, amostra_inicial = c(), text = T, show_process=F){
  k = max(nchar(contextos))
  if(show_process) print(paste("ordem:", k))
  if(length(amostra_inicial)==0) amostra_inicial = sample(c(0,1),2*k,TRUE)
  
  l = length(amostra_inicial)
  if(show_process) print(paste("amostra inicial:", amostra_inicial))
  
  for(i in 1:n){
    passado_relevante = paste(amostra[length(amostra)-((k-1):0)], collapse = '')
    
    c = which(apply(array(contextos), 1, function(x) grepl(glue::glue("{x}$"),passado_relevante)))
    
    if(length(c)==0){
      stop(glue::glue("SEM CONTEXTO PARA {passado_relevante}"))
    }
    
    else{
      amostra = append(amostra, sample(c(0,1),1,T,c(1-probabilidades[c],probabilidades[c])))
    }
  }
  amostra = amostra[(l+1):(n+l)]
  
  if(text) return(paste(amostra, collapse = ""))
  else return(amostra)
}

#teste de ajuste

teste = sim_cemav_bin(contextos, probabilidades, 10000, text = T)

mod_teste_2 =pstree(seqdef(teste, stsep = ''), L=5)

mod_teste_2_pruned = prune(mod_teste_2, gain="G1", C=1, delete=FALSE)
plot(mod_teste_2_pruned)
