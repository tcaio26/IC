amostra = sample(c(0,1),1000,T) #entradas
sensib = 0.1
alpha = 0.05


match = function(str, p){ 
  pos = stringr::str_locate_all(str, p)[[1]][, "start"]
  return(pos)
} #funções


amostra_string = paste(amostra, collapse='')
n_min = ceiling(log(alpha, base = 1-sensib))
n_smp = length(amostra)
alphabet = unique(amostra)

val = list(index = 1:n_smp, n = n_smp)

init.raiz = function(val){
  contexto = 'r'
  nivel = 0
  cont = c(sum(amostra[val$index]), val$n-sum(amostra[val$index]))
  gamma = ifelse(cont[1]==0 || cont[2]==0, sensib^(val$n), 1)
  filhos_logico = (gamma>=alpha && val$n>=n_min)
  filhos = list()
  val = val
  return(list(contexto = contexto, nivel = nivel, cont = cont, 
              gamma = gamma, filhos_logico = filhos_logico, 
              filhos = filhos, val = val))
}

raiz = init.raiz(val)

init.nod = function(pai, simbolo){
  
}