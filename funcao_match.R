#APENAS PARA A FUNÇÃO QUE ENCONTRA AS POSIÇÕES X_i TQ CONTEXTO = W

#amostra para validação
# set.seed(247005)
# a = sample(c(0,1),20,T)
# a_txt = paste0(a, collapse='')

match = function(str, p){ 
  pos = stringr::str_locate_all(str, p)[[1]][, "end"]
  return(pos+1)
}
# 
# contexto_teste = '11'
# 
# match(a_txt, contexto_teste)
# 
# a[3:5]
# a[6:8]
# a[10:12]
