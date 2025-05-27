#APENAS PARA A FUNÇÃO QUE ENCONTRA AS POSIÇÕES X_i TQ CONTEXTO = W

#amostra para validação
# set.seed(247005)
# a = sample(c(0,1),20,T)
# a_txt = paste0(a, collapse='')

#o pacote stringi tem o parametro overlap = T, maravilhoso.

match_str = function(str, p){
  if(p == '') return(1:nchar(str))
  pos = stringi::stri_locate_all_fixed(str, p, overlap = T)[[1]][, "end"] +1 #retorna as posições logo após o contexto
  pos = pos[pos<=nchar(str)] #evita ultrapassar o index maximo
  if(all(is.na(pos))) pos = 'no matches'
  return(pos)
}
# 
# contexto_teste = '11'
# 
# match(a_txt, contexto_teste)
# 
# a[3:5]
# a[6:8]
# a[10:12]
