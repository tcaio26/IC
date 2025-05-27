# n1 = c(F, T, T, F, T, T, F, T, T,T)
# d1 = c(-1,-1,-1,0,0,0,1,1,1,0)
# n2 = c(F,F,T,F,F,T,F,F,T,F)
# d2 = c(-1,-1,-1,-1,-1,-1,0,0,0,0)

decision = function(n1, n2, d1, d2, debug=F){
  #algum nodo é grande suficiente?
  c1 = !any(n1,n2) #TRUE -> corta
  #ambos os nodos tem dom = -1?
  c2 = (d1==-1 && d1==d2) 
  #dom iguais mas apenas um é significante
  c3 = !(d1==d2 && xor(n1, n2))
  #comparação de dom normal
  c4 = (d1==d2)
  
  if(debug) return(c(c1,c2,c3,c4))
  return(ifelse(c1, TRUE,
                ifelse(c2, TRUE,
                       ifelse(c3, c4, FALSE))))
}

# for(i in 1:10){
#   # t = decision(n1[i],n2[i],d1[i],d2[i], debug = T)
#   # print(t)
#   print(decision(n1[i],n2[i],d1[i],d2[i]))
# }
