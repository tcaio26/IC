# n1 = c(F, T, T, F, T, T, F, T, T,T)
# d1 = c(0,0,0,-1,-1,-1,1,1,1,-1)
# n2 = c(F,F,T,F,F,T,F,F,T,F)
# d2 = c(0,0,0,0,0,0,-1,-1,-1,-1)
# 
# c1 = n1*d1
# c2 = n2*d2

decision = function(n1, n2, d1, d2, debug=F){
  c1 = n1*d1 #w0 n>Nmin e dom!=0
  c2 = n2*d2 #w1 n>Nmin e dom!=0
  
  if(debug) print(paste(n1, d1, '->',c1, '||||||',n2,d2,'->',c2))
  if(c1+c2 + c1*c2== 0) return(TRUE) # sem transições proibidas significantes
  else if(c1 == c2) return(TRUE) # mesma transição proibida e significante, não deve acontecer
  else return(FALSE)
}

# for(i in 1:10){
#   print(decision(n1[i],n2[i],d1[i],d2[i], debug = F))
#   print('          ')
# }
