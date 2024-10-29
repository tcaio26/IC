library(CEMAV)

set.seed(247005)

pk = function(k, a, b, c, d){
  p = ifelse(k==0, a,
             ifelse(k==1, b,
                    ifelse(k==2, c, d)))
  return(p)
}

fonte1 = cemav_bin(1000, qk = function(x) pk(x, a=0,b=0.7,c=0.8,d=0.75), q_inf=0.75, q0 = 0)
fonte2 = cemav_bin(1000, qk = function(x) pk(x, a=0,b=0.8,c=0.6,d=0.7), q_inf=0.7, q0 = 0)


#utilizando algoritmo SMC

#parte1: algoritmo BIC

i = l = 0
u = 100
