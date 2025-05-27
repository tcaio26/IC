interval_check = function(x,a,b,inc.L=F,inc.U=F){
  L = ifelse(inc.L,x>=a,x>a)
  U = ifelse(inc.U,x<=b,x<b)
  return(L&U)
}