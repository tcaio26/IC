#FUNÇÃO GERAL

generate_skeleton = function(sample, Nmin=59, sensibility, alpha){
  use_nmin = !missing(Nmin)
  #checks
  if(!is.numeric(sample)&&!all(is.character(sample),length(sample)==1)){
    stop("Sample must be a numeric vector or a string")
  }
  if(is.character(sample)) sample = as.numeric(unlist(strsplit(sample,'')))
  if(!all(unique(sample)%in%c(0,1))) stop("Sample must be comprised of 0 and 1 values")
  if(length(sample)<2) stop("Sample must be at least 2 symbols long")
  
  if(!(missing(alpha)||(is.double(alpha) && interval_check(alpha,0,1)))) stop("alpha must be a numeric value between 0 and 1")
  if(!(missing(sensibility)||(is.double(sensibility) && interval_check(sensibility,0,0.5)))) stop("sensibility must be a numeric value between 0 and 0.5")
  if(!(missing(Nmin)||(is.numeric(Nmin) && Nmin>0))) stop("Nmin must be a positive integer") 
  
  if(all(missing(sensibility), missing(alpha), missing(Nmin))){
    warning("No tolerance arguments provided, proceeding with default Nmin = 59")
    use_nmin=T
  }
  
  if(xor(missing(sensibility),missing(alpha))){
    warning("Provided only alpha or sensibility. Provide both or none. Proceeding with ", Nmin)
    use_nmin=T
  }
  if(!missing(Nmin)&&length(sample)<=Nmin) stop("Sample length must be larger than Nmin, preferably a lot larger")
  
  #generating function
  if(!use_nmin) Nmin = ceiling(log(alpha, 1-sensibility))
  if(Nmin>=length(sample)) stop("Nmin too large for sample")
  #tree = genskel(...)
  
  #output
  
  return(Nmin)
}