library(skeleton)
symbolcounts = function(str, pattern, alphabet){
  positions = match_str(str, pattern)
  if(all(positions == 'no matches')) return(numeric(length(alphabet)))
  vec = string_to_vec(str)[positions]
  sapply(as.numeric(alphabet), function(s) sum(vec==s))
}