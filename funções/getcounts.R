library(skeleton)
getcounts = function(str, pattern, alphabet){
  positions = match_str(str, pattern)
  vec = string_to_vec(str)[positions]
  sapply(as.numeric(alphabet), function(s) sum(vec==s)/length(vec))
}