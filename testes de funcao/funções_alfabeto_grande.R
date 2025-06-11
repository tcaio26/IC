library(skeleton)
string_teste = vec_to_string(sample(1:8,1e6,T))

length(match_str(string_teste, '555')) #aprox 2000 casos, exemplo bom

alfabeto = vec_to_string(1:8) |> strsplit('') |> unlist()

fun1 = function(str, pattern, alphabet){
  positions = match_str(str, pattern)
  vec = string_to_vec(str)[positions]
  sapply(as.numeric(alphabet), function(s) sum(vec==s)/length(vec))
}
fun1('5551555155555552555', '', alfabeto) #acho que tá funcionando

fun2 = function(str, pattern, alphabet){
  t = stringi::stri_count_fixed(str, pattern = pattern, overlap = T)
  sapply(alphabet, function(s) stringi::stri_count_fixed(str = str, pattern = paste0(pattern,s), overlap=T))/
    ifelse(grepl(paste0(pattern,'$'),str),t-1,t)
}
fun2('5551555155555552555', '', alfabeto) |> sum()

str = vec_to_string(sample(1:8,1e5,T))
rbenchmark::benchmark(fun1(str,'555',alfabeto),
                      fun2(str,'555',alfabeto))

str = vec_to_string(sample(1:8,1e6,T))
rbenchmark::benchmark(fun1(str,'555',alfabeto),
                      fun2(str,'555',alfabeto))

str = vec_to_string(sample(1:8,1e7,T))
rbenchmark::benchmark(fun1(str,'555',alfabeto),
                      fun2(str,'555',alfabeto))

#fun2 constantemente demora 2.4 vezes mais, usar 1.
