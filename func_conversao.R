#FUNÇÕES DE CONVERSÃO

vec_to_string = function(vector){
  rawToChar(as.raw(vector+48L))
}

# teste = sample(c(0,1),1e5,T)
# rbenchmark::benchmark(vec_to_string(teste), replications = 1e5) #10.2 sec

string_to_vec = function(string){
  as.numeric(charToRaw(string))-48L
}

# teste = vec_to_string(sample(c(0,1),1e5,T))
# teste2 = vec_to_string(sample(c(0,1),1e3,T))
# rbenchmark::benchmark(string_to_vec(teste), replications = 1e5) #46 sec
# rbenchmark::benchmark(string_to_vec(teste2), replications = 1e5) #0.7 sec
