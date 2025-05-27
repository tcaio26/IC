vec_to_string = function(vector){
  rawToChar(as.raw(vector+48L))
}

# teste = sample(c(0,1),1e5,T)
# rbenchmark::benchmark(vec_to_string(teste), replications = 1e5) #10.2 sec