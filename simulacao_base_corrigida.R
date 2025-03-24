#setup
library(VLMC)
library(stringr)
library(PST)
library(rbenchmark)

contextos = c(
  '000', '100', '10', '001', '101', '011', '111' #"mais recente" na direita, deve ser lido ao contrário <-
)

probabilidades = c(
  0.4, 0.3, 0, 0.6, 0.5, 0.8, 0 #é impossível obter o contexto "101", já que p[1|10]=0, mas achei bom manter por enquanto
)

#função 1

sim_cemav_bin = function(contextos, probabilidades, n, amostra_inicial = c(), text = T, show_process=F){
  k = max(nchar(contextos))
  if(show_process) print(paste("ordem:", k))
  if(length(amostra_inicial)==0) amostra_inicial = sample(c(0,1),2*k,TRUE)
  
  l = length(amostra_inicial)
  if(show_process) print(paste("amostra inicial:", paste(amostra_inicial, collapse="")))
  
  progresso = txtProgressBar(min = 0, max = n, initial = 0)
  
  amostra = character(l+n)
  amostra[1:l] = amostra_inicial
  
  for(i in (l+1):(n+l)){
    passado_relevante = paste(amostra[i-(k:1)], collapse = '')
    
    c = which(apply(array(contextos), 1, function(x) grepl(glue::glue("{x}$"),passado_relevante)))
    
    if(length(c)==0){
      stop(glue::glue("SEM CONTEXTO PARA {passado_relevante}"))
    }
    
    else{
      x = sample(c(0,1),1,T,c(1-probabilidades[c],probabilidades[c]))
      amostra[i] = x
    }
    if(show_process) if(i %in% round((1:100)*(n+l)/100, digits = 0))print(glue::glue("{round((i)*100/(n+l))}%"))
  }
  amostra = amostra[(l+1):(n+l)]
  
  if(text) return(paste(amostra, collapse = ""))
  else return(amostra)
}

#função 2: talvez seja mais rapido com strings



sim_cemav_bin_2 = function(contextos, probabilidades, n, amostra_inicial = c(), text = T, show_process=F){
  k = max(nchar(contextos))
  if(show_process) print(paste("ordem:", k))
  if(length(amostra_inicial)==0) amostra_inicial = paste(sample(c(0,1),2*k,TRUE), collapse='')
  
  l = nchar(amostra_inicial)
  if(show_process) print(paste("amostra inicial:", amostra_inicial))
  
  amostra = amostra_inicial
  
  for(i in (l+1):(n+l)){
    passado_relevante = substr(amostra, nchar(amostra)-k+1, nchar(amostra))
    
    c = which(apply(array(paste0(contextos,"$")), 1, function(x) grepl(x,passado_relevante)))
    
    if(length(c)==0){
      stop(glue::glue("SEM CONTEXTO PARA {passado_relevante}"))
    }
    
    else{
      x = sample(c(0,1),1,T,c(1-probabilidades[c],probabilidades[c]))
      amostra = paste0(amostra,x)
    }
    if(show_process) if(i %in% round((1:100)*(n+l)/100, digits = 0))print(glue::glue("{round((i)*100/(n+l))}%"))
  }
  amostra = substr(amostra, l+1,n+l)
  
  if(text) return(amostra)
  else return(strsplit(amostra, ''))
}


testes1 = benchmark(sim_cemav_bin(contextos, probabilidades, 1000),
                   sim_cemav_bin_2(contextos, probabilidades, 1000),
                   replications = 30) #2 é aprox 8 vezes mais rápida

testes2 = benchmark(sim_cemav_bin(contextos, probabilidades, 10000),
                   sim_cemav_bin_2(contextos, probabilidades, 10000),
                   replications = 30) #2 é aprox 4 vezes mais rápida

testes3 = benchmark(sim_cemav_bin_2(contextos, probabilidades, 1000),
                   sim_cemav_bin_2(contextos, probabilidades, 10000),
                   sim_cemav_bin_2(contextos, probabilidades, 100000, show_process = T),
                   sim_cemav_bin_2(contextos, probabilidades, 1000000, show_process = T),
                   replications = 1) #falta testar, o ultimo demora muito, acho que o crescimento é maior que linear

testes4 = benchmark(sim_cemav_bin(contextos, probabilidades, 100),
                   sim_cemav_bin(contextos, probabilidades, 1000),
                   sim_cemav_bin(contextos, probabilidades, 10000),
                   sim_cemav_bin(contextos, probabilidades, 100000),
                   replications = 1) #crescimento parece ser linear: 0.06, 0.71, 7.07, 69.71 = 1 11.8 117.8 1161.8 (relativo)


####testes com árvore completa
set.seed(247005)
contextos_ic = c(
  "10", #2
  "111", #3
  "1100",
  "1101", #4
  "10100",
  "10001",
  "00101",
  "10101", #5
  "100000",
  "010000",
  "001000",
  "101000",
  "000001",
  "011001",
  "100011",
  "010011",
  "001011",
  "011011", #6
  "0000000",
  "1110000",
  "1011000",
  "0111000",
  "0000100",
  "1100100",
  "1100001",
  "0001001",
  "0111001",
  "0000011",
  "0101011",
  "0111011",
  "1111011", #7
  "01000000",
  "00110000",
  "00011000",
  "10011000",
  "11111000",
  "11000100",
  "01001001",
  "11001001",
  "00101001",
  "10101001",
  "01101001",
  "01111001",
  "01000011",
  "11000011",
  "10110011",
  "11110011", #8
  "011000000",
  "010110000",
  "110110000",
  "101111000",
  "001000100",
  "101000100",
  "100100100",
  "010100100",
  "110100100",
  "100100001",
  "010100001",
  "110100001",
  "111101001",
  "011111001",
  "111111001",
  "100110011",
  "001110011",
  "101110011",
  "101101011",
  "011101011", #9
  "0111000000",
  "1111000000",
  "0001111000",
  "1001111000",
  "0000100100",
  "1000100100",
  "0000100001",
  "1000100001",
  "0011101001",
  "1011101001",
  "0000110011",
  "1000110011",
  "0001101011",
  "1001101011",
  "0111101011",
  "1111101011" #10
)

probabilidades_ic = numeric(length(contextos_ic))
p = 0.3
probabilidades_ic[1:2] = c(0, 0)
for(i in 3:length(probabilidades_ic)){
  prob = rnorm(1, mean = p, sd = 0.2)
  if(prob>=1) prob = 0.95
  if(prob<=0) prob = 0.05
  probabilidades_ic[i] = prob
  print(prob)
  p = 1-prob
}

testes5 = benchmark(sim_cemav_bin(contextos_ic, probabilidades_ic, 1000, show_process = T),
                   sim_cemav_bin_2(contextos_ic, probabilidades_ic, 1000, show_process = T),
                   replications = 1) #2 é aprox 10 vezes mais rápida

testes6 = benchmark(sim_cemav_bin(contextos_ic, probabilidades_ic, 10000, show_process = T),
                   sim_cemav_bin_2(contextos_ic, probabilidades_ic, 10000, show_process = T),
                   replications = 1) #2 é aprox 10 vezes mais rápida

#a diferença parece escalar parecido (ate melhor) para árvores grandes
#último teste:

testes7 = benchmark(sim_cemav_bin(contextos_ic, probabilidades_ic, 10^5, show_process = T),
                   sim_cemav_bin_2(contextos_ic, probabilidades_ic, 10^5, show_process = T),
                   replications = 1) #falta rodar



#testando ajuste de funções

amostra = sim_cemav_bin_2(contextos, probabilidades, 10000)
summary(vlmc(str_split_1(amostra,''), alpha.c = 0.001)) #parece funcionar com alphas mais baixos, será algum erro no código original?

amostra_ic = readLines("amostra_o10_k3.txt") #amostra problematica, rodar de novo e excluir essa.
a = substr(amostra_ic, 1, 10000)
t = summary(vlmc(str_split_1(a,''), alpha.c = 0.005))
draw(t)
