#simulação de uma cadeia de memoria variavel A = {0,1} P[x=1|x_] = q_k, k = inf{n: x_(-n) = 1}

library(CEMAV)

seq_inicial = sample(c(0,1),100,T)
seq_inicial_inf = rep(0,100)

cadeia = cemav_bin(10000,passado = seq_inicial)

plot(cumsum(cadeia$serie), type='s')
plot(diff(cadeia$regen), type = 's')
hist(log(diff(cadeia$regen)))
plot(cadeia$prob, type = 'l')

max_t = c()
for(i in 1:1000){
  c = cemav_bin(10^3)
  max_t = append(max_t, max(diff(c$regen)))
}
hist(max_t, breaks = 50)

probs = c()
for(i in 1:1000){
  c = cemav_bin(10^3)
  probs = append(probs, c$prob[1001])
}
hist(probs, breaks = 50)

cadeia_null = cemav_bin(1000, seq_inicial_inf, q_inf = 0.1)

plot(cumsum(cadeia_null$serie), type='s')
plot(diff(cadeia_null$regen), type = 's')
plot(cadeia_null$prob, type = 'l')


cadeia_finit = cemav_bin(10^5, q0 = 1/2, qk = function(x) 1/log(x+2))
plot(cumsum(cadeia_finit$serie), type='s')
plot(diff(cadeia_finit$regen), type = 's')
hist(diff(cadeia_finit$regen))
plot(cadeia_finit$prob, type = 'l')
plot(cadeia_finit$serie)
plot(cadeia_finit$q, type = 'l')

cbind(ind = 1:100001, soma = cadeia_finit$prob) %>% as.data.frame() %>% ggplot2::ggplot(ggplot2::aes(x = log10(ind), y = soma))+
  ggplot2::geom_path()

plot(cumsum(cemav_bin(10^4, q0=1/2, qk = function(x) 1/log(x+2))$serie))
