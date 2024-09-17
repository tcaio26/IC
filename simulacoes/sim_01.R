library(pacman)
p_load(tidyverse, GGally)
#processo estocástico simples

l = 5*10^3

p0 = 0.2
p1 = 0.7

h = function(x){return(p0+(p1-p0)*x)}

sim = data.frame(t = numeric(l), x = numeric(l), reg = logical(l))
probs = data.frame(t = 1, q00=0, q01=0, q10=0, q11=0)

start = sample(c(0,1),1)

for(i in 1:l){
  u = runif(1)
  if(i==1) x= start
  if(i>1) x = ifelse(u>h(sim$x[i-1]),1,0)
  reg = ifelse(i==1,T,u<=p0|u>=p1)
  sim[i,1]=i
  sim[i,2]=x
  sim[i,3]=reg
  
  if(i==1){q00=0; q01=0; q10=0; q11=0;q0=0;q1=0}
  if(i>1){
    if(sim$x[i-1]==0 & sim$x[i]==0){q00 = q00+1; q0=q0+1}
    if(sim$x[i-1]==0 & sim$x[i]==1){q01 = q01+1; q0=q0+1}
    if(sim$x[i-1]==1 & sim$x[i]==0){q10 = q10+1; q1=q1+1}
    if(sim$x[i-1]==1 & sim$x[i]==1){q11 = q11+1; q1=q1+1}
  }
  if(i>1) probs = rbind(probs, c(i,q00/q0,q01/q0,q10/q1,q11/q1))
}

ggplot(data=probs,aes(x=t))+
  geom_path(aes(y=q00), color = 'blue')+
  geom_path(aes(y=q01), color = 'green')+
  geom_path(aes(y=q10), color = 'red')+
  geom_path(aes(y=q11), color = 'yellow')+
  scale_x_continuous(trans='log10')+
  scale_y_continuous( breaks = seq(0,1,0.05))+
  theme_bw()


#moedas com apenas uma U, acoplamento

moedas = data.frame(m1 = 0, m2 = 0, m3 = 0, m4 = 0)

p = c(0.5, 0.7, 0.2, 0.5)
l = 10^4
for(i in 1:l){
  if(i==l) moedas = moedas[-1,]
  u = runif(1)
  m1 = ifelse(u<p[1],0,1)
  m2 = ifelse(u<p[2],1,0)
  m3 = ifelse(u<p[3],1,0)
  m4 = ifelse(u<p[4],1,0)
  moedas = rbind(moedas, c(m1,m2,m3,m4))
}
cor(moedas) #as probabilidades são igual o que se espera, mas as moedas são altamente correlacionadas.
moedas %>% pivot_longer(1:4, names_to = 'moeda', values_to = 'cara') %>% group_by(moeda) %>% 
  summarise(p = sum(cara)/l)


#simulação de autoregressão binaria com q(x) = 1/[k(1+exp(-2x))]

z = function(x) (2*(1+exp(-2*x)))^(-1)
theta = function(k) k^(-2)

serie = function(n, soma=F){
  for(i in 1:100){
    u = runif(1)
    if(i ==1){
      x = ifelse(u<z(theta(0)),1,-1)
      serie = c(x)
    }
    else if(i>1){
      soma = 0
      for(j in 1:(i-1)){
        soma = soma + theta(j)*serie[i-j]
      }
      x = ifelse(u<z(theta(0)+soma),1,-1)
      serie = append(serie, x)
    }
  }
  if(!soma) return(serie)
  if(soma) return(sum(serie))
}

series = c()
for(i in 1:1000) series = append(series, serie(100,T))
hist(series)
