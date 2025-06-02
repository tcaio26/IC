###DEMONSTRAÇÃO
library(skeleton)
#Amostra está em simulacao_base.R

amostra = readLines('amostra_skel_100k.txt')

##Função geral
(skel = generate_skeleton(amostra, alpha = 0.05, sensibility = 0.01))

skel$skel
skel$matrix


#Funções separadas
(Nmin = log(0.05,0.99))
(arvore = startskel(amostra, 299, prob = T))

(esqueleto = sculptskeleton(arvore, 299, copy = T, print = T))

ToDataFrameTree(esqueleto, 'context','p','dom','n')
ToDataFrameTree(esqueleto, 'context','p','dom','n', filterFun = isLeaf)[,-1]

(M = skel_matrix(esqueleto))

k = ncol(M)
M_sum = M
M_prod = M
for(i in 2:k){
  M_prod = M_prod%*%M
  M_sum = M_sum + M_prod
}
