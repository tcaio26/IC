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


#Propriedades de M
irr = irreductible(M, ReturnMatrix = T)
irr$irred
irr$Q #se existir uma transição proibida com contexto < k, onde k é a ordem do esqueleto, M não é irredutível.
