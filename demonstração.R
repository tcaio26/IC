###DEMONSTRAÇÃO
setwd("Documents/codigos/r_data/IC")
library(skeleton)
#Geração da amostra: contextos
View(read.csv('parametros_geradores_amostra.csv'))

#Amostra está em simulacao_base.R

amostra = readLines('amostra_skel_100k.txt', warn = FALSE)

##Função geral
(skel = generate_skeleton(amostra, alpha = 0.05, sensibility = 0.01))

skel$skel
skel$matrix

#Limpando
rm(skel)
#Funções separadas
(Nmin = ceiling(log(0.05,0.99)))
(arvore = startskel(amostra, Nmin, prob = T))

(esqueleto = sculptskeleton(arvore, 299, copy = T, print = T))

ToDataFrameTree(esqueleto, 'context','p','dom','n')
ToDataFrameTree(esqueleto, 'context','p','dom','n', filterFun = isLeaf)[,-1]

(M = skel_matrix(esqueleto))


#Propriedades de M
irreductible(M)
