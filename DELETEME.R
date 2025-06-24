# funções necessárias:
#   
#   generate_skeleton()
#   startskel()
#   genskel()
#   sculptskeleton()
#   shouldyoucut()
#   getMaxContext()
#   extractTransitions()
#   transitionsToMatrix()
#   matrixToTransitions()
library(skeleton)
amostra = strsplit(readLines('amostra_chuva_4_100k.txt'),' ')[[1]]
A = c('sol','nuvem','chuva','tempestade')

#generate_skeleton
alpha = 0.05
sensib = 0.01
Nmin = log(alpha, 1-sensib)-log(length(A), 1-sensib)

skel = startskel(amostra, A, Nmin) #startskel e dependencias: genskel
sculptskeleton(skel, Nmin) #sculptskel e dependencias: shouldyoucut
print(skel, 'context','n','transitions')

trans = expandTransitions(skel, A, '-') #expand e dependencias: getMaxContext
M = transToMatrix(trans, A, '-') #transToMatrix e matrixToTrans, e dep: 

irreductible(M) #tudo rodando ok.
