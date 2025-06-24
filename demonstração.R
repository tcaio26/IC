###DEMONSTRAÇÃO
setwd("Documents/codigos/r_data/IC")
library(skeleton)

amostra = strsplit(readLines("amostra_chuva_4_100k.txt"), ' ')[[1]]

esqueleto_chuva = skeleton(amostra, c('sol','nuvem','chuva','tempestade'))

print(esqueleto_chuva$skeleton, 'n','transitions')

View(esqueleto_chuva$matrix)

irreductible(esqueleto_chuva$matrix)

