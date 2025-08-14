###DEMONSTRAÇÃO
setwd("Documents/codigos/r_data/IC")
library(skeleton)

amostra = strsplit(readLines("amostra_chuva_4_100k.txt"), ' ')[[1]]

esqueleto_chuva = skeleton(amostra, c('sol','nuvem','chuva','tempestade'))

print(esqueleto_chuva$skeleton, 'n','transitions')

View(esqueleto_chuva$matrix)

irreductible(esqueleto_chuva$matrix)

library(plot.matrix)

am_bin = strsplit(readLines('amostra_skel_100k.txt'), '')[[1]]
esq_1 = skeleton(am_bin, sep = ' ')


pdf('esqueleto_1.pdf', width = 6, height = 6)
par(c(0,4,4,2))
plot(esq_1$matrix, col = c('#edf2f4','#d90429'), asp=T, main='',xlab='',ylab='', key = NULL, 
     border = NULL)
dev.off()

pdf('esqueleto_2.pdf', width = 6, height = 6)
par(c(0,4,4,2))
plot(esqueleto_chuva$matrix, col = c('#edf2f4','#d90429'), asp=T, main='',xlab='',ylab='', key = NULL, 
     axis.row = NULL, axis.col = NULL, polygon.cell = list(border = 'gray', lwd = 0.3))
axis(1, at = 1:64, labels = FALSE, tck = -0.01, lwd = 0.5) # bottom
axis(2, at = 1:64, labels = FALSE, tck = -0.01, lwd = 0.5) # left

rect(xleft   = 0.5,
     ybottom = 0.5,
     xright  = 64.5,
     ytop    = 64.5,
     border  = "black",   # color of outer border
     lwd     = 0.8)   
dev.off()
