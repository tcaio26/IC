library(skeleton)

t = readLines('amostra_skel_100k.txt')

arv = startskel(t, 300, T)
sculptskeleton(arv, 300)

df = ToDataFrameTree(arv, 'context', 'p', 'dom', 'n', filterFun = isLeaf)

df[,c('context','p')]

context = df$context

teststr = '00'
maxcontxt = 4
falta = maxcontxt - nchar(teststr)
