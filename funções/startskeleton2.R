library(skeleton)
startskel2 = function(string, alphabet, Nmin){
  root = Node$new('r')
  root$context = ''
  root$counts = symbolcounts(string, '', alphabet)
  root$n = nchar(string)
  
  genskel2(root, string, alphabet, Nmin)
  
  lapply(Traverse(root), function(node) node$transitions = (node$counts>0))
  
  return(root)
}