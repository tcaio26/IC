library(skeleton)
genskel2 = function(parent, string, alphabet, Nmin){
  for(u in alphabet){
    node = parent$AddChild(paste0(u, parent$name))
    node$context = paste0(u, parent$context)
    node$counts = symbolcounts(string, node$context, alphabet)
    node$n = sum(node$counts)
    if(node$n >= Nmin) genskel2(node, string, alphabet, Nmin)
  }
}