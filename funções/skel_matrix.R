skel_matrix = function(skel_df){
  stopifnot(all(c('context','p')%in%colnames(skel_df)))
  df = skel_df[,c('context','p')]
  
  max_char = max(nchar(df$context))
  matrix_df = tibble::tibble(exp_context = character(0), context = character(0))
  
  for(c in df$context){
    l = max_char-nchar(c)
    if(l==0) matrix_df = rbind(matrix_df, tibble(exp_context = c, context = c))
    fills = apply(expand.grid(replicate(l, c(0,1), simplify = FALSE)), 1, paste, collapse = '')
    contexts = paste0(fills, c)
    matrix_df = rbind(matrix_df, tibble(exp_context = contexts, context = replicate(length(fills),c)))
  }
  
  matrix_df = dplyr::left_join(matrix_df, df)
  pasts = apply(expand.grid(replicate(max_char, c(0,1), simplify = FALSE)), 1, paste, collapse='')
  m = matrix(0, nrow = 2^max_char, ncol = 2^max_char, dimnames = list(pasts, sort(pasts))) #this combination of sorted and unsorted pasts creates "blocks" in the transitions that could have positive probabilities
  for(p in pasts){
    prob = matrix_df[matrix_df$exp_context==p,]$p
    p_cut = vec_to_string(string_to_vec(p)[2:nchar(p)])
    m[p, paste0(p_cut,'0')] = prob
    m[p, paste0(p_cut,'1')] = 1-prob
  }
  m = ifelse(m>0, 1, m)
  return(m)
}