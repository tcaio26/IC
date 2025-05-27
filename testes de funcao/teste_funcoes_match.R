t = paste(sample(c(0,1),1e7,T),collapse = '')

match_expr = function(str, p) gregexpr(p, str)[[1]]

match_expr2 = function(str, p) stringr::str_locate_all(str, p)[[1]][, "start"]

match_expr3 = function(str, p) stringi::stri_locate_all_regex(t, p)[[1]][, "start"]

rbenchmark::benchmark(match_expr(t, '0'), match_expr2(t, '0'), match_expr3(t, '0'))
#resultados com 10M, 100 iterações.
# 1-90sec 2-52sec 3-65sec

t = paste(sample(c(0,1),1e5,T),collapse = '')
rbenchmark::benchmark(match_expr(t, '0'), match_expr2(t, '0'), match_expr3(t, '0'),
                      replications = 1000)
#resultados com 100k, 1000 it.
# 1 - 8.031sec      2 - 3.586sec     3 - 3.631sec

#muito pouca diferença entre 2 e 3, vou usar 2 por conhecer melhor o stringr