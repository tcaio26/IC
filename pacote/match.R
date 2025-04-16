match = function(str, p){ 
  pos = stringr::str_locate_all(str, p)[[1]][, "start"]
  return(pos)
}
