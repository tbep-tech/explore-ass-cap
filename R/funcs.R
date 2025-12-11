# get datasets from repo
# try simple load, download if fail
rdataload <- function(dataurl = NULL){
  
  x <- gsub('\\.RData', '', basename(dataurl))
  
  # try simple load
  ld <- try(load(url(dataurl)), silent = T)
  
  # return x if load worked
  if(!inherits(ld, 'try-error')){
    out <- get(x)
  }

  # download x if load failed
  if(inherits(ld, 'try-error')){
    
    fl <- paste(tempdir(), basename(dataurl), sep = '/')
    download.file(flurl, destfile = fl, quiet = T)
    load(file = fl)
    out <- get(x)
    suppressMessages(file.remove(fl))
    
  }
  
  return(out)
    
}