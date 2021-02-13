#' A neuronlist object that will read neurons from a zip file on demand
#'
#' @param inzip 
#' @param patt 
#' @param df 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
neuronlistz <- function(inzip, patt=NULL, df=NULL, readmode=1L, readcores=1L, ...) {
  stopifnot(requireNamespace("zip", quietly = TRUE))
  inzip=path.expand(inzip)
  zl=zip::zip_list(inzip)
  ff=zl$filename
  keyfilemap <- if(!is.null(patt)) {
    if(is.character(patt)) ff[grepl(patt, ff)]
    else if(is.function(patt)) ff[patt(ff)]
    else stop("Cannot understand patt argument!")
  } else ff
  
  names(keyfilemap)=tools::file_path_sans_ext(basename(keyfilemap))
  if(any(duplicated(names(keyfilemap))))
    stop("keyfilemap must have unique names as elements!")
  
  nlf=structure(rep(F,length(keyfilemap)),.Names=names(keyfilemap))
  attr(nlf,'keyfilemap')=keyfilemap
  if(is.null(df)) {
    df=data.frame(id=names(keyfilemap), stringsAsFactors = FALSE)
    rownames(df)=df[['id']]
  }
  attr(nlf,'db')=inzip
  attr(nlf,'df')=df
  attr(nlf,'options')=list(readcores=readcores, readmode=readmode)
  class(nlf)=c('neuronlistz','neuronlist',class(nlf))
  nlf
}

read_rds_from_zip1 <- function(zipfile, p) {
  td <- tempfile()
  on.exit(unlink(td, recursive = TRUE))
  zip::unzip(zipfile, p, exdir = td)
  n=readRDS(file.path(td, p))
  n
}

read_rds_from_zip2 <- function(zipfile, p) {
  con <- unz(zipfile, p, open = 'rb')
  con2 <- gzcon(con)
  on.exit({close(con2)})
  # on.exit({close(con2);close(con)})
  n=readRDS(con2)
  # read.neuron(p, ...)
  n
}

#' @export
"[[.neuronlistz"<-function(x,i,...){
  p=attr(x,'keyfilemap')[i]
  if(!length(na.omit(p))) {
    stop("No matching neuron: ", i)
  } else {
    zipfile=attr(x, 'db')
    FUN=switch(attr(x, 'options')$readmode, 
           `1`=read_rds_from_zip1, `2`=read_rds_from_zip2)
    n=FUN(zipfile, p)
    n
  }
}

#' @export
"[.neuronlistz" <- function(x, i, j, drop) {
  # treat like a data.frame
  if(nargs()>2) {
    return(NextMethod())
  }
  if(is.numeric(i) && !is.integer(i))
    i=as.character(i)
  else if(!is.character(i))
    i=names(x)[i]
  ichecked = intersect(i, names(x))
  nmissing = length(i) -length(ichecked)
  if(nmissing>0)
    warning("Dropping ", nmissing, " neurons that cannot be found!")
  
  cores=attr(x, "options")$readcores
  l <- if(cores>1) {
    requireNamespace('pbapply')
    pbapply::pbsapply(ichecked, function(n) x[[n]], simplify = F, USE.NAMES = T, cl=cores)
  } else sapply(ichecked, function(n) x[[n]], simplify = F, USE.NAMES = T)
  
  new=as.neuronlist(l, df=attr(x, 'df')[ichecked, , drop=FALSE])
  nat:::copy_nl_attributes(new, x, ignoremore=c("keyfilemap", "db", "options"))
}
