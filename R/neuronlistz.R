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
neuronlistz <- function(inzip, patt=NULL, df=NULL, ...) {
  stopifnot(requireNamespace("zip", quietly = TRUE))
  inzip=path.expand(inzip)
  zl=zip::zip_list(inzip)
  ff=zl$filename
  
  keyfilemap <- if(!is.null(patt)) {
    if(is.character(patt)) ff[grepl(patt, ff)]
    else if(is.function(patt)) ff[patt(ff)]
    else stop("Cannot understand patt argument!")
  } else ff
  
  exts=unique(tools::file_ext(keyfilemap))
  known_exts = c("rds", "qs", "rdsb")
  if(!all(exts %in% known_exts))
    stop("Unrecognised extensions in zipfile:", 
         paste(setdiff(exts, known_exts), collapse = ", "))
  
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
  attr(nlf,'options')=options
  class(nlf)=c('neuronlistz','neuronlist',class(nlf))
  nlf
}

read_from_zip <- function(zipfile, p, multi=FALSE) {
  td <- tempfile()
  on.exit(unlink(td, recursive = TRUE))
  zip::unzip(zipfile, p, exdir = td)
  paths=file.path(td, p)
  n=if(multi) read.neurons(paths) else read.neuron(paths)
  n
}

#' @export
"[[.neuronlistz"<-function(x,i,...){
  p=attr(x,'keyfilemap')[i]
  if(!length(na.omit(p))) {
    stop("No matching neuron: ", i)
  } else {
    zipfile=attr(x, 'db')
    n=read_from_zip(zipfile, p)
    n
  }
}

#' @export
"[.neuronlistz" <- function(x, i, j, drop) {
  # treat like a data.frame
  if(nargs()>2) {
    return(NextMethod())
  }
  if(is.factor(i))
    i=as.character(i)
  if(!is.character(i))
    i=names(x)[i]
  ichecked = intersect(i, names(x))
  nmissing = length(i) -length(ichecked)
  if(nmissing>0)
    warning("Dropping ", nmissing, " neurons that cannot be found!")
  zipfile=attr(x, 'db')
  p=attr(x,'keyfilemap')[ichecked]
  l <- read_from_zip(zipfile, p, multi = T)
  
  new=as.neuronlist(l, df=attr(x, 'df')[ichecked, , drop=FALSE])
  nat:::copy_nl_attributes(new, x, ignoremore=c("keyfilemap", "db", "options"))
}
