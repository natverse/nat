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
neuronlistz <- function(inzip, patt=NULL, df=NULL, options=list(readmode=1L, readcores=1L, multiread=1L), ...) {
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
  attr(nlf,'options')=options
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
  con <- gzcon(unz(zipfile, p))
  on.exit({close(con2)})
  n=readRDS(con2)
  n
}

expand_rds_to_temp <- function(zipfile, p, td=tempfile()) {
  zip::unzip(zipfile, p, exdir = td)
  ff=file.path(td, p)
  ff
}

read_rds_from_tfs <- function(f) {
  on.exit(unlink(f))
  n=readRDS(f)
  n
}


#' @export
"[[.neuronlistz"<-function(x,i,...){
  p=attr(x,'keyfilemap')[i]
  if(!length(na.omit(p))) {
    stop("No matching neuron: ", i)
  } else {
    zipfile=attr(x, 'db')
    readmode=attr(x, 'options')$readmode
    n=if(readmode==1) read_rds_from_zip1(zipfile, p) else read_rds_from_zip2(zipfile, p)
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
  
  cores=attr(x, "options")$readcores
  multiread=attr(x, "options")$multiread
  readmode=attr(x, "options")$readcores
  l <- if(cores>1 && length(ichecked)>10) {
    if(multiread==1) {
      pbapply::pbsapply(ichecked, function(n) x[[n]], simplify = F, USE.NAMES = T, cl=cores)
    } else if(multiread==2) {
      td=tempfile()
      on.exit(unlink(td, recursive = T))
      ff=expand_rds_to_temp(attr(x, 'db'), attr(x,'keyfilemap')[ichecked])
      names(ff)=ichecked
      # pbapply::pbsapply(ff, read_rds_from_tfs, simplify = F, USE.NAMES = T, cl=cores)
      l=sapply(ff, read_rds_from_tfs, simplify = F, USE.NAMES = T)
    } else if(multiread==3) {
      doMC::registerDoMC(cores=cores)
      td=tempfile()
      on.exit(unlink(td, recursive = T))
      ff=expand_rds_to_temp(attr(x, 'db'), attr(x,'keyfilemap')[ichecked])
      names(ff)=ichecked
      l=nlapply(ff, read_rds_from_tfs, .parallel=T)
    } else if(multiread==4) {
      td=tempfile()
      on.exit(unlink(td, recursive = T))
      ff=expand_rds_to_temp(attr(x, 'db'), attr(x,'keyfilemap')[ichecked])
      l=pbmcapply::pbmclapply(ff, read_rds_from_tfs, mc.cores = cores)
      names(l)=ichecked
      l
    } else if(multiread==5) {
      td=tempfile()
      on.exit(unlink(td, recursive = T))
      ff=expand_rds_to_temp(attr(x, 'db'), attr(x,'keyfilemap')[ichecked])
      names(ff)=ichecked
      l=nlapply(ff, read_rds_from_tfs)
      names(l)=ichecked
      l
    } 
  } else sapply(ichecked, function(n) x[[n]], simplify = F, USE.NAMES = T)
  
  new=as.neuronlist(l, df=attr(x, 'df')[ichecked, , drop=FALSE])
  nat:::copy_nl_attributes(new, x, ignoremore=c("keyfilemap", "db", "options"))
}
