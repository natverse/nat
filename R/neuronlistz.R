#' A neuronlist object that will read neurons from a zip file on demand
#'
#' @details \code{\link{neuronlistz}} is designed to wrap zip files containing
#'   neurons saved in the RDS or faster/smaller qs format for rapid access. You
#'   should be able to read typical files in <20 ms. For files of ~3 GB there is
#'   a fixed cost of the order of 10-15ms per read.
#'
#' @param zip Path to the zip file
#' @param patt Optional regex pattern or function to specify a subset of files.
#' @param df A data.frame of metadata that will be attached to the
#' @param ... Additional arguments currently ignored
#'
#' @return A \code{\link{neuronlist}} object with additional class
#'   \code{neuronlistz}
#' @seealso \code{\link{neuronlist}}, \code{\link{neuronlistfh}},
#'   \code{\link{write.neurons}}
#' @family neuronlist
#' @export
#'
#' @examples
#' \donttest{
#' write.neurons(Cell07PNs[1:5], tf <- tempfile(fileext = '.zip'), format='rds')
#' nz=neuronlistz(tf)
#' nz[[1]]
#' nz[1:5]
#' }
#' \dontrun{
#' write.neurons(Cell07PNs[1:5], tf <- tempfile(fileext = '.zip'), format='qs')
#' nz2=neuronlistz(tf)
#' all.equal(nz2[1:3], nz[1:3])
#' }
neuronlistz <- function(zip, patt=NULL, df=NULL, ...) {
  stopifnot(requireNamespace("zip", quietly = TRUE))
  zip=path.expand(zip)
  zl=zip::zip_list(zip)
  ff=zl$filename
  
  dff="write.neurons.dataframe.rds"
  if(dff %in% ff) {
    ff=setdiff(ff, dff)
    if(is.null(df))
      df <- read_from_zip(zip, dff, neuron=FALSE)
  }
  
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
  attr(nlf,'db')=zip
  class(nlf)=c('neuronlistz','neuronlist',class(nlf))
  data.frame(nlf)=df
  nlf
}

read_from_zip <- function(zipfile, p, multi=FALSE, neuron=TRUE) {
  td <- tempfile()
  on.exit(unlink(td, recursive = TRUE))
  zip::unzip(zipfile, p, exdir = td)
  paths=file.path(td, p)
  n <- if(multi) read.neurons(paths) else {
    if(neuron) read.neuron(paths) else readRDS(paths)
  }
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
  copy_nl_attributes(new, x, ignoremore=c("keyfilemap", "db"))
}
