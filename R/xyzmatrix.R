#' Get and assign coordinates for classes containing 3D vertex data
#' 
#' \code{xyzmatrix} gets coordinates from objects containing 3D vertex data
#' @param x object containing 3D coordinates
#' @param ... additional arguments passed to methods
#' @return For \code{xyzmatrix}: Nx3 matrix containing 3D coordinates
#' @export
#' @examples 
#' # see all available methods for different classes
#' methods('xyzmatrix')
#' # ... and for the assignment method
#' methods('xyzmatrix<-')
#' 
#' # basic usage
#' xyzmatrix(cbind(-1,2,3))
#' 
#' # character vector - useful e.g. when encoded in 1 column of a table 
#' str123="(-1,+2,3)"
#' xyzmatrix(str123)
#' # replace
#' xyzmatrix(str123) <- xyzmatrix(str123)/3
#' str123
#' xyzmatrix(str123) <- xyzmatrix(str123)*3
#' str123
xyzmatrix<-function(x, ...) UseMethod("xyzmatrix")

#' @param y,z separate y and z coordinates
#' @details Note that \code{xyzmatrix} can extract or set 3D coordinates in a
#'   \code{matrix} or \code{data.frame} that \bold{either} has exactly 3 columns
#'   \bold{or} has 3 columns named X,Y,Z or x,y,z. As of Nov 2020, if these
#'   columns are character vectors, they will be correctly converted to numeric
#'   (with a warning for any NA values).
#'
#' @section Getting and setting from character vectors:
#'
#'   \code{xyzmatrix} can also both get and set 3D coordinates from a character
#'   vector (including a single data frame column) in which each string encodes
#'   all 3 coordinates e.g. \code{"-1, 4, 10"}. It should handle a range of
#'   separators such as spaces, tabs, commas, semicolons and ignore extraneous
#'   characters such as brackets. Note that data are rounded by
#'   \code{\link{zapsmall}} in the replacement version to try to avoid cases
#'   where rounding errors result in long strings of digits to the right of the
#'   decimal place.
#'
#'   Replacement into character vectors introduces a number of corner cases when
#'   there are not exactly 3 numbers to replace in the target vector. We handle
#'   them as follows: \itemize{
#'
#'   \item 0 values in target, >0 in replacement: use a default pattern
#'
#'   \item 1-2 values in target, same number of "good" values in replacement:
#'   insert those replacement value
#'
#'   \item 1-2 values in target, different number of values in replacement: use
#'   default pattern, give a \code{warning}
#'
#'   }
#'
#'   The default pattern will be the first entry in \code{x} with 3 numbers.
#'   Should there not be such a value, then the pattern will be \code{"x, y,
#'   z"}.
#' @rdname xyzmatrix
#' @export
xyzmatrix.default<-function(x, y=NULL, z=NULL, ...) {
  xyzn=c("X","Y","Z")
  if(is.neuron(x,Strict=FALSE)) {
    x=x$d[,c("X","Y","Z")]
  } else if(!is.null(z)){
    x=cbind(x,y,z)
  } else if(is.data.frame(x) || is.matrix(x)){
    if(ncol(x)>3){
      matched_cols=match(xyzn, toupper(colnames(x)))
      if(!any(is.na(matched_cols))) x=x[, matched_cols, drop=FALSE]
      else stop("Ambiguous column names. Unable to retrieve XYZ data")
    } else if(ncol(x)<3) stop("Must have 3 columns of XYZ data")
  }
  mx=as.matrix(x)
  if(mode(mx)=='character'){
    tryCatch(mode(mx) <- 'numeric', 
             warning=function(w, ...) warning("xyzmatrix: ", w, call. = F))
  }
  colnames(mx)=xyzn
  mx
}

#' @export
#' @rdname xyzmatrix
#' @param empty2na Whether or not to convert empty elements (\code{NULL} or
#'   \code{list()}) into NAs. Default \code{TRUE}.
#' @description \code{xyzmatrix.list} will parse a list containing triplets of 3
#'   numeric values.
xyzmatrix.list<-function(x, empty2na=TRUE, ...) {
  # special case, neuron without a class
  if(is.neuron(x,Strict=FALSE))
    return(xyzmatrix(x$d[,c("X","Y","Z")]))
  
  lens=lengths(x)
  if(empty2na) {
    if(!all(lens %in% c(0,3)))
      stop("xyzmatrix accepts lists where each element has 0 or 3 numbers!")
    if(any(lens==0))
      x[lens==0]=list(rep(NA, 3))
  } else {
    if(any(lens!=3))
      stop("xyzmatrix accepts lists where each element has 3 numbers!")
  }
  
  mat=matrix(unlist(x, use.names = F), ncol=3, byrow = TRUE)
  xyzmatrix(mat)
}

#' @export
#' @rdname xyzmatrix
xyzmatrix.character<-function(x, ...) {
  cc=gsub("[^0-9.\\+eE-]+"," ", x)
  cc=trimws(cc)
  # lines with no input (or bad input should be treated as NA)
  cc[!nzchar(cc)]="NA NA NA"
  mat=read.table(text = cc, fill = TRUE)
  res=xyzmatrix(mat)
  # check we got as many rows as inputs
  stopifnot(isTRUE(nrow(res)==length(x)))
  res
}


#' @export
#' @rdname xyzmatrix
xyzmatrix.neuron<-function(x, ...) data.matrix(x$d[,c("X","Y","Z")])

#' @export
#' @rdname xyzmatrix
xyzmatrix.neuronlist<-function(x, ...) {
  coords=lapply(x, xyzmatrix, ...)
  do.call(rbind, coords)
}

#' @export
#' @rdname xyzmatrix
xyzmatrix.shapelist3d <- xyzmatrix.neuronlist

#' @export
#' @rdname xyzmatrix
xyzmatrix.dotprops<-function(x, ...) x$points

#' @export
#' @rdname xyzmatrix
xyzmatrix.hxsurf<-function(x, ...) {
  # quick function that gives a generic way to extract coords from 
  # classes that we care about and returns a matrix
  # nb unlike xyz.coords this returns a matrix (not a list)
  mx=data.matrix(x$Vertices[,1:3])
  colnames(mx)=c("X","Y","Z")
  mx
}

#' @rdname xyzmatrix
#' @export
xyzmatrix.igraph<-function(x, ...){
  xyz=sapply(c("X","Y","Z"), function(c) igraph::get.vertex.attribute(x, c))
  if(is.list(xyz) && all(sapply(xyz, is.null)))
    xyz = NULL
  xyz
}

#' @rdname xyzmatrix
#' @export
xyzmatrix.mesh3d<-function(x, ...){
  cbind(X=x$vb[1, ]/x$vb[4, ], Y=x$vb[2, ]/x$vb[4, ], Z=x$vb[3, ]/x$vb[4, ])
}

#' @description \code{xyzmatrix<-} assigns xyz elements of neuron or dotprops
#'   object and can also handle matrix like objects with columns named X, Y, Z
#'   or x, y, z.
#' @usage xyzmatrix(x) <- value
#' @param value Nx3 matrix specifying new xyz coords
#' @return For \code{xyzmatrix<-}: Original object with modified coords
#' @export
#' @seealso \code{\link{xyzmatrix}}
#' @rdname xyzmatrix
#' @examples
#' n=Cell07PNs[[1]]
#' xyzmatrix(n)<-xyzmatrix(n)
#' stopifnot(isTRUE(
#'   all.equal(xyzmatrix(n),xyzmatrix(Cell07PNs[[1]]))
#' ))
`xyzmatrix<-`<-function(x, value) UseMethod("xyzmatrix<-")

#' @export
`xyzmatrix<-.default`<-function(x, value){
  # count number of elements in matrices/data.frames and vectors
  nelems <- function(y) {
    dy=dim(y)
    if(is.null(dy)) length(y) else prod(dy)
  }
  
  # short circuit if x and value have no elements
  if(isTRUE(nrow(x)==0 && nelems(value)==0))
    return(x)
  
  xyzn=c("X","Y","Z")
  if(ncol(x)==3) {
    x[,]=value
  } else if(!any(is.na(matched_cols<-match(xyzn, toupper(colnames(x)))))) {
    x[,matched_cols]=value
  }
  else stop("Not a neuron or dotprops object or a matrix-like object with XYZ colnames")
  x
}

#' @export
#' @rdname xyzmatrix
`xyzmatrix<-.character`<-function(x, value){
  stopifnot(ncol(value)==3)
  stopifnot(nrow(value)==1 || nrow(value)==length(x))
  if(any(grepl("%g", x, fixed=T)))
    stop("Sorry I cannot handle input character vectors containing %g")
  
  # turn input values into a format string
  fmtstr=gsub("[0-9.\\+eE-]+","%g", x)
  value <- zapsmall(value)
  # remove any negative zeros ...
  value[value==0]=0
  
  nfmts=stringr::str_count(fmtstr, stringr::fixed("%g"))
  if(any(nfmts!=3L)) {
    # define a default format based on target data
    default_patt=if(!any(nfmts==3)) "%g, %g, %g" else fmtstr[nfmts==3][1]
    # check that how many finite replacement values we have been given
    ngood=3L-rowSums(is.na(value))
    
    # if we have 0 formats but >0 good vals in a line, use default pattern
    fmtstr[nfmts==0 & ngood>0]=default_patt
    
    # when we have neither 0 or 3 values to replace
    funny_lines=nfmts>0 & nfmts!=3
    if(any(funny_lines)) {
      # lines where number of (good) replacement values does not match target
      bad_lines = nfmts[funny_lines]!=ngood[funny_lines]
      if(any(bad_lines)) {
        # put the default pattern there
        fmtstr[funny_lines][bad_lines]=default_patt
        warning(sum(bad_lines), 
                " rows of the target did not have a matching number of items in replacement value")
      }
    }
  }
  
  sprintf(fmtstr, value[,1], value[,2], value[,3])
}

#' @rdname xyzmatrix
#' @export
#' @description  \code{xyzmatrix2str} will convert the XYZ locations associated
#'   with an object to a character vector (by default comma separated).
#' @param format A \code{\link{sprintf}} compatible format string. The default
#'   will give comma separated values.
#' @examples
#' head(xyzmatrix2str(kcs20[[1]]))
#' head(xyzmatrix2str(kcs20[[1]], format="(%g;%g;%g)"))
#' # if you want to process the xyz locations (here rounded to nearest nm)
#' # you must extract them from complex objects yourself
#' xyzmatrix2str(round(xyzmatrix(kcs20[[1]])*1000), format="%d,%d,%d")[1:3]
xyzmatrix2str <- function(x, format="%g, %g, %g") {
  xyz=xyzmatrix(x)
  sprintf(format, xyz[,1], xyz[,2], xyz[,3])
}

#' @export
#' @rdname xyzmatrix
`xyzmatrix<-.neuron`<-function(x, value){
  x$d[,c("X","Y","Z")]=value
  x
}

#' @export
#' @rdname xyzmatrix
`xyzmatrix<-.dotprops`<-function(x, value){
  x$points[,c("X","Y","Z")]=value
  x
}

#' @export
#' @rdname xyzmatrix
`xyzmatrix<-.hxsurf`<-function(x, value){
  x$Vertices[,1:3]=value
  x
}

#' @export
#' @rdname xyzmatrix
`xyzmatrix<-.igraph`<-function(x, value){
  colnames(value)=c("X","Y","Z")
  for(col in colnames(value)){
    x=igraph::set.vertex.attribute(x, col, value=value[,col])
  }
  x
}

#' @export
#' @rdname xyzmatrix
`xyzmatrix<-.shape3d`<-function(x, value){
  x$vb=t(cbind(unname(value), 1))
  x
}

#' @export
#' @rdname xyzmatrix
`xyzmatrix<-.mesh3d`<-`xyzmatrix<-.shape3d`


#' @export
#' @rdname xyzmatrix
`xyzmatrix<-.neuronlist`<-function(x, value){
  # find number of vertices for each neuron
  nv=nvertices(x)
  if (sum(nv) != nrow(value))
    stop("Mismatch between original and replacement number of vertices!")
  idxs=rep(seq_along(x), nv)
  b=by(value, INDICES = idxs, FUN = data.matrix)
  for(i in seq_along(x)) {
    xyzmatrix(x[[i]]) <- b[[i]]
  }
  x
}

#' @export
#' @rdname xyzmatrix
`xyzmatrix<-.shapelist3d`<-`xyzmatrix<-.neuronlist`

#' Find the number of vertices in an object (or each element of a neuronlist)
#' 
#' @param x An object with 3d vertices (e.g. neuron, surface etc)
#' @param ... Additional arguments passed to methods (currently ignored)
#'   
#' @return an integer number of vertices (or a vector of length equal to a
#'   neuronlist)
#' @export
#' 
#' @examples
#' nvertices(Cell07PNs[[1]])
#' nvertices(kcs20)
nvertices <- function(x, ...) UseMethod('nvertices')

#' @rdname nvertices
#' @export
nvertices.default <- function(x, ...) {
  nrow(xyzmatrix(x))
}

#' @export
nvertices.neuron <- function(x, ...) nrow(x$d)

#' @export
nvertices.dotprops <- function(x, ...) nrow(x$points)

#' @rdname nvertices
#' @export
nvertices.neuronlist <- function(x, ...) {
  sapply(x, nvertices)
}

#' @rdname nvertices
#' @export
nvertices.shapelist3d <- nvertices.neuronlist
