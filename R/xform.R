#' Transform the 3D location of objects such as neurons
#' 
#' \code{xform} is designed to operate on a variety of data types, especially 
#' objects encapsulating neurons. \code{xform} depends on two specialised 
#' downstream functions \code{\link{xformpoints}} and \code{\link{xformimage}}. 
#' These are user visible any contain some useful documentation, but should only
#' be required for expert use; in almost all circumstances, you should use only 
#' \code{xform}.
#' 
#' @section Registrations:
#'   
#'   When \code{reg} is a character vector, xform's specialised downstream 
#'   functions will check to see if it defines a path to one (or more) 
#'   registrations on disk. These can be of two classes
#'   
#'   \itemize{
#'   
#'   \item CMTK registrations
#'   
#'   \item \code{\link{reglist}} objects saved in R's \code{RDS} format (see 
#'   \code{\link{readRDS}}) which can contain any sequence of registrations 
#'   supported by nat.
#'   
#'   }
#'   
#'   If the path does indeed point to a CMTK registration, this method will hand
#'   off to \code{xformpoints.cmtkreg} or \code{xformimages.cmtkreg}. In this
#'   case, the character vector may optionally have an attribute, 'swap', a
#'   logical vector of the same length indicating whether the transformation
#'   direction should be swapped. At the moment only CMTK registration files are
#'   supported.
#'   
#'   If \code{reg} is a character vector of length >=1 defining a sequence of 
#'   registration files on disk they should proceed from sample to reference.
#'   
#'   Where \code{reg} is a function, it should have a signature like 
#'   \code{myfun(x,), ...} where the \code{...} \strong{must} be provided in 
#'   order to swallow any arguments passed from higher level functions that are 
#'   not relevant to this particular transformation function.

#' @details Methods are provided for some specialised S3 classes. Further 
#'   methods can of course be constructed for user-defined S3 classes. However 
#'   this will probably not be necessary if the \code{xyzmatrix} and 
#'   \code{`xyzmatrix<-`} generics are suitably overloaded \emph{and} the S3 
#'   object inherits from \code{list}.
#'   
#'   Note that given the behaviour of the \code{xyzmatrix} functions, the
#'   \code{xform.data.frame} method will transform the x,y,z or X,Y,Z columns of
#'   a data.frame if the data.frame has more than 3 columns, erroring out if no
#'   such unique columns exist.
#'   
#' @param x an object to transform
#' @param reg A registration defined by a matrix, a function, a \code{cmtkreg} 
#'   object, or a character vector specifying a path to one or more 
#'   registrations on disk (see Registrations section).
#' @param ... additional arguments passed to methods and eventually to 
#'   \code{\link{xformpoints}}
#' @export
#' @rdname xform
#' @seealso \code{\link{xformpoints}}
xform<-function(x, reg, ...) UseMethod('xform')

#' @details TODO get this to work for matrices with more than 3 columns by
#'   working on xyzmatrix definition.
#' @export
#' @param na.action How to handle NAs. NB drop may not work for some classes.
#' @rdname xform
xform.default<-function(x, reg, na.action=c('warn','none','drop','error'), ...){
  na.action=match.arg(na.action)
  pointst=xformpoints(reg, x, ...)
  if(na.action=='none') return(pointst)
  naPoints = is.na(pointst[, 1])
  if (any(naPoints)) {
    if (na.action == "drop") 
      pointst = pointst[!naPoints, , drop=FALSE]
    else if (na.action == "warn") 
      warning("There were ", sum(naPoints), " points that could not be transformed")
    else if (na.action == "error") 
      stop("There were ", sum(naPoints), " points that could not be transformed")
  }
  pointst
}

#' @description \code{xform.character} is designed to work with files on disk.
#'   Presently it is restricted to images, although other datatypes may be
#'   supported in future.
#' @export
#' @rdname xform
xform.character<-function(x, reg, ...) {
  if(!file.exists(x)) stop("file does not exist:", x)
  fr=getformatreader(x, class = 'im3d')
  if(is.null(fr))
    stop("xform currently only operates on image files. ",
         "See ?xform and ?fileformats for details of acceptable formats.")
  
  xformimage(reg, x, ...)
}

#' @export
#' @rdname xform
#' @param FallBackToAffine Whether to use an affine transform when a cmtk
#'   warping transformation fails.
xform.list<-function(x, reg, FallBackToAffine=TRUE, na.action='error', ...){
  points=xyzmatrix(x)
  pointst=xformpoints(reg, points, FallBackToAffine=FallBackToAffine, 
                na.action=na.action, ...)
  xyzmatrix(x)<-pointst
  x
}

#' @export
#' @rdname xform
xform.shape3d<-xform.list

#' @export
#' @rdname xform
xform.neuron<-xform.list

#' @export
#' @rdname xform
xform.data.frame <- function(x, reg, subset=NULL, ...) {
  points=xyzmatrix(x)
  if(!is.null(subset))
    points = points[subset, , drop=FALSE]
  pointst=xform(points,reg, ...)
  if(is.null(subset)) {
    xyzmatrix(x) <- pointst
  } else {
    xyzmatrix(x)[subset, ] <- pointst
  }
  x
}

#' @export
#' @rdname xform
#' @details For the \code{xform.dotprops} method, dotprops tangent vectors will 
#'   be recalculated from scratch after the points have been transformed (even 
#'   though the tangent vectors could in theory be transformed more or less 
#'   correctly). When there are multiple transformations, \code{xform} will take
#'   care to carry out all transformations before recalculating the vectors.
#' @examples
#' \dontrun{
#' kc1=kcs20[[1]]
#' kc1.default=xform(kc1,function(x,...) x)
#' stopifnot(isTRUE(all.equal(kc1,kc1.default)))
#' kc1.5=xform(kc1,function(x,...) x, k=5)
#' stopifnot(isTRUE(all.equal(kc1.5,kc1.default)))
#' kc1.20=xform(kc1,function(x,...) x, k=20)
#' stopifnot(!isTRUE(all.equal(kc1,kc1.20)))
#' 
#' # apply two registrations converting sample->IS2->JFRC2
#' reg_seq=c("IS2_sample.list", "JFRC2_IS2.list")
#' xform(kc1, reg_seq)
#' # apply two registrations, swapping the direction of the second one
#' # i.e. sample -> IS2 -> FCWB
#' reg_seq=structure(c("IS2_sample.list", "IS2_FCWB.list"), swap=c(FALSE, TRUE))
#' xform(kc1, reg_seq)
#' }
xform.dotprops<-function(x, reg, FallBackToAffine=TRUE, ...){
  points=xyzmatrix(x)
  pointst=xform(points, reg=reg, FallBackToAffine=FallBackToAffine, ...)
  xyzmatrix(x)=pointst
  dotprops(x, ...)
}

#' @details With \code{xform.neuronlist}, if you want to apply a different 
#'   registration to each object in the neuronlist \code{x}, then you should use
#'   \code{VectoriseRegistrations=TRUE}.
#'   
#'   When \code{x}'s attached data.frame contains columns called x,y,z or X,Y,Z 
#'   then these are assumed to be coordinates and also transformed when 
#'   \code{TransformDFCoords=TRUE} (the default). This provides a mechanism for 
#'   transforming the soma positions of \code{neuronlist} objects containing 
#'   \code{dotprops} objects (which do not otherwise store the soma position).
#'   Note that if transformation fails, a warning will be issued and the points
#'   will be replaced with \code{NA} values.
#' @param subset For \code{xform.neuronlist} indices (character/logical/integer)
#'   that specify a subset of the members of \code{x} to be transformed.
#' @param VectoriseRegistrations When \code{FALSE}, the default, each element of
#'   \code{reg} will be applied sequentially to each element of \code{x}. When 
#'   \code{TRUE}, it is assumed that there is one element of \code{reg} for each
#'   element of \code{x}.
#' @param TransformDFCoords If the metadata \code{data.frame} attached to 
#'   \code{x} includes columns that look like x,y,z coordinates, transform those
#'   as well.
#' @inheritParams nlapply
#' @export
#' @rdname xform
#' @examples
#' \dontrun{
#' # apply reg1 to Cell07PNs[[1]], reg2 to Cell07PNs[[2]] etc
#' regs=c(reg1, reg2, reg3)
#' nx=xform(Cell07PNs[1:3], reg=regs, VectoriseRegistrations=TRUE)
#' }
xform.neuronlist<-function(x, reg, subset=NULL, ..., OmitFailures=NA,
                           VectoriseRegistrations=FALSE, TransformDFCoords=TRUE) {
  # first transform objects in the neuronlist
  tx=if(VectoriseRegistrations) {
    nmapply(xform, x, reg=reg, ..., subset=subset, OmitFailures=OmitFailures)
  } else {
    nlapply(x, FUN=xform, reg=reg, ..., subset=subset, OmitFailures=OmitFailures)
  }
  # then check if there is an attached data.frame with things that look like
  # soma coordinates
  if(TransformDFCoords && !is.null(df<-as.data.frame(x))) {
    matched_cols=match(c("X","Y","Z"), toupper(colnames(df)))
    if(all(is.finite(matched_cols))) {
      # we have some data to transform
      if(VectoriseRegistrations) {
        stop("Not yet implemented")
      } else {
        # let's assume that if we were able to transform the neuron, then we
        # want to be able to transform the soma (but will warn on failure)
        # However we just keep rows for neurons in our result neuronlist
        # given that choice we need to convert our subset expression into rownames
        # because numeric indices will get out of register
        if(!is.null(subset) && !is.character(subset))
          subset=rownames(df)[subset]
        df=df[names(tx),,drop=FALSE]
        
        data.frame(tx) <- xform(df, reg, na.action = 'warn', subset = subset)
      }
    }
  }
  tx
}

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
#'   \code{xyzmatrix} can also both get and set 3D coordinates from a character
#'   vector (including a single data frame column) in which each string encodes
#'   all 3 coordinates e.g. \code{"-1, 4, 10"}. It should handle a range of
#'   separators such as spaces, tabs, commas, semicolons and ignore extraneous
#'   characters such as brackets. Note that data are rounded by
#'   \code{\link{zapsmall}} in the replacement version to try to avoid cases
#'   where rounding errors result in long strings of digits to the right of the
#'   decimal place.
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
xyzmatrix.character<-function(x, ...) {
  cc=gsub("[^0-9.\\+eE-]+"," ", x)
  cc=trimws(cc)
  mat=read.table(text = cc)
  xyzmatrix(mat)
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
    stop("Sorry I cannot handle input character vectors containing %f")
    
  # turn input values into a format string
  fmtstr=gsub("[0-9.\\+eE-]+","%g", x)
  value <- zapsmall(value)
  # remove any negative zeros ...
  value[value==0]=0
  sprintf(fmtstr, value[,1], value[,2], value[,3])
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

#' Mirror 3D object about a given axis, optionally using a warping registration
#' 
#' @description mirroring with a warping registration can be used to account 
#'   e.g. for the asymmetry between brain hemispheres.
#'   
#' @details The \code{mirrorAxisSize} argument can be specified in 3 ways for 
#'   the x axis with extreme values, x0+x1: \itemize{
#'   
#'   \item a single number equal to x0+x1
#'   
#'   \item a 2-vector c(x0, x1) (\bold{recommended})
#'   
#'   \item the \code{\link{boundingbox}} for the 3D data to be mirrored: the 
#'   relevant axis specified by \code{mirrorAxis} will be extracted.
#'   
#'   }
#'   
#'   This function is agnostic re node vs cell data, but for node data 
#'   BoundingBox should be supplied while for cell, it should be bounds. See 
#'   \code{\link{boundingbox}} for details of BoundingBox vs bounds.
#'   
#'   See \code{\link{nlapply}} for details of the \code{subset} and 
#'   \code{OmitFailures} arguments.
#'   
#' @param x Object with 3D points (with named cols X,Y,Z) or path to image on
#'   disk.
#' @param ... additional arguments passed to methods or eventually to 
#'   \code{\link{xform}}
#' @return Object with transformed points
#' @export
#' @seealso \code{\link{xform}, \link{boundingbox}}
#' @examples
#' nopen3d()
#' x=Cell07PNs[[1]]
#' mx=mirror(x,168)
#' \donttest{
#' plot3d(x,col='red')
#' plot3d(mx,col='green')
#' }
#' 
#' # also works with dotprops objects
#' nclear3d()
#' y=kcs20[[1]]
#' my=mirror(y,mirrorAxisSize=564.2532,transform='flip')
#' \donttest{
#' plot3d(y, col='red')
#' plot3d(my, col='green')
#' }
#' 
#' \dontrun{
#' ## Example with an image
#' # note that we must specify an output image (obviously) but that as a
#' # convenience mirror calculates the mirrorAxisSize for us
#' mirror('myimage.nrrd', output='myimage-mirrored.nrrd', 
#'   warpfile='myimage_mirror.list')
#' 
#' # Simple flip along a different axis
#' mirror('myimage.nrrd', output='myimage-flipped.nrrd', mirrorAxis="Y", 
#'   transform='flip')
#' }
mirror<-function(x, ...) UseMethod('mirror')

#' @export
#' @description \code{mirror.character} handles images on disk
#' @param output Path to the output image
#' @param target Path to the image defining the target grid (defaults to the
#'   input image - hard to see when this would not be wanted).
#' @rdname mirror
mirror.character<-function(x, output, mirrorAxisSize=NULL, target=x, ...){
  if(is.null(mirrorAxisSize)){
    if(!file.exists(x)) stop("Presumptive image file does not exist:", x)
    fr=getformatreader(x, class = 'im3d')
    if(is.null(fr))
      stop("mirror currently only operates on *image* files. See ?fileformats or output of\n",
           "fileformats(class='im3d',rval = 'info') for details of acceptable formats.")
    im=read.im3d(x, ReadData = FALSE)
    NextMethod(mirrorAxisSize=boundingbox(im))
  } else NextMethod()
}

#' @param mirrorAxisSize A single number specifying the size of the axis to 
#'   mirror or a 2 vector (\bold{recommended}) or 2x3 matrix specifying the 
#'   \code{\link{boundingbox}} (see details).
#' @param mirrorAxis Axis to mirror (default \code{"X"}). Can also be an integer
#'   in range \code{1:3}.
#' @param warpfile Optional registration or \code{\link{reglist}} to be applied
#'   \emph{after} the simple mirroring.. It is called warpfile for historical
#'   reasons, since it is normally the path to a CMTK registration that
#'   specifies a non-rigid transformation to correct asymmetries in an image.
#' @param transform whether to use warp (default) or affine component of 
#'   registration, or simply flip about midplane of axis.
#' @export
#' @rdname mirror
mirror.default<-function(x, mirrorAxisSize, mirrorAxis=c("X","Y","Z"),
                         warpfile=NULL, transform=c("warp",'affine','flip'), ...){
  transform=match.arg(transform)
  if(is.character(mirrorAxis)) {
    mirrorAxis=match.arg(mirrorAxis)
    mirrorAxis=match(mirrorAxis,c("X","Y","Z"))
  }
  if(length(mirrorAxis)!=1 || is.na(mirrorAxis) || mirrorAxis<0 || mirrorAxis>3)
    stop("Invalid mirror axis")
  
  # Handle variety of mirrorAxisSize specifications
  lma=length(mirrorAxisSize>1)
  if(lma>1){
    if(lma==6) mirrorAxisSize=mirrorAxisSize[,mirrorAxis]
    else if(lma!=2) stop("Unrecognised mirrorAxisSize specification!")
    mirrorAxisSize=sum(mirrorAxisSize)
  }
  
  # construct homogeneous affine mirroring transform
  mirrormat=diag(4)
  mirrormat[mirrorAxis, 4]=mirrorAxisSize
  mirrormat[mirrorAxis, mirrorAxis]=-1
  
  if(is.null(warpfile) || transform=='flip') {
    xform(x, reg=mirrormat, ...)
  } else {
    # Combine registrations: 
    xform(x, reg=c(reglist(mirrormat), warpfile), transformtype=transform, ...)
  }
}

#' @param subset For \code{mirror.neuronlist} indices
#'   (character/logical/integer) that specify a subset of the members of
#'   \code{x} to be transformed.
#' @inheritParams nlapply
#' @export
#' @rdname mirror
#' @seealso \code{\link{nlapply}}
mirror.neuronlist<-function(x, subset=NULL, OmitFailures=NA, ...){
  NextMethod()
}



#' Thin plate spline registrations via xform and friends
#'
#' @description \code{tpsreg} creates an object encapsulating a thin plate spine
#'   transform mapping a paired landmark set.
#' @param sample,reference Matrices defining the sample (or floating) and
#'   reference (desired target after transformation) spaces. See details.
#' @param ... additional arguments passed to \code{\link{xformpoints.tpsreg}}
#' @details  Note that we use the \bold{nat} convention for naming the
#'   sample/reference space arguments but these actually clash with the
#'   nomenclature in the underlying \code{Morpho::\link[Morpho]{tps3d}}
#'   function. \itemize{
#'
#'   \item refmat (Morpho3d) == sample (nat)
#'
#'   \item tarmat (Morpho3d) == reference (nat)
#'
#'   }
#' @export
#' @seealso \code{\link[nat]{reglist}}, \code{\link[nat]{read.landmarks}}
#' @examples
#' \dontrun{
#' ## A full worked example of using landmarks based registration to construct
#' ## a mirroring registration from one side of the brain to the other.
#'
#' # read in set of landmarks defined in FAFB CATMAID
#' library('catmaid')
#' emlandmarks=catmaid::read.neurons.catmaid('annotation:^GJLandmark')
#' 
#' # Match up L and R pairs
#' library('stringr')
#' emlandmarks[,'side']=stringr::str_match(emlandmarks[,'name'], "([LR]) Landmark")[,2]
#' emlandmarks[,'shortname']=stringr::str_match(emlandmarks[,'name'], "(.*)([LR]) Landmark.*")[,2]
#' emlandmarks[,'shortname']=sub("[_ ]+$", "", emlandmarks[,'shortname'])
#' 
#' library('dplyr')
#' lmpairs=dplyr::inner_join(
#'   dplyr::filter(emlandmarks[,], side=="L"),
#'   dplyr::filter(emlandmarks[,], side=="R"),
#'   by='shortname', suffix=c(".L",".R"))
#'
#' # find mean xyz position of each landmark (they are drawn as a little cross)
#' lmxyz=t(sapply(emlandmarks, function(x) colMeans(xyzmatrix(x))))
#' # construct thin plate splines registration (here mapping the right side neurons to left side)
#' mirror_reg=tpsreg(
#'   lmxyz[as.character(lmpairs$skid.R),],
#'   lmxyz[as.character(lmpairs$skid.L),]
#' )
#' # map RHS DA2 PNs onto left and compare with LHS neurons
#' da2pns.R=catmaid::read.neurons.catmaid('glomerulus DA2 right')
#' da2pns.L=catmaid::read.neurons.catmaid('glomerulus DA2 left')
#' 
#' da2pns.R.L=xform(da2pns.R, reg = mirror_reg)
#' plot(da2pns.L, col='red')
#' plot(da2pns.R.L, col='blue', add=TRUE)
#' }
tpsreg<-function(sample, reference, ...){
  structure(list(refmat=data.matrix(sample), tarmat=data.matrix(reference), ...),
            class='tpsreg')
}

#' @description \code{xformpoints.tpsreg} enables \code{\link[nat]{xform}} and
#'   friends to transform 3d vertices (or more complex objects containing 3d
#'   vertices) using a thin plate spline mapping stored in a \code{tpsreg}
#'   object.
#' @rdname tpsreg
#' @param reg The \code{tpsreg} registration object
#' @param points The 3D points to transform
#' @param swap Whether to change the direction of registration (default of
#'   \code{NULL} checks if reg has a \code{attr('swap'=TRUE)}) otherwise
#' @export
xformpoints.tpsreg <- function(reg, points, swap=NULL, ...){
  if(isTRUE(swap) || isTRUE(attr(reg, 'swap'))) {
    tmp=reg$refmat
    reg$refmat=reg$tarmat
    reg$tarmat=tmp
  }
  do.call(Morpho::tps3d, c(list(x=points), reg,  list(...)))
}
