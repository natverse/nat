#' dotprops: Neurons as point clouds with tangent vectors (but no connectivity)
#' @param x Object to be tested/converted
#' @name dotprops
#' @export
is.dotprops<-function(x) inherits(x,"dotprops")

#' @rdname dotprops
#' @export
as.dotprops<-function(x, ...){
  if(is.null(x)) return (NULL)
  if(!is.dotprops(x)) class(x)=c("dotprops",class(x))
  if("topo" %in% names(x)) class(x) = union("topo.dotprops", class(x))
  if(is.null(colnames(x$points))) colnames(x$points) <-c("X","Y","Z")
  x
}

#' Arithmetic for nat dotprops and surface objects
#' 
#' @param e1 A dotprops or surface (hxsurf, mesh3d) object
#' @param e2 A scalar or 3-vector that will be applied to the dotprops object
#' @return A new dotprops / surface object
#' @export
#' @rdname dotprops-arithmetic
#' @seealso \code{\link{scale.dotprops}}, \code{\link{Ops.neuron}}
#' @examples 
#' kcs20.shift=kcs20+c(2,3,4)
#' \donttest{
#' plot3d(kcs20, col='grey')
#' plot3d(kcs20.shift, col='red')
#' }
Ops.dotprops <- function(e1, e2=NULL) {
  r=e1
  e1=xyzmatrix(e1)
  # I don't exactly know why it is necessary to change this directly, but if not
  # NextMethod dispatches on original class of e1 even when I specify object=
  .Class=class(e1)
  lx=length(e2)
  if(lx==3) e1=t(e1) else if(lx>1) stop("expects a numeric vector of length 0, 1 or 3")
  res <- NextMethod(generic=.Generic)
  if(lx==3) res=t(res)
  xyzmatrix(r)=res
  r
}

#' @export
#' @rdname dotprops-arithmetic
Ops.mesh3d <- Ops.dotprops

#' @export
#' @rdname dotprops-arithmetic
Ops.hxsurf <- Ops.dotprops

#' @rdname scale.neuron
#' @include neuron.R
#' @export
#' @aliases scale.dotprops
#' @description note that \code{scale.dotprops} recalculates the tangent vectors
#'   after scaling the 3D coords. See \code{\link{dotprops}} for details.
scale.dotprops<-function(x, center=TRUE, scale=TRUE){
  xyzmatrix(x)<-scale(xyzmatrix(x),scale=scale,center=center)
  dotprops(x)
}

#' @description \code{dotprops} makes dotprops representation from raw 3D points
#'   (extracting vertices from S3 objects that have them)
#' @details \code{k} will default to 20 nearest neighbours when unset (i.e. when
#'   it has default value of NA) unless \code{x} is a dotprops object (when the
#'   original value of \code{k} is reused).
#' @param ... Additional arguments passed to methods
#' @export
#' @rdname dotprops
dotprops<-function(x, ...) UseMethod('dotprops')

#' @description \code{dotprops.character} makes dotprops objects from one or
#'   more files on disk (typically binary segmentations saved as NRRDs).
#'   \code{x} can a vector of paths or be a directory (in which case
#'   \code{pattern} can be used to restrict the files to read). The \code{...}
#'   argument is passed first to \code{\link{nlapply}} (if there is more than
#'   one file) and then to \code{dotprops.default}.
#' @export
#' @rdname dotprops
#' @inheritParams base::list.files
#' @examples
#' \dontrun{
#' # process a single file on disk
#' dp=dotprops.character('~/skeleton-nrrds/file01.nrrd', k=5)
#' # process a whole directory of files
#' dps=dotprops.character('~/skeleton-nrrds/', OmitFailures=T, k=5)
#' }
dotprops.character <- function(x, pattern = NULL, OmitFailures = NA, ...) {
  if(length(x)==1 && isTRUE(file.info(x)$isdir))
    x=dir(x, full.names = TRUE, pattern = pattern)
  
  if(length(x)>1) {
    # process many files by making a dummy neuronlist
    df=data.frame(filename=basename(x), stringsAsFactors = FALSE)
    rownames(df)=tools::file_path_sans_ext(df$filename)
    nl=as.neuronlist(as.list(x), df = df)
    res=nlapply(nl, dotprops, OmitFailures = OmitFailures, ...)
    return(res)
  }
  
  fileName <- x
  x <- read.im3d(x)
  l <- dotprops(x, ...)
  attr(l,'file') <- fileName
  fi <- file.info(fileName)
  attr(l, 'mtime') <- fi$mtime
  attr(l, 'size') <- fi$size
  l
}

#' @description \code{dotprops.dotprops} will default to the original vale of 
#'   \code{k} and copy over all attributes that are not set by
#'   \code{dotprops.default}.
#' @export
#' @export
#' @rdname dotprops
dotprops.dotprops<-function(x, k=attr(x,'k'), ...) {
  y=dotprops(xyzmatrix(x), k=k, ...)
  # copy over attributes, taking care not to overwrite any
  attin=attributes(x)
  attout=attributes(y)
  attributes(y)<-c(attout, attin[setdiff(names(attin), names(attout))])
  y
}

#' @export
#' @rdname dotprops
dotprops.im3d <- function(x, ...) {
  l <- ind2coord(x)
  l <- dotprops(l, ...)
  l
}

#' @export
dotprops.list<-function(x, ...) {
  # FIXME - change to an abstract base class for objects with 3D vertices
  # rather than the completely generic list
  dotprops(xyzmatrix(x), ...)
}

#' @description \code{dotprops.neuronlist} will run for every object in the 
#'   neuronlist using \code{\link{nlapply}}. \code{...} arguments will be passed to 
#'   \code{nlapply} in addition to the named argument \code{OmitFailures}.
#' @export
#' @rdname dotprops
#' @inheritParams nlapply
#' @seealso \code{\link{nlapply}}
dotprops.neuronlist<-function(x, ..., OmitFailures=NA) {
  nlapply(x, dotprops, ..., OmitFailures=OmitFailures)
}

#' @export
#' @param resample When finite, a new length to which all segmented edges will
#'   be resampled. See \code{\link{resample.neuron}}.
#' @param topo flag that says whether or not to add topological features
#' (reversed Strahler Order and distance from soma)
#' @rdname dotprops
dotprops.neuron<-function(x, Labels=NULL, resample=NA, topo=FALSE, ...) {
  if(is.finite(resample)) x=resample(x, stepsize = resample)
  if(is.null(Labels) || isTRUE(Labels)) Labels=x$d$Label
  else if(is.logical(labels) && labels==FALSE) Labels=NULL
  topo_features <- NULL
  if (isTRUE(topo)) topo_features <- get_topo_features(x)
  dotprops(xyzmatrix(x), Labels=Labels, topo_features=topo_features, ...)
}

#' Get topological features per each node
#'
#' @param n neuron object with soma
#'
#' @return list with distance and Reversed Strahler order features per node.
#' @rdname dotprops-topo
#' @export
#' @examples
#' get_topo_features(Cell07PNs[[1]])
get_topo_features <- function(n) {
  topovec <- list()
  topovec$distance <- get_distance_to_soma(n)
  so <- strahler_order(n)
  # normalizing so the main branch is always 0
  topovec$rso <- abs(so$points-max(so$points))
  topovec
}

#' Get distance from soma
#' 
#' Assigns to each node a distance from cell body.
#'
#' @param n neuron object with soma
#'
#' @return vector with distances from soma
#' @importFrom igraph distances
#' @rdname dotprops-topo
#' @export
#' @seealso \code{\link{dotprops}}, \code{\link{ngraph}}
#' @examples
#' get_distance_to_soma(Cell07PNs[[1]])
get_distance_to_soma <- function(n) {
  gw <- as.ngraph(n, weights=TRUE)
  dst <- distances(gw, v = rootpoints(n))
  as.numeric(dst)
}

#' @export
#' @rdname dotprops
#' @param k Number of nearest neighbours to use for tangent vector calculation 
#'   (set to k=20 when passed NULL)
#' @param Labels Vector of labels for each point e.g. identifying axon vs 
#'   dendrite. The default value \code{NULL} will produce class-specific default
#'   behaviour for different classes of input object, \code{TRUE} always uses 
#'   labels when an incoming object has them and \code{FALSE} never uses labels.
#' @param na.rm Whether to remove \code{NA} points (default FALSE)
#' @param topo_features topological features of each dotprops
#' @importFrom nabor knn
#' @references The dotprops format is essentially identical to that developed 
#'   in:
#'   
#'   Masse N.Y., Cachero S., Ostrovsky A., and Jefferis G.S.X.E. (2012). A 
#'   mutual information approach to automate identification of neuronal clusters
#'   in \emph{Drosophila} brain images. Frontiers in Neuroinformatics 6 (00021).
#'   \doi{10.3389/fninf.2012.00021}
dotprops.default<-function(x, k=NULL, Labels=NULL, na.rm=FALSE, topo_features=NULL,
                           ...){
  # store labels from SWC format data if this is a neuron
  x=xyzmatrix(x)
  if(is.null(k)) k=20

  if(length(Labels) && length(Labels)!=nrow(x))
  stop("Length of Labels does not match number of points!")

  if(na.rm){
    narows=rowSums(is.na(x))>0
    if(any(narows)) {
      x=x[!narows,]
      # don't forget to remove labels for NA points as well
      if(length(Labels)) Labels=Labels[!narows]
    }
  }
  npoints=nrow(x)
  if(npoints<k) stop("Too few points to calculate properties")
  if(ncol(x)!=3) stop("points must be a N x 3 matrix")
  
  alpha=rep(0,npoints)
  vect=matrix(0,ncol=3,nrow=npoints)
  
  nns=knn(x, k=k)
  # transpose points to 3xN because 
  # R arithmetic of matric / vector operates column-wise
  pointst=t(x)
  for(i in 1:npoints){
    indNN=nns$nn.idx[i,]
    pt=pointst[,indNN]
    cpt=pt-rowMeans(pt)
    
    inertia=cpt%*%t(cpt)
    v1d1<-eigen(inertia,symmetric=TRUE)
    
    alpha[i]=(v1d1$values[1]-v1d1$values[2])/sum(v1d1$values)
    vect[i,]=v1d1$vectors[,1]
  }
  rlist=list(points=x,alpha=alpha,vect=vect)
  
  rlist$labels=Labels
  
  if (!is.null(topo_features)) {
    rlist$topo <- topo_features
  }
  
  attr(rlist,'k')=k
  return(as.dotprops(rlist))
}

# internal function to convert a dotprops object to SWC
# representation.
dotprops2swc<-function(x, label=0L, veclength=1, radius=0) {
  
  # compute segments based on points and tangent vectors
  halfvect=x$vect/2*veclength
  starts=x$points-halfvect
  stops=x$points+halfvect
  interleaved=matrix(t(cbind(starts,stops)),ncol=3,byrow=T)
  
  n=nrow(x$points)
  # make sequence of parent ids
  # should look like -1, 1, -1, 2, -1, 3 ...
  parents=as.vector(t(cbind(-1L, 
                            seq.int(from=1L, length.out=n, by=2L)
  )))
  df=data.frame(PointNo=seq.int(2*n), Label=label)
  df[,c("X","Y","Z")]=interleaved
  df$R=radius
  df$Parent=parents
  df
}

#' all.equal method tailored to dotprops objects
#' 
#' @details This method is required because the direction vectors are computed
#'   using an eigenvector decomposition where the sign of the eigenvector is
#'   essentially random and subject to small numerical instabilities. Therefore
#'   it does not usually make sense to check the value of vect exactly.
#' @param target,current dotprops objects to compare
#' @param check.attributes Whether to check attributes (false by default)
#' @param absoluteVectors Whether to check only the absolute value of
#'   eigenvectors for equality (default TRUE, see details)
#' @param ... Additional arguments passed to base \code{all.equal}.
#' @export
#' @examples
#' # equal using default 
#' kc1=kcs20[[1]]
#' kc1.recalc=dotprops(kc1)
#' # not equal due to differences in attributes and vectors
#' all.equal.default(kc1.recalc, kc1)
#' # still not equal because of tangent vector flipping
#' all.equal.default(kc1.recalc, kc1, check.attributes=FALSE)
#' # equal using appropriate method
#' stopifnot(isTRUE(all.equal(kc1.recalc, kc1)))
#' # NB identical when recalculated on same setup from same data
#' stopifnot(isTRUE(all.equal.default(kc1.recalc, dotprops(kc1))))
all.equal.dotprops<-function(target, current, check.attributes=FALSE, 
                             absoluteVectors=TRUE, ...){
  if(!is.dotprops(current)) stop("current is not a dotprop object")
  if(absoluteVectors){
    target$vect=abs(target$vect)
    current$vect=abs(current$vect)
  }
  NextMethod(check.attributes=check.attributes, ...)
}

#' 3D plots of dotprops objects using rgl package
#' 
#' @details Tangent vectors are plotted by \code{segments3d} and centred on the
#'   relevant point. Points are plotted by \code{points3d}.
#'   
#'   \code{color} will be recycled by \code{points3d} and \code{segments3d}. 
#'   However in the special case that \code{color} has length equal to the 
#'   number of points in \code{x}, then it will be duplicated before being 
#'   passed to \code{segments3d} so that the result is that each vector is 
#'   coloured uniformly according to \code{color} (since segments3d expects 2
#'   colours for each line segment, blending them if they are different).
#' @param x A dotprops object
#' @param scalevecs Factor by which to scale unit vectors (numeric, default: 
#'   1.0)
#' @param alpharange Restrict plotting to points with \code{alpha} values in 
#'   this range to plot (default: null => all points). See 
#'   \code{\link{dotprops}} for definition of \code{alpha}.
#' @param color Character or numeric vector specifying colours for 
#'   points/vectors. See details.
#' @param PlotPoints,PlotVectors Whether to plot points and/or tangent vectors 
#'   (logical, default: tangent vectors only)
#' @param UseAlpha Whether to scale tangent vector length by the value of 
#'   \code{alpha}
#' @param ... Additional arguments passed to \code{points3d} and/or 
#'   \code{segments3d}
#' @inheritParams plot3d.neuronlist
#' @return invisible list of results of rgl plotting commands
#' 
#' @export
#' @seealso \code{\link{dotprops}, \link[rgl]{plot3d}, \link[rgl]{points3d}, 
#'   \link[rgl]{segments3d}}
#' @examples
#' \donttest{
#' open3d()
#' plot3d(kcs20[[1]])
#' nclear3d()
#' plot3d(kcs20[[1]],col='red')
#' nclear3d()
#' plot3d(kcs20[[1]],col='red',lwd=2)
#' plot3d(kcs20[[2]],col='green',lwd=2)
#' }
plot3d.dotprops<-function(x, scalevecs=1.0, alpharange=NULL, color='black',
                          PlotPoints=FALSE, PlotVectors=TRUE, UseAlpha=FALSE, 
                          ..., gridlines = FALSE,
                          plotengine = getOption('nat.plotengine')){
  # rgl's generic plot3d will dispatch on this
  if (!is.null(alpharange))
    x=subset(x,x$alpha<=alpharange[2] & x$alpha>=alpharange[1])
  plotengine <- check_plotengine(plotengine)
  rlist=list()
  if (plotengine == 'plotly') {
    psh <- openplotlyscene()$plotlyscenehandle
    params=list(...)
    opacity <- if("alpha" %in% names(params)) params$alpha else 1
  }
  
  if(PlotPoints){
    if (plotengine == 'rgl'){
      rlist$points=points3d(x$points, color=color, ...)
    } else {
      plotdata <- as.data.frame(x$points)
      psh <- psh %>% 
        plotly::add_trace(data = plotdata, x = ~X, y = ~Y , z = ~Z, 
        hoverinfo = "none",type = 'scatter3d', mode = 'markers',
        opacity = opacity, marker=list(color = color, size = 3))
    }
  }
    
  if(PlotVectors){
    if(length(color)>1 && length(color)==nrow(x$points)){
      color=rep(color,rep.int(2,length(color)))
    }
    halfvect=x$vect/2*scalevecs
    if(UseAlpha) halfvect=halfvect*x$alpha
    starts=x$points-halfvect
    stops=x$points+halfvect
    interleaved=matrix(t(cbind(starts,stops)),ncol=3,byrow=T)
    if (plotengine == 'rgl'){
        rlist$segments=segments3d(interleaved, color=color, ...)
    } else {
      tempdata <- interleaved
      tempseglist <- list()
      for (tempidx in seq(1,nrow(tempdata)/2)) {
        tempseglist[[tempidx]] <- c(1,2)+2*(tempidx-1)
      }
      
      tempdata<-do.call(rbind,sapply(tempseglist,function(s) {rbind(tempdata[s,],NA)},simplify=FALSE))
      
      plotdata <- as.data.frame(tempdata)
      names(plotdata) <- c('X','Y','Z')
      psh <- psh %>% 
        plotly::add_trace(data = plotdata, x = ~X, y = ~Y , z = ~Z, 
        hoverinfo = "none", type = 'scatter3d', mode = 'lines',
        opacity = opacity, line=list(color = color, width = 4))
    }
  }
  if (plotengine == 'rgl'){
    invisible(rlist)
  } else {
 
    psh <- psh %>% plotly::layout(showlegend = FALSE, scene=list(camera=.plotly3d$camera))
    if(gridlines == FALSE){
      psh <- psh %>% plotly::layout(scene = list(xaxis=.plotly3d$xaxis,
                                                 yaxis=.plotly3d$yaxis,
                                                 zaxis=.plotly3d$zaxis))
    }
    
    assign("plotlyscenehandle", psh, envir=.plotly3d)
    psh
  }
}

#' @rdname plot.neuron
#' @description \code{plot.dotprops} plots a 2D projection of a
#'   \code{\link{dotprops}} format object
#' @details \code{plot.dotprops} is limited in that 1) it cannot plot somata
#'   directly (this is handled by \code{\link{plot.neuronlist}}) and 2) it can
#'   only plot a frontal (XY) view.
#' @inheritParams plot.neuron
#' @inheritParams plot3d.dotprops
#' @export
#' @examples
#'
#' plot(kcs20[[1]], col='red')
#' # NB soma ignored
#' plot(kcs20[[1]], col='red', soma=TRUE)
#' plot(kcs20[1], col='red', soma=TRUE)
plot.dotprops<-function(x, scalevecs=1.0, alpharange=NULL, col='black', 
                        PlotPoints=FALSE, PlotVectors=TRUE, UseAlpha=FALSE,
                        asp=1, add=FALSE, axes=TRUE, tck=NA,
                        boundingbox=NULL, xlim=NULL, ylim=NULL, 
                        soma=FALSE, ...){
  if (!is.null(alpharange))
    x=subset(x,x$alpha<=alpharange[2] & x$alpha>=alpharange[1])
  
  
  if(!add) {
    if(is.null(boundingbox))
      boundingbox=boundingbox(x, na.rm=TRUE)
    if(is.null(xlim)) xlim=boundingbox[,1]
    if(is.null(ylim)) ylim=rev(boundingbox[,2])
    plot.new()
    plot.window(xlim, ylim, asp=asp)
    if(axes) {
      box()
      axis(2, tck=tck)
      axis(1, tck=tck)
    }
  }
  if(PlotPoints){
    points(x$points, col=col, pch=20, ...)
  }
    
  if(PlotVectors){
    if(length(col)>1 && length(col)==nrow(x$points)){
      col=rep(col,rep.int(2,length(col)))
    }
    halfvect=x$vect/2*scalevecs
    if(UseAlpha) halfvect=halfvect*x$alpha
    starts=x$points-halfvect
    stops=x$points+halfvect
    # interleaved=matrix(t(cbind(starts[,1:2],stops[,1:2])),ncol=2,byrow=T)
    segments(starts[,1], starts[,2], stops[,1], stops[,2], col=col, ...)
  }
  invisible()
}

#' Subset points in dotprops object that match given conditions
#'
#' @details \code{subset} defines either logical or numeric indices, in which
#'   case these are simply applied to the matrices that define the
#'   \code{points}, \code{vect} fields of the \code{dotprops} object etc OR a
#'   function (which is called with the 3D points array and returns T/F. OR an
#'   expression vector).
#' @param x A dotprops object
#' @param subset A subset of points defined by indices, an expression or a
#'   function (see Details)
#' @param ... Additional parameters (currently ignored)
#' @inheritParams subset.neuron
#' @return subsetted dotprops object
#' @export
#' @seealso \code{prune.dotprops}, \code{subset.neuron}
#' @examples
#' ## subset using indices ...
#' dp=kcs20[[10]]
#' dp1=subset(dp, 1:50)
#'
#' # ... or an expression
#' dp2=subset(dp, alpha>0.7)
#' front=subset(dp, points[,'Z']<40)
#' # use a helper function
#' between=function(x, lower, upper) x>=lower & x<=upper
#' middle=middle=subset(dp, between(points[,'Z'], 40, 60))
#'
#' # plot results in 3D
#' \donttest{
#' plot3d(front, col='red')
#' plot3d(middle, col='green')
#' plot3d(dp, col='blue')
#' }
#'
#' \dontrun{
#'
#' ## subset using an selection function
#' s3d=select3d()
#' dp1=subset(dp, s3d(points))
#' # special case of previous version
#' dp2=subset(dp, s3d)
#' # keep the points that were removed from dp2
#' dp2.not=subset(dp, s3d, invert=TRUE)
#' # (another way of doing the same thing)
#' dp2.not=subset(dp, Negate(s3d))
#' stopifnot(all.equal(dp1, dp2))
#' dp2=subset(dp, alpha>0.5 & s3d(pointd))
#' dp3=subset(dp, 1:10)
#'
#' ## subset each dotprops object in a whole neuronlist
#' plot3d(kcs20)
#' s3d=select3d()
#' kcs20.partial = nlapply(kcs20, subset, s3d)
#' clear3d()
#' plot3d(kcs20.partial, col='red')
#' plot3d(kcs20, col='grey')
#' }
#'
#' \dontrun{
#' ## subset dotprops by mesh
#' #' library(nat.flybrains)
#' # extract calyx surface and convert to mesh3d
#' calyx=as.mesh3d(subset(MBL.surf, "MB_CA_L"))
#' # subset one neuron with this surface
#' kcs20.2_ca=subset(kcs20[[2]], function(x) pointsinside(x, calyx))
#' shade3d(calyx, alpha=0.2)
#' plot3d(kcs20.2_ca, lwd=3, col='black')
#' 
#' ## subset neuronlist of dotprops by mesh
#' peduncle=as.mesh3d(subset(MBL.surf, "MB_PED_L"))
#' kcs20.ped=nlapply(kcs20, function(x) subset(x, pointsinside(x, peduncle)))
#' shade3d(peduncle, alpha=.2)
#' plot3d(kcs20.ped)
#' }
subset.dotprops<-function(x, subset, invert=FALSE, ...){
  e <- substitute(subset)
  r <- eval(e, x, parent.frame())
  if (!is.logical(r) && !is.numeric(r)) {
    # a function that tells us whether a point is in or out
    if(is.function(r)) r=r(x$points)
    else stop("Cannot evaluate subset")
  }
  if(is.logical(r)) {
    r <- r & !is.na(r)
    if(invert) r <- !r
  } else if(is.numeric(r)) {
    if(invert) r <- setdiff(seq_len(nvertices(x)), r)
  } else {
    stop("Subset must evaluate to a logical or numeric index")
  }
  
  x$points=x$points[r,,drop=F]
  x$alpha=x$alpha[r]
  x$vect=x$vect[r,,drop=F]
  if(!is.null(x$labels)) x$labels=x$labels[r]
  x
}

#' prune an object by removing points near (or far) from a target object
#' @export
#' @param x The object to prune. (e.g. \code{dotprops} object, see details)
#' @param target Another object with 3D points that will determine which points 
#'   in x are kept.
#' @param ... Additional arguments for methods (eventually passed to 
#'   \code{prune.default})
#' @seealso \code{\link{prune_strahler}}, \code{\link{spine}},
#'   \code{\link{prune_vertices}}, \code{\link{subset.neuron}}
#' @examples
#' ## prune single neurons
#' \donttest{
#' plot3d(kcs20[[1]],col='blue')
#' plot3d(kcs20[[2]],col='red')
#' }
#' # prune neuron 2 down to points that are close to neuron 1
#' neuron2_close=prune(kcs20[[2]], target=kcs20[[1]], maxdist=10)
#' \donttest{
#' plot3d(neuron2_close, col='cyan', lwd=3)
#' }
#' neuron2_far=prune(kcs20[[2]], target=kcs20[[1]], maxdist=10, keep='far')
#' \donttest{
#' plot3d(neuron2_far, col='magenta', lwd=3)
#' }
#' 
#' ## Prune a neuron with a neuronlist
#' pruned=prune(kcs20[[11]], kcs20[setdiff(1:20, 11)], maxdist=8)
#' \donttest{
#' plot3d(pruned, col='red', lwd=3)
#' plot3d(kcs20[[11]], col='green', lwd=3)
#' plot3d(kcs20,col='grey')
#' }
prune<-function(x, target, ...) UseMethod("prune")

#' @export
#' @details \code{prune.neuron} depends on a more basic function 
#'   \code{\link{prune_vertices}} and is also related to
#'   \code{\link{subset.neuron}}.
#' @rdname prune
#' @seealso \code{\link{subset.neuron}}
#' @family neuron
prune.neuron<-function(x, target, ...){
  indstokeep=NextMethod(return.indices=TRUE)
  indstodrop=setdiff(seq(nrow(x$d)), which(indstokeep))
  prune_vertices(x, indstodrop, ...)
}

#' @export
#' @rdname prune
#' @seealso \code{\link{subset.dotprops}}
prune.dotprops<-function(x, target, ...){
  inds=NextMethod(return.indices=TRUE)
  subset(x, inds)
}

#' @export
#' @rdname prune
prune.neuronlist<-function(x, target, ...){
  nlapply(x, prune, target=target, ...)
}

#' @export
#' @rdname prune
#' @param maxdist The threshold distance for keeping points
#' @param keep Whether to keep points in x that are near or far from the target
#' @param return.indices Whether to return the indices that pass the test rather
#'   than the 3D object/points (default \code{FALSE})
#' @importFrom nabor knn
prune.default<-function(x, target, maxdist, keep=c("near","far"), 
                        return.indices=FALSE, ...){
  keep=match.arg(keep, c("near", "far"))
  xyzx=xyzmatrix(x)
  nn_dists=drop(knn(xyzmatrix(target), xyzx, k=1)$nn.dists)
  inds=if(keep=="near") nn_dists<=maxdist else nn_dists>maxdist
  if(return.indices) inds else xyzx[inds, , drop=FALSE]
}
