#' neuron: class to represent traced neurons
#' 
#' neuron objects consist of a list containing multiple fields describing the 3D
#' location and connectivity of points in a traced neuron. The critical fields 
#' of a neuron, n, are n$d which contains a dataframe in SWC format and 
#' n$SegList which contains a representation of the neuron's topology used for 
#' most internal calculations. For historical reasons, n$SegList is limited to a
#' \emph{single fully-connected} tree. If the tree contains multiple unconnected
#' subtrees, then these are stored in n$SubTrees and nTrees will be >1; the 
#' "master" subtree (typically the one with the most points) will then be stored
#' in n$SegList and n$NumPoints will refer to the number of points in that 
#' subtree, not the whole neuron.
#' @description \code{neuron} makes a neuron object from appropriate variables.
#' @details StartPoint, BranchPoints, EndPoints are indices matching the rows of 
#'   the vertices in \code{d} \strong{not} arbitrary point numbers typically 
#'   encoded in \code{d$PointNo}.
#' @rdname neuron
#' @export
#' @family neuron
#' @seealso \code{\link{neuronlist}}
#' @param d matrix of vertices and associated data in SWC format
#' @param NumPoints Number of points in master subtree
#' @param StartPoint,BranchPoints,EndPoints Nodes of the neuron
#' @param SegList List where each element contains the vertex indices for a 
#'   single segments of the neuron, starting at root.
#' @param SubTrees List of SegLists where a neuron has multiple unconnected 
#'   trees (e.g. because the soma is not part of the graph, or because the 
#'   neuronal arbour has been cut.)
#' @param ... Additional fields to be included in neuron. Note that if these 
#'   include CreatedAt, NodeName, InputFileStat or InputFileMD5, they will 
#'   override fields of that name that are calculated automatically.
#' @param InputFileName Character vector with path to input file
#' @param NeuronName Character vector containing name of neuron or a function 
#'   with one argument (the full path) which returns the name. The default
#'   (\code{NULL}) sets NeuronName to the file name without the file extension.
#' @param MD5 Logical indicating whether to calculate MD5 hash of input
#' @importFrom tools md5sum
neuron<-function(d, NumPoints=nrow(d), StartPoint, BranchPoints=integer(), EndPoints,
                 SegList, SubTrees=NULL, InputFileName=NULL, NeuronName=NULL, ...,
                 MD5=TRUE){
  
  coreFieldOrder=c("NumPoints", "StartPoint", "BranchPoints", 
               "EndPoints", "nTrees", "NumSegs", "SegList", "SubTrees","d" )
  mcl<-as.list(match.call())
  n=c(mcl,list(NumPoints=NumPoints,
             nTrees=ifelse(is.null(SubTrees),1,length(SubTrees)),
             NumSegs=length(SegList)))
  n=n[intersect(coreFieldOrder,names(n))]
  n=lapply(n, eval)
  if(is.null(InputFileName)){
    n$NeuronName=NeuronName
  } else {
    if(is.null(NeuronName)) NeuronName=sub("\\.[^.]+$","",basename(InputFileName))
    else if(is.function(NeuronName)) NeuronName=NeuronName(InputFileName)
    neuron_extra=list(NeuronName=NeuronName,
                      InputFileName=InputFileName,
                      CreatedAt=Sys.time(),
                      NodeName=Sys.info()["nodename"])
    if(file.exists(InputFileName)) {
      neuron_extra$InputFileStat=file.info(InputFileName)[1,]
      if(MD5) neuron_extra$InputFileMD5=md5sum(InputFileName)
    }
    n=c(neuron_extra,n)
  }
  dots=list(...)
  if(length(dots)) {
    n[names(dots)]=dots
  }
  as.neuron(n)
}

#' @param x A neuron or other object to test/convert
#' @description \code{is.neuron} will check if an object looks like a neuron.
#' @param Strict Whether to check class of neuron or use a more relaxed
#'   definition based on object being a list with a SegList component.
#' @export
#' @rdname neuron
is.neuron<-function(x,Strict=FALSE) {
  inherits(x,"neuron") ||
    (!Strict && is.list(x) && !is.null(x$SegList))
}

#' @description \code{as.neuron} will convert a suitable object to a neuron
#' @export
#' @rdname neuron
as.neuron<-function(x, ...) UseMethod('as.neuron')

#' @export
as.neuron.neuron<-function(x, ...) x

#' @rdname neuron
#' @export
#' @details Columns will be ordered c('PointNo','Label','X','Y','Z','W','Parent')
#' @description \code{as.neuron.data.frame} expects a block of SWC format data
as.neuron.data.frame<-function(x, ...) {
  x=normalise_swc(x)
  as.neuron(as.ngraph(x), vertexData=x, ...)
}

#' Normalise an SWC format block of neuron morphology data
#' @param x A data.frame containing neuron morphology data
#' @param requiredColumns Character vector naming columns we should have
#' @param ifMissing What to do if \code{x} is missing a required column
#' @param includeExtraCols Whether to include any extra columns include in 
#'   code{x}
#' @param defaultValue A list containing default values to use for any missing 
#'   columns
#' @return A data.frame containing the normalised block of SWC data with 
#'   standard columns in standard order.
#' @seealso \code{\link{as.neuron.data.frame}}, \code{\link{seglist2swc}}
#' @details Note that row.names of the resultant data.frame will be set to NULL
#'   so that they have completely standard values.
#' @export
normalise_swc<-function(x, requiredColumns=c('PointNo','Label','X','Y','Z','W','Parent'),
                        ifMissing=c('usedefaults','warning','stop'),
                        includeExtraCols=TRUE,
                        defaultValue=list(PointNo=seq.int(nrow(x)),Label=2L,
                                          X=NA_real_,Y=NA_real_,Z=NA_real_,
                                          W=NA_real_,Parent=NA_integer_)
                        ){
  cnx=colnames(x)
  ifMissing=match.arg(ifMissing)
  if(ifMissing!='usedefaults') ifMissing=match.fun(ifMissing)
  missingColumns=setdiff(requiredColumns, cnx)
  if(length(missingColumns)){
    if(is.character(ifMissing)){
      x[,missingColumns]=defaultValue[missingColumns]
    } else {
      ifMissing("Columns ", paste(missingColumns, collapse=","), " are missing from x")
    }
  }
  
  # if only giving a warning for missing columns we may may be missing some
  selectedCols=intersect(requiredColumns, colnames(x))
  if(includeExtraCols)
    selectedCols=c(selectedCols, setdiff(cnx, requiredColumns))
  row.names(x)=NULL
  x[,selectedCols]
}

#' Make SegList (and other core fields) from full graph of all nodes and origin
#' 
#' @description \code{as.neuron.ngraph} converts a graph (typically an 
#'   \code{ngraph} object) to a neuron
#' @details Uses a depth first search on the tree to reorder using the given 
#'   origin.
#' @details When the graph contains multiple subgraphs, only one will be chosen 
#'   as the master tree and used to construct the SegList of the resultant 
#'   neuron. However all subgraphs will be listed in the SubTrees element of the
#'   neuron and nTrees will be set appropriately.
#' @details When the graph vertices have a label attribute derived from PointNo,
#'   the origin is assumed to be specified with respect to the vertex labels 
#'   rather than the raw vertex ids.
#' @param vertexData A dataframe with SWC fields especially X,Y,Z,W,PointNo,
#'   Parent.
#' @param origin Root vertex, matched against labels (aka PointNo) when 
#'   available (see details)
#' @param Verbose Whether to be verbose (default: FALSE)
#' @return A list with elements: 
#'   (NumPoints,StartPoint,BranchPoints,EndPoints,nTrees,NumSegs,SegList, 
#'   [SubTrees]) NB SubTrees will only be present when nTrees>1.
#' @export
#' @method as.neuron ngraph
#' @rdname neuron
#' @seealso \code{\link{graph.dfs}, \link{as.seglist}}
as.neuron.ngraph<-function(x, vertexData=NULL, origin=NULL, Verbose=FALSE, ...){
  # translate origin into raw vertex id if necessary 
  if(length(origin)){
    vertex_labels=igraph::V(x)$label
    if(!is.null(vertex_labels)){
      origin=match(origin,vertex_labels)
      if(is.na(origin)) stop("Invalid origin")
    }
  }
  # save original vertex ids
  igraph::V(x)$vid=seq.int(igraph::vcount(x))
  # check if we have multiple subgraphs
  if(no.clusters(x)>1){
    if(!length(origin)){
      # no origin specified, will pick the biggest subtree
      # decompose into list of subgraphs
      gg=igraph::decompose.graph(x)
      # reorder by descending number of vertices
      gg=gg[order(sapply(gg,igraph::vcount), decreasing=TRUE)]
      subtrees=lapply(gg, as.seglist, Verbose=Verbose)
      sl=subtrees[[1]]
      masterg=gg[[1]]
    } else {
      # origin specified, subtree containing origin will be the master
      cg=igraph::clusters(x)
      master_tree_num=cg$membership[origin]
      # make a master graph with the vertices from subgraph including origin
      masterg=igraph::induced.subgraph(x, which(cg$membership==master_tree_num))
      # ... and then corresponding seglist
      sl=as.seglist(masterg, origin=origin)
      # now deal with remaining vertices
      remainderg=igraph::induced.subgraph(x, which(cg$membership!=master_tree_num))
      gg=igraph::decompose.graph(remainderg)
      # reorder by descending number of vertices
      gg=gg[order(sapply(gg,igraph::vcount), decreasing=TRUE)]
      subtrees=c(list(sl),lapply(gg, as.seglist, Verbose=Verbose))
    }
    nTrees=length(subtrees)
  } else {
    # this is a well-behaved graph that is immediately ready to be master graph
    # of neuron
    sl=as.seglist(masterg<-x, origin=origin, Verbose=Verbose)
    subtrees=list(sl)
    nTrees=1
  }
  if(length(sl)==0 || length(sl[[1]])<2)
    stop("Invalid neuron! Must contain at least one segment with 2 points")
  # Finalise StartPoint - should always be head point of first segment
  StartPoint=sl[[1]][1]
  
  # sort out the vertex information
  # TODO refactor this into a separate function e.g. normalise.swc since
  # we need to do something similar in as.neuron.dataframe and seglist2swc etc
  d=data.frame(PointNo=get.vertex.attribute(x,'label'))
  if(is.null(vertexData)){
    # get vertex information from graph object
    xyz=xyzmatrix(x)
    if(!is.null(xyz)) d[,c("X","Y","Z")]=xyz[igraph::V(x),]
    diam=V(x)$diam
    if(!is.null(diam)) d[, "W"]=diam[igraph::V(x)]
  } else {
    # we were given a block of vertexData
    if("PointNo"%in%colnames(vertexData)){
      # to be on the safe side, let's reorder the vertex data so that PointNos
      # matches PointNos stored in graph as vertex attributes
      ids=match(d$PointNo, vertexData$PointNo)
      if(any(is.na(ids)))
        stop("Mismatch between PointNos stored in graph and those in vertexData")
      d=cbind(d, vertexData[ids,colnames(vertexData)!='PointNo'])
    } else {
      # the datablock doesn't have a PointNo field so just assume that it
      # is ordered according to the vertex indices
      if(nrow(d)!=nrow(vertexData))
        stop("vertexData does not have PointNo column and does not have as",
             "many rows as there are points in the graph.")
      d=cbind(d, vertexData)
    }
  }
  
  d=seglist2swc(x=subtrees,d=d)
  d=normalise_swc(d)
  n=list(d=d,NumPoints=igraph::vcount(masterg),
         StartPoint=StartPoint,
         BranchPoints=branchpoints(masterg, original.ids='vid'),
         EndPoints=endpoints(masterg, original.ids='vid'),
         nTrees=nTrees,
         NumSegs=length(sl),
         SegList=sl)
  if(nTrees>1) n=c(n,list(SubTrees=subtrees))
  # NB unname is a guard against named fields coming in.
  # The name would otherwise turn into a suffix in the neuron that would cause
  # trouble when constructing the neuron
  # e.g. InputFileName->InputFileName.XT23L1
  if(!missing(...)) n=c(n, lapply(pairlist(...), unname))
  do.call(neuron, n)
}

#' @description \code{as.neuron.default} will add class "neuron" to a neuron-like
#'   object.
#' @rdname neuron
#' @export
as.neuron.default<-function(x, ...){
  if(is.null(x)) return (NULL)
  if(is.neuron(x,Strict=FALSE)) class(x)=c("neuron",class(x))
  x
}

#' Arithmetic for neuron coordinates
#'
#' If x is a 1-vector or a 3-vector, multiply xyz only
#' If x is a 4-vector, multiply xyz and diameter by that
#' TODO Figure out how to document arithemtic functions in one go
#' @param n a neuron
#' @param x (a numeric vector to multiply neuron coords in neuron)
#' @return modified neuron
#' @export
#' @rdname neuron-arithmetic
#' @seealso neuron
#' @examples
#' n1<-Cell07PNs[[1]]*2
#' n2<-Cell07PNs[[1]]*c(2,2,2,1)
#' stopifnot(all.equal(n1,n2))
#' n3<-Cell07PNs[[1]]*c(2,2,4)
#' @method * neuron
`*.neuron` <- function(n,x) {
  # TODO use xyzmatrix
  
  nd=n$d[,c("X","Y","Z","W")]
  stopifnot(is.numeric(x))
  lx=length(x)
  if(lx==1) nd[,-4]=nd[,-4]*x
  else if(lx%in%c(3,4)) nd[,1:lx]=t(t(nd[,1:lx])*x)
  else stop("expects a numeric vector of length 1, 3 or 4")
  n$d[,colnames(nd)]=nd
  n
}

#' @method + neuron
#' @rdname neuron-arithmetic
#' @export
`+.neuron` <- function(n,x) {
  if(!is.numeric(x))
    stop("expects a numeric vector")
  nd=n$d[,c("X","Y","Z","W")]
  lx=length(x)
  if(lx==1) nd[,-4]=nd[,-4]+x
  else if(lx%in%c(3,4)) nd[,1:lx]=t(t(nd[,1:lx])+x)
  else stop("expects a numeric vector of length 1, 3 or 4")
  n$d[,colnames(nd)]=nd
  n
}

#' @method - neuron
#' @rdname neuron-arithmetic
#' @export
`-.neuron` <- function(n,x) {
  if(!missing(x))
    n+(-x)
  else {
    n*-1
  }
}

#' @method / neuron
#' @rdname neuron-arithmetic
#' @export
`/.neuron` <- function(n,x) n*(1/x)

#' Divide neuron coords by a factor (and optionally center)
#'
#' @details Note that if scale=TRUE, the neuron will be rescaled to unit sd in each axis
#' likewise if center=TRUE, the neuron will be centred around the axis means
#' @param x A neuron
#' @param center 3-vector to subtract from x,y,z coords
#' @param scale 3-vector used to divide x,y,z coords
#' @return neuron with scaled coordinates
#' @method scale neuron
#' @export
#' @seealso \code{\link{scale.default}}
#' @examples
#' n1.scaledown=scale(Cell07PNs[[1]],scale=c(2,2,3))
#' n1.scaleup=scale(Cell07PNs[[1]],scale=1/c(2,2,3))
scale.neuron<-function(x,center=FALSE,scale=FALSE){
  xyzmatrix(x)<-scale(xyzmatrix(x),scale=scale,center=center)
  x
}

#' Check equality on key fields of neuron object
#' 
#' @inheritParams base::all.equal.default
#' @param fieldsToCheck Which fields in the neuron are always check
#' @param fieldsToCheckIfPresent These fields are only checked if they are 
#'   present
#' @param fieldsToExclude Character vector of fields to exclude from check
#' @param CheckSharedFieldsOnly Logical whether to check shared fields only 
#'   (default: FALSE)
#' @param ... additional arguments passed to \code{all.equal}
#' @method all.equal neuron
#' @export
#' @seealso \code{\link{all.equal}}
#' @examples
#' x=Cell07PNs[[1]]
#' y=x
#' y$NeuronName='rhubarb'
#' # NOT TRUE
#' all.equal(x, y)
#' # TRUE
#' all.equal(x, y, fieldsToExclude='NeuronName')
all.equal.neuron<-function(target,current,tolerance=1e-6,check.attributes=FALSE,
                           fieldsToCheck=c("NumPoints", "StartPoint", "BranchPoints",
                                           "EndPoints", "NumSegs", "SegList", "d"), 
                           fieldsToCheckIfPresent=c("NeuronName","nTrees","SubTrees"),
                           fieldsToExclude=character(),
                           CheckSharedFieldsOnly=FALSE, ...){
  if(length(fieldsToCheck)==1 && is.na(fieldsToCheck))
    fieldsToCheck=names(current)
  
  if(!is.neuron(target) || !is.neuron(current))
    return ("target and current must both be neurons")
  fieldsInCommon=intersect(names(target),names(current))
  # figure out which of the optional fields to check are present
  fieldsToCheckIfPresent=intersect(fieldsInCommon,fieldsToCheckIfPresent)
  # and add those to the fields to check 
  fieldsToCheck=unique(c(fieldsToCheck,fieldsToCheckIfPresent))
  if(CheckSharedFieldsOnly){
    fieldsToCheck=intersect(fieldsInCommon,fieldsToCheck)
  } else{
    # check all core fields
    missingfields=setdiff(fieldsToCheck,names(current))
    if(length(missingfields)>0)
      return(paste("Current missing fields: ",missingfields))
    missingfields=setdiff(fieldsToCheck,names(target))
    if(length(missingfields)>0)
      return(paste("Target missing fields: ",missingfields))		
  }
  fieldsToCheck=setdiff(fieldsToCheck,fieldsToExclude)
  all.equal(target[fieldsToCheck],current[fieldsToCheck],
            tolerance=tolerance, check.attributes=check.attributes, ...)
}

#' Calculate length of all segments in neuron
#' 
#' @param x A neuron
#' @param all Whether to calculate lengths for all segments when there are 
#'   multiple subtrees (default: \code{FALSE})
#' @param flatten Whether to flatten the lists of lists into a single list when 
#'   \code{all=TRUE}
#' @param sumsegment Whether to return the length of each segment (when 
#'   {sumsegment=TRUE}, the default) or a list of vectors of lengths of each
#'   individual edge in the segment.
#' @details A segment is an ubranched portion of neurite consisting of at least 
#'   one vertex joined by edges.Only segments in x$SegList will be calculated 
#'   unless \code{all=TRUE}. Segments containing only one point will have 0 
#'   length.
#' @return A \code{vector} of lengths for each segment or when 
#'   \code{sumsegment=FALSE} a \code{list} of vectors
#' @export
#' @seealso \code{\link{as.seglist.neuron}}
#' @examples
#' summary(seglengths(Cell07PNs[[1]]))
#' hist(unlist(seglengths(Cell07PNs[[1]], sumsegment = FALSE)),
#'   br=20, main='histogram of edge lengths', xlab='edge lengths /microns')
seglengths=function(x, all=FALSE, flatten=TRUE, sumsegment=TRUE){
  # convert to numeric matrix without row names
  sts<-as.seglist(x, all=all, flatten=flatten)
  d=data.matrix(x$d[, c("X", "Y", "Z")])
  if(all && !flatten) {
    lapply(sts, function(st) sapply(st, 
                                    function(s) seglength(d[s, ], sum=sumsegment),
                                    simplify=sumsegment, USE.NAMES = FALSE ))
  } else sapply(sts, function(s) seglength(d[s, ], sum=sumsegment),
                simplify=sumsegment, USE.NAMES = FALSE)
}

# Calculate length of single segment in neuron
seglength=function(ThisSeg, sum=TRUE){
  #ThisSeg is an array of x,y and z data points
  #In order to calculate the length
  #Need to find dx,dy,dz
  #Then find sqrt(dx^2+...)
  #Then sum over the path
  if(nrow(ThisSeg)==1) return(0)
  ds=diff(ThisSeg)
  edgelengths=sqrt(rowSums(ds*ds))
  if(sum) sum(edgelengths) else unname(edgelengths)
}

#' Resample an object with a new spacing
#' @param x An object to resample
#' @param ... Additional arguments passed to methods
#' @export
resample<-function(x, ...) UseMethod("resample")

#' resample a neuron with a new spacing
#' 
#' @param stepsize The new spacing along the tracing
#' @details \code{resample.neuron} Floating point columns including X,Y,Z,W will
#'   be interpolated using linear interpolation, while integer or factor columns
#'   will be interpolated using constant interpolation. See \code{\link{approx}}
#'   for details.
#' @export
#' @rdname resample
#' @seealso \code{\link{approx}}, \code{\link{seglengths}}
resample.neuron<-function(x, stepsize, ...) {
  # extract original vertex array before resampling
  cols=c("X","Y","Z")
  if(!is.null(x$d$W)) cols=c(cols, 'W')
  if(!is.null(x$d$Label)) cols=c(cols, 'Label')
  d=x$d[, cols, drop=FALSE]
  if(any(is.na(d[,1:3])))
    stop("Unable to resample neurons with NA points")

  # fetch all segments and process each segment in turn
  sl=as.seglist(x, all = T, flatten = T)
  for (i in seq_along(sl)){
    s=sl[[i]]
    # interpolate this segment
    dold=d[s, , drop=FALSE]
    dnew=resample_segment(dold, stepsize=stepsize, ...)
    if(is.null(dnew)) next
    # if we've got here, we need to do something
    # add new points to the end of the swc block
    # and give them sequential point numbers
    newids=seq.int(from = nrow(d)+1, length.out = nrow(dnew))
    d=rbind(d, dnew)
    # replace internal ids in segment so that proximal point is connected to head
    # and distal point is connected to tail
    sl[[i]]=c(s[1], newids, s[length(s)])
  }
  rownames(d)=NULL
  # in order to avoid re-ordering the segments when as.neuron.ngraph is called
  # we can renumber the raw indices in the seglist (and therefore the vertices)
  # in a strictly ascending sequence based on the seglist
  old_ids=unique(unlist(sl))
  sl=lapply(sl, function(x) match(x, old_ids))
  # reorder vertex information to match this
  d=d[old_ids,]
  swc=seglist2swc(sl, d)
  as.neuron(swc, origin=match(x$StartPoint, old_ids))
}

# Interpolate ordered 3D points (optionally w diameter)
# NB returns NULL if unchanged (when too short or <=2 points) 
# and only returns _internal_ points, omitting the head and tail of a segment
resample_segment<-function(d, stepsize, ...) {
  # we must have at least 2 points to resample
  if(nrow(d) < 2) return(NULL)
  
  dxyz=xyzmatrix(d)
  if(!is.data.frame(d)) d=as.data.frame(d)
  # we should only resample if the segment is longer than the new stepsize
  l=seglength(dxyz)
  if(l<=stepsize) return(NULL)
  
  # figure out linear position of new internal points
  internalPoints=seq(stepsize, l, by=stepsize)
  nInternalPoints=length(internalPoints)
  # if the last internal point is actually in exactly the same place 
  # as the endpoint then discard it
  if(internalPoints[nInternalPoints]==l) {
    internalPoints=internalPoints[-length(internalPoints)]
    nInternalPoints=length(internalPoints)
  }
  
  # find cumulative length stopping at each original point on the segment
  diffs=diff(dxyz)
  cumlength=c(0,cumsum(sqrt(rowSums(diffs*diffs))))
  
  # find 3d position of new internal points
  # using linear approximation on existing segments
  # make an emty list for results
  dnew=list()
  for(n in names(d)) {
    dnew[[n]] <- if(!all(is.finite(d[[n]]))) {
      rep(NA, nInternalPoints)
    } else {
      approx(cumlength, d[[n]], internalPoints, 
             method = ifelse(is.double(d[[n]]), "linear", "constant"))$y
    }
  }
  as.data.frame(dnew)
}
