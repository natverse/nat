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
#' @examples 
#' ## See help for functions listed in See Also for more detailed examples
#' ## Basic properties
#' # a sample neuron 
#' n = Cell07PNs[[1]]
#' # inspect its internal structure
#' str(n)
#' # summary of 3D points
#' summary(xyzmatrix(n))
#' # identify 3d location of endpoints
#' xyzmatrix(n)[endpoints(n),]
#' 
#' ## Other methods
#' # plot
#' plot(n)
#' # all methods for neuron objects
#' methods(class = 'neuron')
#' 
#' ## Neurons as graphs 
#' # convert to graph and find longest paths by number of nodes
#' ng=as.ngraph(n)
#' hist(igraph::distances(ng))
#' # ... or in distances  microns
#' ngw=as.ngraph(n, weights=TRUE)
#' hist(igraph::distances(ngw))
#' 
#' # converting back and forth between neurons and graphs
#' g=as.ngraph(Cell07PNs[[1]])
#' gstem=igraph::induced.subgraph(g, 1:10)
#' # this is fine
#' plot(gstem)
#' plot(as.neuron(gstem))
#' 
#' # but if you had an undirected graph 
#' ug=igraph::as.undirected(gstem)
#' # you get a warning because there is no explicit origin for the graph
#' as.neuron(ug)
#' 
#' # If you need finer control of the conversion process 
#' gstem2=as.ngraph(ug, root = 10)
#' plot(gstem2)
#' plot(as.neuron(gstem2))
neuron<-function(d, NumPoints=nrow(d), StartPoint, BranchPoints=integer(), EndPoints,
                 SegList, SubTrees=NULL, InputFileName=NULL, NeuronName=NULL, ...,
                 MD5=TRUE){get
  
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
    (!Strict && is.list(x) && !is.null(x[["SegList"]]))
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
#' @param origin Root vertex, matched against names (aka PointNo) when 
#'   available (see details)
#' @param Verbose Whether to be verbose (default: FALSE)
#' @return A list with elements: 
#'   (NumPoints,StartPoint,BranchPoints,EndPoints,nTrees,NumSegs,SegList, 
#'   [SubTrees]) NB SubTrees will only be present when nTrees>1.
#' @export
#' @method as.neuron ngraph
#' @importFrom igraph V V<- vcount decompose.graph
#' @rdname neuron
#' @seealso \code{\link{graph.dfs}, \link{as.seglist}}
as.neuron.ngraph<-function(x, vertexData=NULL, origin=NULL, Verbose=FALSE, ...){
  # translate origin into raw vertex id if necessary 
  if(length(origin)){
    vertex_names=igraph::V(x)$name
    if(!is.null(vertex_names)){
      origin=match(origin, vertex_names)
      if(is.na(origin)) stop("Invalid origin")
    }
  }
  # save original vertex ids
  igraph::V(x)$vid=seq.int(igraph::vcount(x))
  # check if we have multiple subgraphs
  if(igraph::no.clusters(x)>1){
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
  d=data.frame(PointNo=get.vertex.attribute(x,'name'))
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

#' @description \code{as.neuron.igraph} will convert an \code{ngraph} compatible
#'   \code{\link[igraph]{igraph}} object into a \code{neuron}.
#' @export
#' @rdname neuron
as.neuron.igraph <- function(x, ...) {
  must_have=c("X","Y","Z","diam")
  if(!all(must_have %in% igraph::vertex_attr_names(x)))
    stop("Sorry this does not look like an ngraph! Missing XYZ/diam data!")
  as.neuron.ngraph(x, ...)
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
#' If x is a 1-vector or a 3-vector, operate on xyz only
#' If x is a 4-vector, apply operation to xyz and diameter
#' @param e1 a neuron
#' @param e2 (a numeric vector to multiply neuron coords in neuron)
#' @return modified neuron
#' @export
#' @rdname neuron-arithmetic
#' @seealso neuron
#' @examples
#' n1<-Cell07PNs[[1]]*2
#' n2<-Cell07PNs[[1]]*c(2,2,2,1)
#' stopifnot(all.equal(n1,n2))
#' n3<-Cell07PNs[[1]]*c(2,2,4)
Ops.neuron <- function(e1, e2=NULL) {
  ok <-
    switch(
      .Generic,
      `-` = ,
      `*` = ,
      `/` = ,
      `+` = ,
      `^` = ,
      '>' = ,
      '<' = ,
      '>=' = ,
      '<=' = TRUE,
      FALSE
    )
  if (!ok) {
    stop(gettextf("%s not meaningful for neurons", sQuote(.Generic)))
  }
  r=e1
  lx=length(e2)
  e1 <- if(lx==3) {
    t(xyzmatrix(e1))
  } else if(lx==4) {
    t(cbind(xyzmatrix(e1), e1$d$W))
  } else if(lx>1) {
    stop("second operand must be a numeric vector of length 0, 1, 3 or 4")
  } else {
    e1=xyzmatrix(r)
  }
  # I don't exactly know why it is necessary to change this directly, but if not
  # NextMethod dispatches on original class of e1 even when I specify object=
  .Class=class(e1)
  res <- NextMethod(generic=.Generic)
  if(lx==4) {
    r$d$W=res[4,]
    res=t(res[-4,])
  } else if(lx==3){
    res=t(res)
  }
  xyzmatrix(r)=res
  r
}

#' Scale and centre neuron 3D coordinates
#' 
#' @details If \code{scale=TRUE}, the neuron will be rescaled to unit sd in each
#'   axis. If \code{center=TRUE}, the neuron will be centred around the axis
#'   means. See \code{base::\link{scale.default}} for additional details.
#' @param x A neuron
#' @param center 3-vector to subtract from x,y,z coords
#' @param scale 3-vector used to divide x,y,z coords
#' @return neuron with scaled coordinates
#' @method scale neuron
#' @export
#' @seealso \code{\link{scale.default}}, \code{\link{Ops.neuron}}
#' @aliases scale
#' @examples
#' n1.scaledown=scale(Cell07PNs[[1]],scale=c(2,2,3))
#' n1.scaleup=scale(Cell07PNs[[1]],scale=1/c(2,2,3))
scale.neuron<-function(x, center=TRUE, scale=TRUE){
  xyzmatrix(x)<-scale(xyzmatrix(x),scale=scale,center=center)
  x
}

#' Check equality on key fields of neuron object
#' 
#' @inheritParams base::all.equal.default
#' @param fieldsToCheck Which fields in the neuron are always checked. The
#'   special value of \code{NA} indicates that \bold{all} fields in the neurons
#'   will be compared.
#' @param fieldsToCheckIfPresent These fields are only checked if they are 
#'   present
#' @param fieldsToExclude Character vector of fields to exclude from check
#' @param CheckSharedFieldsOnly Logical whether to check shared fields only 
#'   (default: FALSE)
#' @param ... additional arguments passed to \code{all.equal}
#' @method all.equal neuron
#' @export
#' @seealso \code{\link[base]{all.equal}}
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
    fieldsToCheck=union(names(current), names(target))
  
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
#' @details A segment is an unbranched portion of neurite consisting of at least 
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
                                    function(s) seglength(d[s, , drop=FALSE], sum=sumsegment),
                                    simplify=sumsegment, USE.NAMES = FALSE ))
  } else sapply(sts, function(s) seglength(d[s, , drop=FALSE], sum=sumsegment),
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
#' @family neuron
resample.neuron<-function(x, stepsize, ...) {
  # extract original vertex array before resampling
  cols=c("X","Y","Z")
  if(!is.null(x$d$W)) cols=c(cols, 'W')
  # if(!is.null(x$d$Label)) cols=c(cols, 'Label')
  d=data.matrix(x$d[, cols, drop=FALSE])
  # if(!is.null(d$Label)) d$Label=as.integer(d$Label)
  if(any(is.na(d[,1:3])))
    stop("Unable to resample neurons with NA points")

  # fetch all segments and process each segment in turn
  sl=as.seglist(x, all = T, flatten = T)
  npoints=nrow(d)
  dl=list(d)
  for (i in seq_along(sl)){
    s=sl[[i]]
    # interpolate this segment
    dold=d[s, , drop=FALSE]
    dnew=resample_segment(dold, stepsize=stepsize, ...)
    if(is.null(dnew)) next
    dl[[length(dl)+1]]=dnew
    # if we've got here, we need to do something
    # add new points to the end of the swc block
    # and give them sequential point numbers
    newids=seq.int(from = npoints+1, length.out = nrow(dnew))
    npoints=npoints+nrow(dnew)
    # replace internal ids in segment so that proximal point is connected to head
    # and distal point is connected to tail
    sl[[i]]=c(s[1], newids, s[length(s)])
  }
  d=do.call(rbind, dl)
  d=as.data.frame(d)
  rownames(d)=NULL
  # let's deal with the label column which was dropped - assume that always the
  # same within a segment
  head_idxs=sapply(sl, "[", 1)
  seglabels=x$d$Label[head_idxs]

  # in order to avoid re-ordering the segments when as.neuron.ngraph is called
  # we can renumber the raw indices in the seglist (and therefore the vertices)
  # in a strictly ascending sequence based on the seglist
  # it is *much* more efficient to compute this on a single vector rather than
  # separately on each segment in the seglist. However this does involve some
  # gymnastics 
  usl=unlist(sl)
  old_ids=unique(usl)
  # reorder vertex information to match this
  d=d[old_ids,]

  node_per_seg=sapply(sl, length)
  df=data.frame(id=usl, seg=rep(seq_along(sl), node_per_seg))
  df$newid=match(df$id, old_ids)
  sl=split(df$newid, df$seg)
  labels_by_seg=rep(seglabels, node_per_seg)
  # but there will be some duplicated ids (branch points) that we must remove
  d$Label=labels_by_seg[!duplicated(df$newid)]
  swc=seglist2swc(sl, d)
  as.neuron(swc, origin=match(x$StartPoint, old_ids))
}

# Interpolate ordered 3D points (optionally w diameter)
# NB returns NULL if unchanged (when too short or <=2 points) 
# and only returns _internal_ points, omitting the head and tail of a segment
#' @importFrom stats approx
resample_segment<-function(d, stepsize, ...) {
  # we must have at least 2 points to resample
  if(nrow(d) < 2) return(NULL)
  
  dxyz=xyzmatrix(d)
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
  
  # find 3D position of new internal points
  # using linear approximation on existing segments
  # make an emty object for results
  # will have same type (matrix/data.frame as input)
  dnew=matrix(nrow=nInternalPoints, ncol=ncol(d))
  colnames(dnew)=colnames(d)
  if(is.data.frame(d)){
    dnew=as.data.frame(dnew)
  }
  for(n in seq.int(ncol(dnew))) {
    dnew[,n] <- if(!all(is.finite(d[,n]))) {
      rep(NA, nInternalPoints)
    } else {
      approx(cumlength, d[,n], internalPoints, 
             method = ifelse(is.double(d[,n]), "linear", "constant"),
             ties="ordered")$y
    }
  }
  dnew
}

#' Smooth the 3D coordinates of a neuron skeleton
#'
#' \code{smooth_neuron} smooths a neuron. 
#' @param n Neuron to smooth
#' @param method Smoothing method
#' @param ... Additional parameters passed to segment smoothing functions
#'
#' @return A new neuron with smoothed 3d coordinates
#' @export
#'
#' @examples
#' ns=smooth_neuron(Cell07PNs[[1]], sigma=2)
#' # plot in 2D zooming in on axon terminals 
#' plot(Cell07PNs[[1]], col='grey', xlim=c(260,290), ylim=c(115,90))
#' plot(ns, col='red', add=TRUE)
#' \donttest{
#' # 3D plot
#' plot3d(Cell07PNs[[1]], col='grey')
#' plot3d(ns, col='red')
#' }
smooth_neuron <- function(n, method=c("gauss", "spline"), ...) {
  method=match.arg(method)
  FUN=get(paste0('smooth_segment_', method), mode='function')
  # iterate over segments
  d=xyzmatrix(n)
  if(any(is.na(d[,1:3])))
    stop("Unable to resample neurons with NA points")
  
  # fetch all segments and process each segment in turn
  sl=as.seglist(n, all = T, flatten = T)
  for (i in seq_along(sl)){
    s=sl[[i]]
    # interpolate this segment
    d[s,]=FUN(d[s, , drop=FALSE], ...)
  }
  xyzmatrix(n) <- d
  n
}

#' @rdname smooth_neuron
#' @param xyz A block of 3D coordinates defining an unbranched segment
#' @param sigma The standard deviation of the Gaussian smoothing kernel (which
#'   has the same spatial units as the object being smoothed)
#' @importFrom stats dnorm
smooth_segment_gauss <- function(xyz, sigma, ...){
  if(nrow(xyz)<2) return(xyz)
  # make variable t as the cumulative position along segment
  t=c(0,cumsum(seglength(xyz, sum = F)))
  
  xyzt=xyz
  
  for(i in 2:(nrow(xyz)-1)){
    weights=dnorm(abs(t-t[i]), sd = sigma)
    weights=weights/sum(weights)
    xyzt[i,]=colSums(xyz*weights)
  }
  xyzt
}

#' @importFrom stats smooth.spline
smooth_segment_spline <- function(xyz, ...) {
  if(nrow(xyz)<4) return(xyz)
  # make variable t as the cumulative position along segment
  t=c(0,cumsum(seglength(xyz, sum = F)))
  # ensure that ends are fixed
  w=rep(1,nrow(xyz))
  w[1]=1e6
  w[length(w)]=w[1]
  
  fittedxyz=apply(xyz, 2, function(u) smooth.spline(t, u, w=w, ...)$y)
  fittedxyz
}

#' Subset neuron by keeping only vertices that match given conditions
#'
#' @details \code{subset} defines which vertices of the neuron to keep and is
#'   one of \itemize{
#'
#'   \item logical or numeric indices, in which case these are simply used to
#'   index the vertices in the order of the data.frame \code{x$d}. Note that any
#'   NA values are ignored.
#'
#'   \item a function (which is called with the 3D points array and returns T/F
#'   vector)
#'
#'   \item an expression evaluated in the context of the \code{x$d} data.frame
#'   containing the SWC specification of the points and connectivity of the
#'   neuron. This can therefore refer e.g. to the X,Y,Z location of vertices in
#'   the neuron.
#'
#'   }
#'
#'   Note that due to its use of
#'   \href{http://adv-r.had.co.nz/Computing-on-the-language.html}{non-standard
#'   evaluation} \code{subset.neuron}, which is convenient interactive use but
#'   can be fragile when used inside other functions. If you run into trouble it
#'   is recommended to use the underlying \code{\link{prune_vertices}} function.
#' @param x A neuron object
#' @param subset A subset of points defined by indices, an expression, or a
#'   function (see Details)
#' @param invert Whether to invert the subset criteria - a convenience when
#'   selecting by function or indices.
#' @param ... Additional parameters (passed on to \code{\link{prune_vertices}})
#' @return subsetted neuron
#' @export
#' @seealso \code{\link{prune.neuron}}, \code{\link{prune_vertices}},
#'   \code{\link{subset.dotprops}}
#' @examples
#' n=Cell07PNs[[1]]
#' # keep vertices if their X location is > 2000
#' n1=subset(n, X>200)
#' # diameter of neurite >1
#' n2=subset(n, W>1)
#' # first 50 nodes
#' n3=subset(n, 1:50)
#' # everything but first 50 nodes
#' n4=subset(n, 1:50, invert=TRUE)
#'
#' ## subset neuron by graph structure
#' # first plot neuron and show the point that we will use to divide the neuron
#' n=Cell07PNs[[1]]
#' plot(n)
#' # this neuron has a tag defining a point at which the neuron enters a brain
#' # region (AxonLHEP = Axon Lateral Horn Entry Point)
#' points(t(xyzmatrix(n)[n$AxonLHEP, 1:2]), pch=19, cex=2.5)
#'
#' # now find the points downstream (distal) of that with respect to the root
#' ng=as.ngraph(n)
#' # use a depth first search
#' distal_points=igraph::graph.dfs(ng, root=n$AxonLHEP, unreachable=FALSE,
#'   neimode='out')$order
#' distal_tree=subset(n, distal_points)
#' plot(distal_tree, add=TRUE, col='red', lwd=2)
#'
#' # Find proximal tree as well
#' # nb this does not include the AxonLHEP itself as defined here
#' proximal_points=setdiff(igraph::V(ng), distal_points)
#' proximal_tree=subset(n, proximal_points)
#' plot(proximal_tree, add=TRUE, col='blue', lwd=2)
#'
#' \dontrun{
#' ## subset using interactively defined spatial regions
#' plot3d(n)
#' # nb you can save this select3d object using save or saveRDS functions
#' # for future non-interactive use
#' s3d=select3d()
#' n4=subset(n, s3d(xyzmatrix(n)))
#' # special case of previous version
#' n5=subset(n, s3d)
#' stopifnot(all.equal(n4,n5))
#' # keep the points that were removed from n1
#' n4.not=subset(n,Negate(s3d))
#' # vertices with x position > 100 and inside the selector function
#' n6=subset(n,X>100 & s3d(X,Y,Z))
#'
#' ## subset each neuron object in a whole neuronlist
#' n10=Cell07PNs[1:10]
#' plot3d(n10, lwd=0.5, col='grey')
#' n10.crop = nlapply(n10, subset, X>250)
#' plot3d(n10.crop, col='red')
#'
#' ## subset a neuron using a surface
#' library(nat.flybrains)
#' # extract left lateral horn surface and convert to mesh3d
#' lh=as.mesh3d(subset(IS2NP.surf, "LH_L"))
#' # subset neuron with this surface
#' x=subset(Cell07PNs[[1]], function(x) pointsinside(x, lh))
#' shade3d(lh, alpha=0.3)
#' plot3d(x, lwd=3, col='blue')
#' # Now find the parts of the neuron outside the surface
#' y=subset(Cell07PNs[[1]], function(x) Negate(pointsinside)(x, lh))
#' plot3d(y, col='red', lwd=2)
#' }
#' @family neuron
subset.neuron<-function(x, subset, invert=FALSE, ...){
  e <- substitute(subset)
  r <- eval(e, x$d, parent.frame())
  if (!is.logical(r) && !is.numeric(r)) {
    # a function that tells us whether a point is in or out
    if(is.function(r)) r=subset(x$d[,c("X","Y","Z")])
    else stop("Cannot evaluate subset")
  }
  if(is.logical(r)) {
    r <- r & !is.na(r)
    r <- which(r)
  } else if(is.numeric(r)) {
    r=r[!is.na(r)]
  } else stop("Subset must evaluate to a logical or numeric index")
  # nb !invert since prune_vertices drops vertices whereas subset.neuron keeps vertices
  prune_vertices(x, r, invert=!invert, ...)
}


#' Simplify a neuron to the longest tree with n branch points
#'
#' @details If the neuron already contains fewer than or exactly the requested
#'   number of branches, then the original neuron is returned. The approach is
#'   to build up the new neuron starting from the longest tree including no
#'   branches all the way up to the longest tree containing n branches. The
#'   distance calculations are only carried out once so it should be reasonably
#'   efficient. Nevertheless at each iteration, the longest path from the tree
#'   so far to the newly selected leaf is calculated and it is likely that this
#'   step could be avoided. Furthermore for large values of n, pruning excess
#'   branches rather than building would presumably be more efficient.
#'
#' @param x A \code{\link[nat]{neuron}} to simplify
#' @param n Required number of branch points (default=1, minimum 0)
#' @param invert Whether to keep the simplified backbone (when
#'   \code{invert=FALSE}, the default) or its inverse.
#' @param ... Additional arguments (currently ignored)
#'
#' @return The simplified \code{neuron} or the untouched original neuron for
#'   neurons that have <=n branch points.
#' @author Gregory Jefferis \email{jefferis@gmail.com}
#' @export
#' @seealso \code{\link[nat]{spine}}
#' @examples
#' \donttest{
#' n=Cell07PNs[['ECA34L']]
#' n.simp=simplify_neuron(n)
#' n.simp4=simplify_neuron(n, n=4)
#' 
#' plot(n.simp, col='red', add = TRUE)
#' plot(n.simp4, col='blue', add = TRUE)
#' plot(n, col='green', WithNodes = FALSE)
#'
#' # calculate the inverse as well
#' n.simp4.inv=simplify_neuron(n, n=4, invert=TRUE)
#' plot(n.simp4, col='blue')
#' plot(n.simp4.inv, col='red', add = TRUE)
#' }
#' 
#' # 3D plots
#' \dontrun{
#' nclear3d()
#' plot3d(n.simp, col='red', add = TRUE)
#' plot3d(n.simp4, col='blue', add = TRUE)
#' plot3d(n, col='green', WithNodes = FALSE)
#' }
#' 
#' # or with plotly where transparency works
#' \dontrun{
#' options(nat.plotengine = 'plotly')
#' nclear3d()
#' plot3d(n.simp, col='red', alpha = 0.5, add = TRUE)
#' plot3d(n.simp4, col='blue', alpha = 0.5, add = TRUE)
#' plot3d(n, col='green', alpha = 0.5, WithNodes = FALSE)
#' }
#' 
simplify_neuron <- function(x, n=1, invert=FALSE, ...) {
  #Step 1a:Get the number of branch points in the neuron.. 
  nbps=length(branchpoints(x))
  #Step 1b:Compare with the actual branch points requested.. 
  if (nbps <= n)
    return(x)
  if (n < 0)
    stop("Must request >=0 branch points!")
  
  #Step 2:Convert to ngraph object.. 
  ng = as.ngraph(x, weights = T)
  
  if (!igraph::is_dag(ng)) {
    stop("I can't simplify neurons with cycles!")
  }
  
  # Step 3a: Compute all the leaf nodes..
  leaves=setdiff(endpoints(ng, original.ids=FALSE), rootpoints(ng, original.ids=FALSE))
  # Step 3b: Compute all the branch nodes..
  bps=branchpoints(ng, original.ids=FALSE)
  # Step 3c: Compute the distance from all the branch nodes to the leaf nodes let's call 
  # it distance table.. Rows are branch nodes and Columns are leaf nodes..
  dd=igraph::distances(ng, v=bps, to=leaves, mode = 'out')
  
  # Step 3d: Compute the decendant paths for all the branch nodes (to get the possibilities
  #           of different paths from a particular branch point)..
  bpdesccount=igraph::ego_size(ng, order = 1, nodes = bps, mode='out', mindist = 1)
  
  names(bpdesccount)=bps
  bpsused=rep(0L, length(bps))
  names(bpsused)=bps
  lp_verts=list()
  
  # The approach is to find the longest tree (lets call it spine) from the root to the 
  # farthest leaf node first..
  # Then additional branches are added to the spine such that they are longest that can be
  # added..
  
  for (i in 0:n) {
    if (i == 0) {
      # Step 4a: Compute the spine, so for that compute the farthest leaf node 
      # with the distance table..
      start = rootpoints(ng, original.ids=FALSE)
      
      # Step 4b: Find out the leaf node which is farthest
      furthest_leaf_idx = which.max(apply(dd, 2, robust_max))
      
    } else {
      # Step 7a: Find out the leaf node which is farthest Select only the branch nodes 
      # that are currently in our selected spine
      # Also, choose only those who still have some unused descendent paths..
      bps_available = bpsused > 0 & bpsused < bpdesccount
      
      # find the length we could add for each leaf
      # nb this will be the smallest value that can be added to
      # currently selected nodes
      # Step 7b: Now choose the shortest path to all the leaf nodes from the available 
      # branch nodes.. 
      additional_length = apply(dd[bps_available, , drop=FALSE], 2, min, na.rm = T)
      # remove any infinite values
      additional_length[!is.finite(additional_length)] = 0
      
      # Step 7c: Now choose the leaf nodes that is the farthest of distance among all the 
      # shortest path to the leaf nodes.. 
      furthest_leaf_idx = which.max(additional_length)
      start_idx = which.min(dd[bps_available, furthest_leaf_idx])
      # Step 7d: Get the vertex index in the original graph
      start = bps[which(bps_available)[start_idx]]
    }
    # Step 5 or 8: Avoid the choosen leaf from distance computations in next iteration
    furthest_leaf = leaves[furthest_leaf_idx]
    # strike off selected leaf
    dd[, furthest_leaf_idx] = Inf
    
    # Step 6 or 9: Find the path to that chosen leaf from the start point..
    path = leafpath(ng, start, furthest_leaf)
    lp_verts[[i+1]]=path
    # add one to count of any bps used
    bpsused[bps %in% path] = bpsused[bps %in% path] + 1
  }
  
  # Step 10: Find the edgelist from the path(start to farthest leaf)..
  el=EdgeListFromSegList(lp_verts)
  
  # Step 11: Prune the edgelist and choose to retain the pruned one (based on the invert flag)..
  prune_edges(ng, el, invert = !invert)
}

leafpath <- function(ng, from, to) {
  res=igraph::get.shortest.paths(ng,from = from,to = to,mode = "out")
  as.integer(res$vpath[[1]])
}

robust_max=function(x) {
  x=x[is.finite(x)]
  if(length(x)) max(x) else {
    warning("Some points in neuron cannot be reached! Multiple trees?")
    -Inf
  }
}

# Simplify a neuron to the longest tree with n branch points
#
# @details If the neuron already contains fewer than or exactly the requested
#   number of branches, then the original neuron is returned. The approach is 
#   basically to compute the longest path in the neuron and then collect that path,
#   further delete that path and recompute the longest path and then collect that new
#   path again. Perform this approach until you reach the requested number of branchpoints.
#
# @inheritParams simplify_neuron 
# @author Sridhar Jagannathan \email{j.sridharrajan@gmail.com} 
# @export
# @seealso \code{\link[nat]{simplify_neuron}} 
# @examples
# \dontrun{
# library(catmaid)
# dl1=read.neuron.catmaid(catmaid_skids('annotation:DL1')[1])
# dl1.simp=simplify_neuron2(dl1)
# dl1.simp4=simplify_neuron2(dl1, n=4)
# nclear3d()
# plot3d(dl1, col='green', alpha = 0.5, WithNodes = FALSE)
# plot3d(dl1.simp4, col='blue', alpha = 0.5, add = TRUE)
# plot3d(dl1.simp, col='red', alpha = 0.5, add = TRUE)
#
# # calculate the inverse as well
# dl1.simp4.inv=simplify_neuron2(dl1, n=4, invert=TRUE)
# nclear3d()
# plot3d(dl1.simp4, col='blue',alpha = 0.5)
# plot3d(dl1.simp4.inv, col='red', alpha = 0.5, add = TRUE)
# }
simplify_neuron2 <- function(x, n=1, invert=FALSE, ...) {
  
  #Step 1a: Handle subtrees here..
  x <- handlesubtrees(x)
  
  #Step 1b:Get the number of branch points in the neuron.. 
  nbps=length(branchpoints(x))
  
  #Step 1c:Compare with the actual branch points requested.. 
  if (nbps <= n)
    return(x)
  if (n < 0)
    stop("Must request >=0 branch points!")
  
  #Step 2:Convert to ngraph object.. 
  ng = as.ngraph(x, weights = T)
  
  if (!igraph::is_dag(ng)) {
    stop("I can't simplify neurons with cycles!")
  }
  
  
  # Step 3: Compute the longest path..
  pts_pair <- igraph::farthest_vertices(ng, directed=T)
  node_ids <- igraph::vertex_attr(ng, 'name')
  pts_start <- which(igraph::vertex_attr(ng, 'name')[pts_pair$vertices[1]]  == node_ids)
  pts_stop <-  which(igraph::vertex_attr(ng, 'name')[pts_pair$vertices[2]]  == node_ids)
  
  longestpath <- as.integer(igraph::shortest_paths(graph = ng, from = pts_start, to = pts_stop)$vpath[[1]])
  path_list=list()
  path_list[[1]] = longestpath
 
  if(n == 0){
    
  }else{
    templongpath = longestpath
    tempng = ng
    # Step 4: Now keep on adding as many branches as required..
    for (idx in 1:n) {
          
      tempneuron= prune_edges(tempng, templongpath, invert = FALSE)
      tempng <- as.ngraph(tempneuron, weights = T)
      
      pts_pair <- igraph::farthest_vertices(tempng, directed=T)
      
      pts_start <- which(igraph::vertex_attr(tempng, 'name')[pts_pair$vertices[1]]  == node_ids)
      pts_stop <-  which(igraph::vertex_attr(tempng, 'name')[pts_pair$vertices[2]]  == node_ids)
      
      #now take the path in the original neuron..
      templongpath <- as.integer(igraph::shortest_paths(graph = ng, from = pts_start, to = pts_stop)$vpath[[1]])
      
      path_list[[idx+1]]= templongpath
      
      #now get the path for the reduced graph for use in next iteration..
      templongpath <- as.integer(igraph::shortest_paths(graph = tempng, from = pts_pair$vertices[1], 
                                                        to = pts_pair$vertices[2])$vpath[[1]])
    }
  }
  
  # Step 5: Find the edgelist for the paths..
  el=EdgeListFromSegList(path_list)
  
  # Step 6: Prune the edgelist and choose to retain the pruned one (based on the invert flag)..
  req_neuron <- prune_edges(ng, el, invert = !invert)
  req_neuron
  
}


handlesubtrees=function(x) {
  if(exists("x$nTrees") && x$nTrees>1){
    warning("Multiple trees here so choosing one along the main(long) path")
    ng = as.ngraph(x, weights = T)
    sub_gs <- igraph::components(ng)$membership
    pts_pair <- igraph::farthest_vertices(ng, directed=T)
    maincomp <- sub_gs[pts_pair$vertices[1]]
    rm_nodes <- names(which(sub_gs != maincomp))
    ng <- igraph::delete.vertices(ng, rm_nodes)
    x <- as.neuron(ng)
  }
  x
}

#' Stitch multiple fragments into single neuron using minimum spanning tree
#'
#' @details The neurons are joined using the minimum spanning tree i.e. the tree
#'   that minimises the sum of edge weights (here, the Euclidean distance). The
#'   neuron is rooted in the largest cluster; if this cluster contained the
#'   original root of the neuron, then this should be retained.
#' @param x Fragments that could be \code{\link{neuronlist}} or a single neuron
#'   with multiple unconnected subtrees.
#' @param thresh_el The threshold distance (units in microns) above which new vertices 
#' will not be connected (default=1000, set to NULL to disable this feature). This parameter prevents the 
#' merging of vertices that are so far away from the main neuron such that they are likely to be spurious.
#' @return A single \code{neuron} object containing all input fragments.
#' @author Sridhar Jagannathan \email{j.sridharrajan@gmail.com}
#' @seealso \code{\link{simplify_neuron}}, \code{\link{spine}},
#'   \code{\link{ngraph}}, \code{igraph::\link[igraph]{mst}}
#' @export
#' @examples
#'
#' n=Cell07PNs[['ECA34L']]
#' n_main=simplify_neuron(n, n = 10)
#' n_branches=simplify_neuron(n, n = 10, invert = TRUE)
#'
#' # this is the setup
#' plot(n_main, col='red', WithNodes=FALSE)
#' plot(n_branches, col='blue', add=TRUE, WithNodes=FALSE)
#'
#' # make a neuronlist containing the two fragments
#' nl=neuronlist(n_main, n_branches)
#' # and stitch those
#' n_stitched=stitch_neurons_mst(nl)
#'
#' \dontrun{
#' # look at the neurons in 3D - they appear identical in this case
#' plot3d(n, alpha=.5, col='cyan', WithNodes=FALSE)
#' plot3d(n_stitched, alpha=.5, col='red', WithNodes=FALSE)
#' }
stitch_neurons_mst <- function(x, thresh_el = 1000) {
  #Step 1: First check if the input is fragmented and then proceed further..
  if(is.neuron(x)){
    if(x$nTrees == 1){
      message("The neuron is already complete, so not stiching it further")
      return(x)
    }
  }
  
  #Step 2: Check input and convert to ngraph object..
  if(is.neuronlist(x)){
    if(length(x)==0) return(NULL)
    if(length(x)==1) return(x[[1]])
    
    for (baseidx in 1:(length(x)-1)){
        for (targetidx in (baseidx+1):length(x)) {
            # if there are any repeats in PointNo, augment those in subsequent neuron
            if(any(x[[baseidx]]$d$PointNo%in%x[[targetidx]]$d$PointNo)){
                x[[targetidx]]$d$PointNo=x[[targetidx]]$d$PointNo+max(x[[baseidx]]$d$PointNo)
                x[[targetidx]]$d$Parent=x[[targetidx]]$d$Parent+max(x[[baseidx]]$d$PointNo)
              }
        }
    }
    #Convert the neuronlist to list of ngraph objects..
    ngraph_list=nlapply(x, FUN = function(x) {as.ngraph(x, weights = T, method = 'seglist')})
    
    #Make a single ngraph object..
    ng=as.ngraph(igraph::disjoint_union(ngraph_list))
    
  } else if(is.neuron(x)) {
    ng = as.ngraph(x, weights = T)
  } else {
    stop("x must be a neuronlist or a neuron object!")
  }
  
  #make a copy of the master graph now..
  masterng <- ng
  
  #Step 3a: Find the rootnode of the largest cluster, this will be the rootnode of the stitched neuron..
  cc=igraph::components(ng)
  sorted=order(cc$csize, decreasing = T)
  root_points <- rootpoints(ng, original.ids = FALSE)
  master_root <- names(which(cc$membership[root_points] == sorted[1]))
  
  #Step 3b: Set the weights of existing edges to zero, so they are not affected by
  #any operations done below..
  igraph::E(ng)$weight = 0
  
  #Step 4: Find all the leaf nodes now, these are the potential sites to stitch..
  root_id <- which(names(V(ng)) == master_root)
  leaves = setdiff(1:length(V(ng)),root_id) #actually use all of them, slower but accurate..
   
  #Step 5: Create list of edges that will be added (from potential sites..) and compute the distances between them..
  #Now divide them into clusters and compute combinations among them..
  cc_membership <- unique(cc$membership)
  combedge_start <- NULL
  combedge_stop <- NULL
  
  xyz <- xyzmatrix(x) #for using with knn..
  
  for (clusterbaseidx in 1:(length(cc_membership)-1)){
    for (clustertargetidx in (clusterbaseidx+1):length(cc_membership)) {
      #cat('\nComparing base cluster idx#:',clusterbaseidx,'with target cluster idx#:',clustertargetidx)
      leaves_base <- intersect(leaves, which(cc$membership == clusterbaseidx))
      leaves_target <- intersect(leaves, which(cc$membership == clustertargetidx))
      
       #take the locations of pts in base leaf and target leaf..
       base_pt=xyz[leaves_base,]
       target_pt=xyz[leaves_target,]
       
       #find the pts nearest in base leaf to the target leaf..
       minval <- min(10,length(leaves_base),length(leaves_target))
       nnres=nabor::knn(base_pt, target_pt, k=minval)
       
       targetpt_idx=which.min(nnres$nn.dists)
       basept_idx=nnres$nn.idx[targetpt_idx,1]
       
       trunc_base <- leaves_base[basept_idx]
      
       #compute the combination now, only based on the truncated base leaf pt and all pts in target leaf
       leaves_combo <- expand.grid(trunc_base,leaves_target)
      
      #leaves_combo <- expand.grid(leaves_base,leaves_target)
      combedge_start <- c(combedge_start,leaves_combo[,1])
      combedge_stop <- c(combedge_stop,leaves_combo[,2])
    }
    
  }
  
  #edge_list <- utils::combn(leaves,m =2)
  edge_list <- rbind(combedge_start,combedge_stop)
  
  
  starts<-edge_list[1,]
  stops<-edge_list[2,]
  
  # nb drop = FALSE to ensure that we always have a matrix
  vecs=xyz[stops, , drop=FALSE] - xyz[starts, , drop=FALSE]
  weights=sqrt(rowSums(vecs*vecs))
  
  #Step 6: Add those edges and create a new graph..
  mod_graph <- igraph::add_edges(ng,edge_list,"weight"= weights)
  
  #Step 7: Find the minimum spanning tree of the new graph..
  mst <- igraph::minimum.spanning.tree(mod_graph)
  
  #Step 8: Find the new edges added by the mst..
  new_edges <- igraph::difference(igraph::E(mst),igraph::E(masterng))
  
  newedge_list <- igraph::as_ids(new_edges)
  
  rawel <- igraph::ends(mst,new_edges)
  weight_attr <- igraph::edge_attr(graph = mst, 'weight', index = new_edges)
  nodenames <- names(igraph::V(masterng))
  
  #Step 9: Now add the new edges in the master graph vertex by vertex..
  stitchedng <- masterng
  for (idx in 1:nrow(rawel)) {
    vertex_ids <- match(rawel[idx, ], nodenames)
    if (is.null(thresh_el)){
      stitchedng <- igraph::add_edges(stitchedng, c(vertex_ids[[1]], vertex_ids[[2]]), 
                          "weight" = weight_attr[idx])
    }else{
      #Choose only those edges that are below a certain threshold..
      if(weight_attr[idx]<=thresh_el){
      stitchedng <- igraph::add_edges(stitchedng, c(vertex_ids[[1]], vertex_ids[[2]]), 
                                        "weight" = weight_attr[idx])
      }else{
        warning(paste0("Could not connect two vertices as edge length (in microns) ", 
                       round(weight_attr[idx],digits = 2), " is above threshold"))
      }
      
    }
    
  }
  
  #Step 10: Now keep only vertices that are connected to the master root and cross verify the same..
  cc_stitched=igraph::components(stitchedng)
  stitchedroot_id <- which(names(V(stitchedng)) == master_root)
  verticestoprune <- which(cc_stitched$membership != cc_stitched$membership[stitchedroot_id])
  sorted=order(cc_stitched$csize, decreasing = T)
  if(sorted[1] != cc_stitched$membership[stitchedroot_id]){
    warning("The root node doesn't belong to the largest fragment")
  }
  
  stitchedng=igraph::delete.vertices(stitchedng, verticestoprune)
  
  #Step 11: Set the root of the stitched graph now..
  stitchedneuron <- as.neuron(stitchedng, origin = master_root)
  if(stitchedneuron$nTrees!= 1){
    stop('The neuron being returned has multiple fragments')}
  stitchedneuron
}


#' Stitch two neurons together at their closest endpoint
#'
#' @param a,b Neurons to join together
#' @details This function joins two neurons at their nearest point (only one).
#'   Let's say you have two neurons a and b. a and b will be joined at one point that are closest to each other.
#'   However, when say there are multiple points at a and b which are closer and could be joined, 
#'   then do not use this function, use the function \code{\link{stitch_neurons_mst}}, 
#'   which is slower but will merge at multiple points.
#'   Note that for CATMAID neurons the neuron with the soma tag will be
#'   treated as the first (master neuron). Furthermore in this case the PointNo
#'   (aka node id) should already be unique. Otherwise it will be adjusted to
#'   ensure this.
#' @author Gregory Jefferis \email{jefferis@gmail.com} 
#' @export
#' @seealso \code{\link{stitch_neurons}}
#' @examples
#' \dontrun{
#' library(catmaid)
#' dl1=read.neuron.catmaid(catmaid_skids('annotation:DL1')[1])
#' dl1_main=simplify_neuron(dl1, n = 1, invert = F)
#' dl1_branches=simplify_neuron(dl1, n = 1, invert = T)
#' dl1_whole = stitch_neuron(dl1_main,dl1_branches)
#' 
#' }
stitch_neuron<-function(a, b){
  
  #Step1: if there are any repeats in PointNo, augment those in second neuron
  if(any(a$d$PointNo%in%b$d$PointNo)){
    b$d$PointNo=b$d$PointNo+max(a$d$PointNo)
    b$d$Parent=b$d$Parent+max(a$d$PointNo)
  }
  
  #Step2: Convert them to ngraph objects and make a single graph(that can have multiple subtrees)
  ag=as.ngraph(a, weights = T, method = 'seglist')
  bg=as.ngraph(b, weights = T, method = 'seglist')
  abg=as.ngraph(igraph::disjoint_union(ag, bg))
  
  
  #Step3: find closest node (or endpoint?) in each neuron and join those
  ce=closest_ends(a, b) #b neuron is the query neuron let's say has 3 pts and a neuron has 230 pts, then we 
  #end up with a 230x3 matrix and find out the closest point in b neuron 
  a_pointno=a$d$PointNo[ce$a_idx]
  b_pointno=b$d$PointNo[ce$b_idx]
  # older versions of nat use label for nodes, newer use name
  node_label=intersect(c("name","label"),
                       igraph::list.vertex.attributes(ag))[1]
  if(all(is.na(node_label))) stop("Graph nodes are not labelled!")
  abg=abg+igraph::edge(which(igraph::vertex_attr(abg, node_label)==a_pointno),
                       which(igraph::vertex_attr(abg, node_label)==b_pointno))
  
  #Step4: Convert them back to neuron..
  as.neuron(as.ngraph(abg))
}

# Find raw vertex ids and distance for closest end points of two neurons
closest_ends<-function(a, b){
  epa=endpoints(a)
  epb=endpoints(b)
  axyz=a$d[epa,c("X","Y","Z")]
  bxyz=b$d[epb,c("X","Y","Z")]
  nnres=nabor::knn(axyz, bxyz, k=1)
  b_idx=which.min(nnres$nn.dists)
  a_idx=nnres$nn.idx[b_idx,1]
  return(list(a_idx=epa[a_idx], b_idx=epb[b_idx], dist=min(nnres$nn.dists)))
}


#' Stitch multiple fragments into single neuron using nearest endpoints
#'
#' @details Neurons will be ordered by default such the largest (by node count)
#'   neuron with a soma tag is the \code{master} neuron - i.e. the one
#'   containing the root node. Fragments are joined recursively in this sort
#'   order each time simply picking the closest fragment to the current
#'   \emph{master}. Closest is here defined by the distance between nearest
#'   endpoints.
#' @param x A neuronlist containing fragments of a single neuron
#' @param prefer_soma When TRUE (the default) the fragment tagged as the soma
#'   will be used as the master neuron.
#' @param sort When TRUE (the default) the fragments will be sorted by the
#'   number of nodes they contain before stitching.
#' @param warndist If distance is greater than this value, create a warning.
#' @return A single \code{neuron} object containing all input fragments.
#' @importFrom igraph E set_edge_attr
#' @seealso \code{\link{stitch_neuron}}
#' @author Gregory Jefferis \email{jefferis@gmail.com} 
#' @export
#' @examples
#' \dontrun{
#' library(catmaid)
#' dl1=read.neuron.catmaid(catmaid_skids('annotation:DL1')[1])
#' dl1_main=simplify_neuron(dl1, n = 1, invert = F)
#' dl1_branches=simplify_neuron(dl1, n = 1, invert = T)
#' dl1_branches1=simplify_neuron(dl1_branches, n = 1, invert = F)
#' dl1_branches2=simplify_neuron(dl1_branches, n = 1, invert = T)
#' dl1_fragment <- list(dl1_main,dl1_branches1,dl1_branches2)
#' dl1_fragment <- as.neuronlist(dl1_fragment)
#' dl1_whole = stitch_neurons(dl1_fragment)
#' }
stitch_neurons <- function(x, prefer_soma=TRUE, sort=TRUE, warndist=1000) {
  #Step1: Check if it is neuronlist
  if(!is.neuronlist(x)) stop("x must be a neuronlist object!")
  if(length(x)<=1) return(x)
  
  #Step2a: Check if there is a soma tag associated (this will be considered as base or master neuron)
  if(prefer_soma) {
    svec=sapply(x, has_soma)
  } else {
    svec=rep(0,length(x))
  }
  
  #Step2b: If sort is enabled then you sort by number of nodes, so that the neuron with the largest number of nodes
  #becomes the base or master neuron
  if(sort){
    nnodes=sapply(x, function(n) nrow(n$d))
    eps=1/(max(nnodes)+1)
    svec=(eps+svec)*nnodes  #just normalising here such that the highest one gets 1 and others are weighted by it..
  }
  
  #Step3: Sort the neurons in the list so you can get a base or master neuron (the first element in the vector)
  if(any(svec>0))
    x=x[order(svec, decreasing = TRUE)]
  
  #Step4: if there are only two neurons, just merge them by their closest points by knn algoirthm
  if(length(x)==2) return(stitch_neuron(x[[1]], x[[2]]))
  
  #Step4a: if there are more than two neurons, just merge them by their closest points by knn algoirthm
  dists=sapply(x[-1], function(n) closest_ends(x[[1]], n)$dist) #find the closest fragment to the base fragment..
  mindist=min(dists)
  if(isTRUE(is.finite(warndist)) && mindist>warndist){
    warning("Suspicious minimum distance between fragments ( ",mindist, ")!")
  }
  chosen=which.min(dists)+1 #find the index of the closest to the base fragment..
  #Step4b: stitch the base fragment to the closest fragment..
  x[[1]]=stitch_neuron(x[[1]], x[[chosen]])
  #Step4c: iteratively do this process by removing the already merged fragment..
  stitch_neurons(x[-chosen], prefer_soma=FALSE, sort=FALSE)
}

has_soma<-function(x){
  !is.null(x$tags$soma)
}



#' Return indices of points in a neuron distal to a given node
#'
#' @description This function returns a list (containing the order of nodes) travelled using a depth
#' first search starting from the given node.
#' @param x A neuron
#' @param node.idx,node.pointno The id(s) of node(s) from which distal points
#'   will be selected. \code{node.idx} defines the integer index (counting from
#'   1) into the neuron's point array whereas \code{node.pointno} matches the
#'   PointNo column which will be the CATMAID id for a node.
#' @param root.idx,root.pointno The root node of the neuron for the purpose of
#'   selection. You will rarely need to change this from the default value. See
#'   \code{node} argument for the difference between \code{root.idx} and
#'   \code{root.pointno} forms.
#' @return Integer 1-based indices into the point array of points that are
#'   distal to the specified node(s) when traversing the neuron from the root to
#'   that node. Will be a vector if only one node is specified, otherwise a list is returned
#' @export
#' @seealso \code{\link[nat]{subset.neuron}}, \code{\link[nat]{prune}}
#' @examples
#' \dontrun{
#' ## Fetch a finished DL1 projection neuron
#' library(catmaid)
#' finished_pns=catmaid_get_neuronnames('annotation:^LH_DONE')
#' # should only be one neuron but pick first just in case
#' dl1skid=names(grep('DL1', finished_pns, value = TRUE))[1]
#' dl1=read.neuron.catmaid(dl1skid)
#'
#' ## subset to part of neuron distal to a tag "SCHLEGEL_LH"
#' # nb distal_to can accept either the PointNo vertex id or raw index as a
#' # pivot point
#' dl1.lh=subset(dl1, distal_to(dl1,node.pointno = dl1$tags$SCHLEGEL_LH))
#' plot(dl1,col='blue', WithNodes = FALSE)
#' plot(dl1.lh, col='red', WithNodes = FALSE, add=TRUE)
#' }
distal_to <- function(x, node.idx=NULL, node.pointno=NULL, root.idx=NULL,
                      root.pointno=NULL) {
  #Step1: Check the node.idx and node.pointno argument and map them to node.idx..
  if(is.null(node.idx)) {
    if(is.null(node.pointno))
      stop("At least one of node.idx or node.pointno must be supplied")
    node.idx=match(node.pointno, x$d$PointNo)
    if(any(is.na(node.idx)))
      stop("One or more invalid node.pointno. Should match entries in x$d$PointNo!")
  }
  #Step2: Convert the neuron to ngraph object and reroot it if root.idx or root.pointno is given..
  if(is.null(root.idx) && is.null(root.pointno)) {
    g=as.ngraph(x)
  } else {
    if(!is.null(root.pointno))
      root.idx=match(root.pointno, x$d$PointNo)
    if(length(root.idx)>1)
      stop("A single unique root point must be supplied")
    # we need to re-root the graph onto the supplied root
    gorig=as.ngraph(x)
    g=as.directed.usingroot(gorig, root = root.idx)
  }
  
  #Step3: For each node id, travese the graph from the given node using depth first search and return the visited
  #nodes..
  l=sapply(node.idx, dfs_traversal, g, simplify = FALSE)
  if(length(node.idx)==1) l[[1]] else l
}

dfs_traversal <- function(x, g) {
  gdfs=igraph::dfs(g, root = x, unreachable = FALSE)
  as.integer(gdfs$order)[!is.na(gdfs$order)]
}


#' @description \code{prune_twigs} will prune twigs less than a certain path length from a neuron
#' @export
#' @rdname prune_twigs
prune_twigs<-function(x, ...) UseMethod('prune_twigs')

#' Remove all twigs less than a certain path length from a neuron
#'
#' @param x A \code{\link{neuron}} or \code{\link{neuronlist}} object
#' @param twig_length Twigs shorter than this will be pruned
#' @param ... Additional arguments passed to \code{\link{nlapply}},
#'   \code{\link{prune_vertices}} and eventually \code{\link{as.ngraph}}.
#'
#' @author Gregory Jefferis \email{jefferis@gmail.com} 
#' @export
#' @rdname prune_twigs
#' @examples
#' # Prune twigs up to 5 microns long
#' pt5=prune_twigs(Cell07PNs[1:3], twig_length = 5)
#' # compare original (coloured) and pruned (black) neurons
#' plot(Cell07PNs[1:3], WithNodes=FALSE, lwd=2, xlim=c(240,300), ylim=c(120, 90))
#' plot(pt5, WithNodes=FALSE, add=TRUE, lwd=2, col='black')
prune_twigs.neuron <- function(x, twig_length, ...) {
  #Step1: Convert the neuron to ngraph object..
  ng = as.ngraph(x, weights = T)
  if (!igraph::is_dag(ng)) {
    stop("I can't prune neurons with cycles!")
  }
  root <- rootpoints(ng, original.ids=FALSE)
  if(length(root)!=1)
    stop("I can't prune neurons with more than 1 root!")
  
  #Step2: Make a table with rows(branch pts) and columns (leaves), the item entry would be the distance..
  #This will help us identify the leaves (end pts) that are farthest away from the branch pts and eliminate them..
 
  #Step2a: Compute the leaves and branchpoints..
  leaves=setdiff(endpoints(ng, original.ids=FALSE), root)
  bps=branchpoints(ng, original.ids=FALSE)
  #Step2b: Compute the table where rows(branch points) and columns (leaves)..
  dd=igraph::distances(ng, v=bps, to=leaves, mode = 'out')
  
  
  #Step3: For each column(leaf) set the bps that are farther than twig_length as -1, if all the bps are farther
  #then set the particular column as NA..Then each column find the index of the element(which is the bps) 
  #that is farthest..
  max_bp_idx <- apply(dd, 2, function(x) {
    # set values that are too long to signalling length of -1
    too_long=x>twig_length
    if(all(too_long)) return(NA_integer_)
    x[too_long]=-1
    wm=which.max(x)  #here choose the leaf that has the max length to prune
    if(is.finite(x[wm]) & x[wm]>0) wm else NA_integer_
  })
  
  #Step4: Now take only leaves that have finite values (which are acceptable eps that have less than the twig_length)
  # and also compute their corresponding starting pts(which are bps)
  eps_to_prune <- leaves[is.finite(max_bp_idx)]
  bps_to_start <- bps[max_bp_idx[is.finite(max_bp_idx)]]
  
  #Step5: Now we need a structure that will hold the bps and eps that we want to keep, the easiest approach 
  #would be to simply remove the eps we don't need by removing path to those eps, but the path may
  #actually pass through other bps that we want to keep, so we need to isolate those bps we want to keep
  #and those eps we want to keep
  #For e.g. we  also want to keep paths up until the branch points proximal to
  # the twigs that we are pruning - even if we prune all the twigs
  # downstream of those branch points
  
  #Step5a: Find the eps that we want to keep
  eps_to_keep <- setdiff(leaves, eps_to_prune)
  
  #Step5b: Also find the bps that we want to keep and collate them to nodes that we want to keep...
  nodes_to_keep=c(eps_to_keep, bps_to_start)
  
  #Step5c: Now compute the paths from the root to all the nodes that we want to keep..
  # note that mode = 'o' should be fine(as we have a directed graph from root), but mode 'all' is probably safer
  res2 <-
    igraph::shortest_paths(
      ng,
      from = root,
      to = nodes_to_keep,
      mode = 'all',
      weights = NA
    )
  
  #Step5d:Now compute the vertices you want to keep and prune the rest of the vertices
  verts_to_keep=unique(unlist(res2$vpath))
  # now we can basically prune everything not in that set
  prune_vertices(ng, verts_to_keep, invert=TRUE, ...)
}
