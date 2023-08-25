read.fijixml<-function(f, components=c("path", "fill"), ..., Verbose=FALSE){
  if(!file.exists(f)) 
    stop("File: ", f, "does not exist!")
  doc=try(XML::xmlParse(f, ...))
  if(inherits(doc, 'try-error')) stop("Unable to parse file as Fiji XML")

  r<-XML::xmlRoot(doc)
  if(XML::xmlName(r)!="tracings") stop("This is not a Longair format tracing")
  
  l<-list()
  stopifnot(names(r)[1:2]==c("samplespacing","imagesize"))	
  attr(l,"samplespacing")<-XML::xmlAttrs(r['samplespacing'][[1]])
  attr(l,"imagesize")<-XML::xmlAttrs(r['imagesize'][[1]])
  
  tracings=r[-c(1:2)]
  if(length(tracings)==0) stop("No tracings in this file")
  if(Verbose) cat("There are",length(tracings),"tracings in this file\n")	
  
  fetch_attrs <- function(x, attrs) {
    res=XML::xmlSApply(tracings[[i]],function(x) as.numeric(XML::xmlAttrs(x)[attrs]))
    res=t(res)
    rownames(res)<-NULL
    # use names of attrs if present as colnames of output
    colnames(res) <- if(!is.null(names(attrs))) names(attrs) else attrs
    pathAttributes=XML::xmlAttrs(tracings[[i]])
    attr(res,'pathAttributes')=pathAttributes
    res
  }
  
  for(i in 1:length(tracings)){
    nti=names(tracings)[i]
    if(isFALSE(nti %in% components))
      next
    # we will store the result in the next empty slot in the list
    lidx=length(l)+1
    if(isTRUE(nti=='path')) {
      l[[lidx]]=fetch_attrs(tracings[[i]], c(X="xd",Y="yd",Z="zd"))
    } else if(isTRUE(nti=='fill')) {
      l[[lidx]]=fetch_attrs(tracings[[i]], c(id="id", X="x",Y="y",Z="z", d="distance"))
    } else {
      warning("Ignoring unrecognised traces file component: ", nti)
    }
    
    # set the list item name to the tracing id 
    # (a number, but not necessarily from a perfect 0 indexed sequence)
    names(l)[lidx]=attr(l[[lidx]],'pathAttributes')['id']
  }
  l
}

#' Read a neuron saved by Fiji's Simple Neurite Tracer Plugin
#'
#' @param f Path to a file
#' @param ... Additional arguments passed to \code{\link[XML]{xmlParse}}.
#' @param simplify Whether to return a single neuron as a \code{neuron} object
#'   rather than a \code{neuronlist} of length 1.
#' @param components Which components to read in (path or fill). Only paths are
#'   properly supported at present (see details).
#' @param Verbose Whether to print status messages during parsing.
#' @details simple neurite tracer .traces files are an XML based format so
#'   parsing it depends on installation of the suggested XML package.
#'
#'   They can contain both paths (skeleton lines) and fill information (saved as
#'   XYZ coordinates of voxels inside the object). The latter cannot currently
#'   be handled very well by \code{\link{read.neuron}}. If you wish to access
#'   them you will probably need to use the private \code{read.fijixml} function
#'   to do so (see examples).
#'
#'   Your best best if you want to produce a fully 3D object with "width"
#'   information would be to generate a 3D mesh using Fiji's 3D viewer. You can
#'   do this by selecting the object in the viewer and the choosing \code{File
#'   ... Export Surface ... Wavefront} \emph{while the 3D viewer window is
#'   active}. The resultant obj file can then be read in by
#'   \code{\link{read.neurons}}. You could use this mesh to find radius
#'   information for a skeleton by shooting rays from skeleton to mesh to
#'   estimate the radius.
#'
#' @references \url{https://imagej.net/plugins/snt/}
#'   \url{https://imagej.net/plugins/snt/extending}
#' @export
#'
#' @examples
#' \dontrun{
#' n=read.neuron.fiji("my.traces")
#' plot3d(n)
#' fill=read.neuron.fiji("my.traces", components='fill')
#' points3d(fill, col='grey')
#' }
read.neuron.fiji<-function(f, ..., simplify=TRUE, 
                           components=c("path", "fill"),
                           Verbose=FALSE){
  components=match.arg(components, several.ok = FALSE)
  
  l=read.fijixml(f, ..., components=components, Verbose=Verbose)
  if(components=='fill') {
    voxdims=as.numeric(attr(l, 'samplespacing')[1:3])
    for(i in seq_along(l)) {
      xyzmatrix(l[[i]])=scale(xyzmatrix(l[[i]]), scale = 1/voxdims, center = F)
    }
    return(l)
  }
  
  dflist=as.list(rep(NA,length(l)))
  MasterPath=seq(l)
  pointOffsets=rep(0,length(l))
  names(dflist)<-names(pointOffsets)<-names(MasterPath)<-names(l)	
  
  for(id in names(l)){
    d=l[[id]]
    df=seglist2swc(list(1:nrow(d)), d=xyzmatrix(d))
    pathAttributes=attr(l[[id]],"pathAttributes")
    if('swctype'%in%names(pathAttributes)) {
      df$Label=as.integer(pathAttributes['swctype'])
    }
    if('startson'%in%names(pathAttributes)){
      # this path is joined to another, so find out which
      parentPathId=pathAttributes['startson']
      # now find the Master Path of that path
      MasterPath[id]=MasterPath[parentPathId]
      
      # make a note of the number of points by which we have to offset
      # points for this path
      pointOffsets[id]=nrow(dflist[[MasterPath[id]]])
      
      # adjust all the point ids by the number of rows in the master data fram
      # in the parent path data frame
      df[,c("Parent","PointNo")]=df[,c("Parent","PointNo")]+pointOffsets[id]
      
      # now find the index of the point in the parent path to which this path is corrected
      # nb Mark's paths are 0 indexed (R is 1 indexed)
      parentStartIndex=as.numeric(pathAttributes['startsindex'])+1
      # ... and correct this (in case that path was not path 0)
      parentStartIndex=parentStartIndex+pointOffsets[parentPathId]
      # now set the parent of this new path to the point on the parent path
      df$Parent[1]=parentStartIndex
      
      # finally add these data to the dataframe for the master path
      dflist[[MasterPath[id]]]=rbind(dflist[[MasterPath[id]]],df)
    } else {
      df$Parent[1]=-1 # if this is a new path set root's parent to -1
      dflist[[id]]=df
    }
  }
  
  # Actually make neurons from the dataframes of points
  neuronList=neuronlist()
  for(df in dflist){
    if(!is.data.frame(df)) next
    neuronList[[length(neuronList)+1]]=as.neuron(df, InputFileName=f)
  }
  names(neuronList)=paste(sub("\\.[^\\.]+$","",basename(f)),sep="-",seq(neuronList))
#   if(MergePaths) {
#     else MergeUnconnectedPathsToSingleNeuron(neuronList)
#   }
  if(simplify && length(neuronList)==1) return(neuronList[[1]])
  else neuronList
}

#' Check whether a file is in Fiji's simple neurite tracer format
#' 
#' This will check a file on disk to see if it is in Fiji's simple neurite
#' tracer XML format.
#' 
#' Some prechecks (optionally taking place on a supplied raw vector of bytes)
#' should weed out nearly all true negatives and identify many true positives
#' without having to read/parse the file header.
#' 
#' @param f path to a file on disk
#' @param bytes optional raw vector of bytes used for prechecks
#' @export
is.fijitraces<-function(f, bytes=NULL){
  if(!is.null(bytes) && length(f)>1)
    stop("can only supply raw bytes to check for single file")
  if(length(f)>1) return(sapply(f,is.fijitraces))
  
  if(!generic_magic_check(f, "<?xml")) return(FALSE)
  # still not sure? Now we need to start reading in some lines
  h=readLines(f, n = 3)
  isTRUE(any(grepl("<!DOCTYPE tracings",h, useBytes=T, fixed = T)))
}

# Read a file is in the Fiji landmarks format (XML)
# See https://imagej.net/plugins/name-landmarks-and-register
read.landmarks.fiji<-function(f, ...){
  if(!file.exists(f))
    stop("File: ", f, "does not exist!")
  
  if(!requireNamespace('XML', quietly = TRUE))
    stop("Please install the XML package in order to use read.landmarks.fiji!")

  doc=try(XML::xmlParse(f, ...))
  if(inherits(doc, 'try-error'))
    stop("Unable to parse XM landmarks file")

  r<-XML::xmlRoot(doc)
  if(XML::xmlName(r)!="namedpointset")
    stop("This is not a Fiji (Longair) format landmark file")
  
  points=XML::xmlSApply(r,function(x) as.numeric(XML::xmlAttrs(x)[c("x","y","z")]))
  pointnames=unname(XML::xmlSApply(r,function(x) XML::xmlAttrs(x)[c("name")]))
  matrix(points, ncol=3, byrow = T, dimnames = list(pointnames, c("X", "Y", "Z")))
}

write.landmarks.fiji <- function(x, file, ...) {
  header='<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE namedpointset [
  <!ELEMENT namedpointset (pointworld*)>
  <!ELEMENT pointworld EMPTY>
  <!ATTLIST namedpointset version CDATA #REQUIRED>
  <!ATTLIST pointworld set (true|false) #REQUIRED>
  <!ATTLIST pointworld name CDATA #REQUIRED>
  <!ATTLIST pointworld x CDATA #IMPLIED>
  <!ATTLIST pointworld y CDATA #IMPLIED>
  <!ATTLIST pointworld z CDATA #IMPLIED>
  ]>
  '
  
  point_start='<namedpointset version="1.0">'
  point_end='</namedpointset>'
  points=sprintf('<pointworld set="true" name="%s" x="%f" y="%f" z="%f"/>', rownames(x), x[,1], x[,2], x[,3])
  cat(header, file=file, sep="\n", ...)
  cat(point_start, file=file, sep="\n", append = TRUE)
  cat(points, file=file, sep="\n", append = TRUE)
  cat(point_end, file=file, sep="\n", append = TRUE)
}

# Test if a file is in the Fiji landmarks format
is.fijilandmarks<-function(f, bytes=NULL){
  if(!is.null(bytes) && length(f)>1)
    stop("can only supply raw bytes to check for single file")
  if(length(f)>1) return(sapply(f, is.fijilandmarks))
  
  if(!generic_magic_check(f, "<?xml")) return(FALSE)
  # still not sure? Now we need to start reading in some lines
  h=readLines(f, n = 3)
  isTRUE(any(grepl("<!DOCTYPE namedpointset",h, useBytes=T, fixed = T)))
}
