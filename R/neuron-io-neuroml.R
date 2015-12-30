#' Return parsed XML or R list versions of a NeuroML file
#' 
#' @description \code{read.morphml} is designed to expose the full details of 
#'   the morphology information in a NeuroML file either as a parsed XML 
#'   structure processed by the \code{XML} package \emph{or} as an extensively 
#'   processed R list object. To obtain a \code{\link{neuron}} object use 
#'   \code{read.neuron.neuroml}.
#'   
#' @details NeuroML files consist of an XML tree containing one more or more 
#'   \bold{cells}. Each \bold{cell} contains a tree of \bold{segments} defining 
#'   the basic connectivity/position and an optional tree \bold{cables} defining
#'   attributes on groups of \bold{segments} (e.g. a name, whether they are 
#'   axon/dendrite/soma etc).
#'   
#'   \code{read.morphml} will either provide the parsed XML tree which you can 
#'   query using XPath statements or a more heavily processed version which 
#'   provides as much information as possible from the segments and cables trees
#'   in two R data.frames. The latter option will inevitably drop some 
#'   information, but will probably be more convenient for most purposes.
#' @param f Path to a file on disk or a remote URL (see 
#'   \code{\link[XML]{xmlParse}} for details).
#' @param ... Additional arguments passed to \code{\link[XML]{xmlParse}}
#' @param ReturnXML Whether to return a parsed XML tree (when 
#'   \code{ReturnXML=TRUE}) or a more extensively processed R list object when 
#'   \code{ReturnXML=FALSE}, the default.
#' @return Either an R list of S3 class containing one \code{morphml_cell} 
#'   object for every cell in the NeuroML document or an object of class 
#'   \code{XMLDocument} when \code{ReturnXML=TRUE}.
#' @export
#' @seealso \code{link[XML]{xmlParse}}, \code{\link{read.neuron.neuroml}}
#' @references \url{http://www.neuroml.org/specifications}
read.morphml<-function(f, ..., ReturnXML=FALSE){
  # basic parsing of xml doc (using libxml)
  doc=try(XML::xmlParse(f, ...))
  if(inherits(doc, 'try-error')) stop("Unable to parse file as neuroml")
  ns=XML::xmlNamespaceDefinitions(doc)
  defaultns=try(ns[[1]]$uri)
  if(inherits(defaultns, 'try-error'))
    stop("Unable to identify default neuroml namespace")
  
  # fetch cells 
  cells=XML::getNodeSet(doc,'//*/nml:cell', c(nml=defaultns))
  cell_names=XML::xmlSApply(cells, function(x) XML::xmlAttrs(x)['name'])
  names(cells)<-cell_names
  if(ReturnXML)
    cells
  else {
    lapply(cells, process_morphml_cell)
  }
}

# process the xml tree for a single (morphml) format cell
process_morphml_cell<-function(cell, ...) {
  if("morphology" %in% names(cell)) {
    stop("NeuroML2 is not yet supported!")
  }
  if(!"segments" %in% names(cell))
    stop("Cells must contain segment information")
  
  ## process morphml segments
  # first segment attributes
  segs=XML::xmlChildren(cell[['segments']])
  # only keep children called "segment"
  segs=segs[names(segs)=='segment']
  seginfo=t(sapply(segs, function(x) {
      atts=XML::xmlAttrs(x)
      if(length(atts)==3) atts['parent']=-1
      atts[c('id','cable','parent','name')]
    } ))
  rownames(seginfo)=as.vector(seginfo[,'id'])
  int_cols=c("id","cable","parent")
  seginfo_int=seginfo[,int_cols]
  mode(seginfo_int)='integer'
  
  segdf=as.data.frame(seginfo_int)
  other_cols=setdiff(colnames(seginfo), int_cols)
  segdf[,other_cols]=seginfo[,other_cols]
  
  # extract proximal and distal info for each segment
  # and convert to data.frame
  proxinfo=t(sapply(segs, function(x) {
    proxi=XML::xmlChildren(x)[['proximal']]
    if(is.null(proxi)) rep(NA_real_, 4)
    else as.numeric(XML::xmlAttrs(proxi))}))
  colnames(proxinfo)=paste0(c("x", "y", "z", "diameter"),".p")
  
  distinfo=t(sapply(segs, function(x) {
    distal=XML::xmlChildren(x)[['distal']]
    if(is.null(distal)) rep(NA_real_, 4)
    else as.numeric(XML::xmlAttrs(distal))}))
  colnames(distinfo)=paste0(c("x", "y", "z", "diameter"),".d")
  
  segdf[,colnames(proxinfo)]=proxinfo
  segdf[,colnames(distinfo)]=distinfo
  
  if("cables" %in% names(cell)) {
    
    ## extract cable info
    cables=XML::xmlChildren(cell[['cables']])
    # only keep children called "cable"
    cables=cables[names(cables)=='cable']
    
    cableinfo=t(sapply(cables, function(x) {
      atts=XML::xmlAttrs(x)
      rval=c(id=NA_integer_, name=NA_character_, fract_along_parent=NA_real_)
      rval['id']=atts['id']
      rval['name']=atts['name']
      if('fract_along_parent'%in%names(atts)){
        rval['fract_along_parent']=as.numeric(atts['fract_along_parent'])
      }
      rval
    }))
    rownames(cableinfo)=as.vector(cableinfo[,'id'])
    special_cols=c("id","fract_along_parent")
    
    cabledf=data.frame(id=as.integer(cableinfo[,'id']), 
                       fract_along_parent=as.numeric(cableinfo[,'fract_along_parent']))
    cabledf=cbind(cabledf,cableinfo[,!colnames(cableinfo)%in%special_cols, drop=FALSE])
    # cable type
    cabledf$type=sapply(cables, function(x) XML::xmlValue(x)[1])
  } else {
    cabledf=NULL
  }
  
  # return a list of class morphml_cell with segment/cable information
  structure(list(segments=segdf, cables=cabledf), class='morphml_cell')
}

#' @importFrom stats reshape
as.data.frame.morphml_cell<-function(x, ...){

  # convert neuroml to swc
  # 1. do we have cable info?
  # no: 
  # set PointNo to 2x (segment id + 1)
  # if proximal -1
  # if distal +0
  # 
  # now interleave all points 
  # 
  # yes:
  #   2. is fract_along_parent ever anything other than NA or 1?
  #   no: as above
  #   yes: we may need to insert segments to model the connection part way along
  # parent segment or connect to the proximal rather than the distal segment. 
  # For the time being just use distal point of parent segment
  
  if(!is.null(x$cables) && !is.null(x$cables$fract_along_parent)){
    # fract_along_parent exists, let's check for bad values
    fap=x$cables$fract_along_parent
    if(any(fap!=1, na.rm = TRUE)) {
      ndodgy=sum(fap!=1, na.rm = TRUE)
      warning(ndodgy," cable(s) connect at somewhere other than the end of their parent segment!\n",
              "Presently these will be connected to the distal point of the parent segment.\n",
              "In future it might make sense to introduce an appropriately located 3D point, ",
              "splitting the parent segment in two.")
    }
  }
  
  s=x$segments
  
  if(any(s$parent==-1 & is.na(s$x.p))) stop("Invalid morphml: root segments must have proximal and distal points")
  # note that we set proximal pointno to NA if missing
  prox_nas=ifelse(is.na(s$x.p), NA_integer_, 1L)
  s$PointNo.p=(2 * s$id + 1) * prox_nas
  s$PointNo.d= 2 * s$id + 2
  # parent point of proximal point is distal point of parent seg 
  s$parent.p=(2 * s$parent + 2) * prox_nas
  # parent point of distal point is either
  # a) distal point of parent seg when no proximal point
  # OR proximal point of this segment
  distal_points_parent_seg=2 * s$parent + 2
  
  s$parent.d=ifelse(is.na(s$PointNo.p), distal_points_parent_seg, s$PointNo.p)
  
  # Now fix any root nodes, which will have parent.p=0
  s$parent.p[s$parent.p==0]=-1
  
  # now reshape from wide to long
  r=reshape(s,direction='long', varying=names(s)[-(1:4)])
  # ... interleave proximal, distal points for each seg
  r=r[order(r$id),]
  # ... and drop (proximal) NA points that did not actually exist in input
  r=r[!is.na(r$PointNo),]
  
  # renumber PointNo so they are sequential
  r$NewPointNo=seq.int(length.out = nrow(r))
  r$NewParent=match(r$parent, r$PointNo, nomatch = -1L)
  
  
  if(!is.null(x$cables$type)) {
    # see if we can extract information for the SWC Label column
    # We use the following (according to neuromorpho.org)
    # 0 - undefined
    # 1 - soma
    # 2 - axon
    # 3 - (basal) dendrite
    cable_swc_labels=match(x$cables$type,
                           c("soma_group","axon_group","dendrite_group"),
                           nomatch = 0L)
    r$Label=cable_swc_labels[match(r$cable, x$cables$id)]
    in_names=c("NewPointNo", "Label", 'x','y','z','diameter','NewParent')
    out_names=c("PointNo", "Label", "X", "Y", "Z", "W", "Parent")
  } else {
    in_names=c("NewPointNo",'x','y','z','diameter','NewParent')
    out_names=c("PointNo", "X", "Y", "Z", "W", "Parent")
  }
  structure(r[in_names], .Names=out_names)
}

as.neuron.morphml_cell<-function(x, ...) as.neuron(as.data.frame(x, ...))

as.ngraph.morphml_cell<-function(x, ...) {
  as.ngraph(as.data.frame(x), ...)
}

#' Read one or more neurons from a NeuroML v1 file
#' 
#' @param f Path to a NeuroML format XML file
#' @param ... Additional arguments passed to read.morphml (and on to 
#'   \code{\link[XML]{xmlParse}})
#' @param AlwaysReturnNeuronList See \bold{Value} section (default \code{FALSE})
#' @return When the XML file contains only 1 cell \emph{and} 
#'   \code{AlwaysReturnNeuronList=FALSE}, a \code{\link{neuron}} object, 
#'   otherwise a \code{\link{neuronlist}} containing one or more neurons.
#' @references \url{http://www.neuroml.org/specifications}
#' @export
#' @seealso \code{\link{read.morphml}}
read.neuron.neuroml<-function(f, ..., AlwaysReturnNeuronList=FALSE) {
  cells=read.morphml(f, ...)
  if(AlwaysReturnNeuronList || length(cells)>1) {
    nlapply(cells, as.neuron)
  } else {
    as.neuron(cells[[1]])
  }
}

# Function to check if a file (or raw bytes) starts with a magic value
# if magic is a character vector of length >1 or a list of raw values
# it will return true if any magic value matches the file header
generic_magic_check<-function(f, magic) {
  if(is.character(magic)) {
    magic=lapply(magic, charToRaw)
  } else if(is.raw(magic)){
    magic=list(magic)
  }
  if(is.character(f) && length(f)>1)
    return(sapply(f,generic_magic_check, magic))
  
  nbytes=max(sapply(magic, length))
  
  firstnbytes=try({
    if(is.character(f)) {
      f=gzfile(f, open='rb')
      on.exit(close(f))
      readBin(f, what=raw(), n=nbytes)
    } else {
      f[seq.int(length.out = nbytes)]
    }
  },silent=TRUE)
  
  if(inherits(firstnbytes,'try-error') || length(firstnbytes)<nbytes)
    return(FALSE)
  
  for(m in magic) {
    if(all(firstnbytes[1:length(m)]==m)) return(TRUE)
  }
  FALSE
}

#' Check whether a file is in NeuroML format
#' 
#' This will check a file on disk to see if it is in NeuroML format. Some 
#' prechecks (optionally taking place on a supplied raw vector of bytes) should 
#' weed out nearly all true negatives and identify many true positives without
#' having to read/parse the file header.
#' 
#' @param f path to a file on disk
#' @param bytes optional raw vector of bytes used for prechecks
is.neuroml<-function(f, bytes=NULL){
  if(!is.null(bytes) && length(f)>1)
    stop("can only supply raw bytes to check for single file")
  if(length(f)>1) return(sapply(f,is.neuroml))
  
  bytes=if(is.null(bytes)) readBin(f, what=raw(), n=8L) else bytes
  if(!generic_magic_check(bytes, charToRaw("<"))) return(FALSE)
  if(generic_magic_check(bytes, "<neuroml")) return(TRUE)
  if(generic_magic_check(bytes, "<morphml")) return(TRUE)
  # if we got this far, then we can check if this has an XML header
  if(!generic_magic_check(f, "<?xml")) return(FALSE)
  # still not sure? Now we need to start reading in some lines
  h=readLines(f, n = 4)
  isTRUE(any(grepl("<neuroml",h,useBytes=T)) || any(grepl("<morphml",h,useBytes=T)))
}
