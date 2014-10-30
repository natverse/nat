read.morphml<-function(f, ...){
  # basic parsing of xml doc (using libxml)
  doc=try(XML::xmlParse(f))
  if(inherits(doc, 'try-error')) stop("Unable to parse file as neuroml")
  ns=XML::xmlNamespaceDefinitions(doc)
  defaultns=try(ns[[1]]$uri)
  if(inherits(defaultns, 'try-error'))
    stop("Unable to identify default neuroml namespace")
  
  # fetch cells 
  cells=XML::getNodeSet(doc,'//*/nml:cell', c(nml=defaultns))
  cell_names=XML::xmlSApply(cells, function(x) XML::xmlAttrs(x)['name'])
  names(cells)<-cell_names
  invisible(cells)
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
  #   2. is fract_along_parent ever anything other than NA 0 1?
  #   no: as above
  #   yes: we may need to insert segments to model the connection part way along
  # parent segment. For the time being just use distal point of parent segment
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
  
  in_names=c("NewPointNo",'x','y','z','diameter','NewParent')
  out_names=c("PointNo", "X", "Y", "Z", "W", "Parent")
  structure(r[in_names], .Names=out_names)
}

as.neuron.morphml_cell<-function(x, ...) as.neuron(as.data.frame(x, ...))

as.ngraph.morphml_cell<-function(x, ...) {
  as.ngraph(as.data.frame(x), ...)
}

read.neuron.neuroml<-function(f, ...) {
  cells=read.morphml(f)
  celli=lapply(cells, process_morphml_cell)
  if(length(celli)>1) {
    nlapply(celli, as.neuron)
  } else {
    as.neuron(celli[[1]])
  }
}
