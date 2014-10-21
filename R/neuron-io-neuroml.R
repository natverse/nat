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
  if(!all(c("segments",'cables') %in% names(cell))){
    stop("Each cell must contain segments and cables elements")
  }
  
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

  ## extract cable info
  cables=XML::xmlChildren(cell[['cables']])
  # only keep children called "cable"
  cables=cables[names(cables)=='cable']
  
  cableinfo=t(sapply(cables, function(x) {
    atts=XML::xmlAttrs(x)
    rval=c(id=NA_integer_, name=NA_character_, fract_along_parent=NA_real_)
    rval['id']=atts['id']
    rval['name']=atts['name']
    if('fract_along_parent'%in%atts['fract_along_parent']){
      rval['fract_along_parent']=as.numeric(atts['fract_along_parent'])
    }
    rval
  }))
  rownames(cableinfo)=as.vector(cableinfo[,'id'])
  special_cols=c("id","fract_along_parent")
  
  cabledf=data.frame(id=as.integer(cableinfo[,'id']), 
                     fract_along_parent=as.numeric(cableinfo[,'fract_along_parent']))
  cabledf=cbind(cabledf,cableinfo[,!colnames(cableinfo)%in%special_cols])
  # cable type
  cabledf$type=sapply(cables, function(x) XML::xmlValue(x)[1])
  cabledf
  
  # return a list of class morphml_cell with segment/cable information
  structure(list(segments=segdf, cables=cabledf), class='morphml_cell')
}

as.data.frame.morphml_cell<-function(x, ...){
  in_names=c("id",'x.d','y.d','z.d','diameter.d','parent')
  out_names=c("PointNo", "X", "Y", "Z", "W", "Parent")
  structure(x[['segments']][in_names], .Names=out_names)
}

as.neuron.morphml_cell<-function(x, ...) as.neuron(as.data.frame(x, ...))

as.ngraph.morphml_cell<-function(x, ...) {
  as.ngraph(as.data.frame(x), ...)
}

read.neuron.neuroml<-function(f, ...) {
  cells=read.neuroml(f)
  celli=lapply(cells, neuroml_process_cell)
  if(length(celli)>1) {
    nlapply(celli, as.neuron)
  } else {
    as.neuron(celli[[1]])
  }
}
