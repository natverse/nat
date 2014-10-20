read.neuroml<-function(f, ...){
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
