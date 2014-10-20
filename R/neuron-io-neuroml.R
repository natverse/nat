read.neuron.neuroml<-function(f, ...){
  # basic parsing of xml doc (using libxml)
  doc=try(XML::xmlParse(f))
  if(inherits(doc, 'try-error')) stop("Unable to parse file as neuroml")
  ns=xmlNamespaceDefinitions(doc)
  defaultns=try(ns[[1]]$uri)
  if(inherits(if(inherits(defaultns, 'try-error'))
    stop("Unable to identify default neuroml namespace")
  
  # fetch cells 
  cells=getNodeSet(doc,'//*/nml:cell', c(nml=defaultns)
  cell_names=XML::xmlSApply(cells, function(x) XML::xmlAttrs(x)['name'])
  as.vector(cell_names)
}
