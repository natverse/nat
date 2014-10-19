read.neuron.neuroml<-function(f, ...){
  doc=try(XML::xmlParse(f))
  if(inherits(doc, 'try-error')) stop("Unable to parse file as neuroml")
  cells=getNodeSet(doc,'//*/nml:cell', c(nml="http://morphml.org/neuroml/schema"))
  cell_names=XML::xmlSApply(cells, function(x) XML::xmlAttrs(x)['name'])
  as.vector(cell_names)
}
