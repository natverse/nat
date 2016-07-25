#' Write object to VTK file
#' 
#' @param x Object to write
#' @param file  Path to output file
#' @param title Title of the .vtk file (defaults to file)
#' @param datatype The VTK data type (one of float or double)
#' @param WriteAllSubTrees Whether to write all subtrees in the neuron or just 
#'   the main tree.
#' @param ... Additional arguments to methods
#'   
#' @export
#' @examples 
#' \dontrun{
#' n=Cell07PNs[[1]]
#' write.vtk(n, paste0(n$NeuronName, ".vtk"))
#' write.neuron(n, paste0(n$NeuronName, ".vtk"))
#' }
write.vtk <-function(x, file, ...) UseMethod("write.vtk")

#' @export
#' @rdname write.vtk
write.vtk.neuron <-function(x, file, datatype=c("float","double"), title = file, WriteAllSubTrees=TRUE, ...){
  file.create(file)
  points=xyzmatrix(x)
  if(ncol(points)!=3) stop("Expect N rows x 3 cols of 3d points")
  datatype=match.arg(datatype)
  if(missing(title)) title=paste(file, "written from R by write.vtk at",Sys.time())
  
  # Make a seglist containing main or all segments
  SegList=as.seglist(x, all=WriteAllSubTrees, flatten=TRUE)
  # only use WriteAllSubTrees when there actually are multiple subtrees
  WriteAllSubTrees=WriteAllSubTrees && isTRUE(x$nTrees>1)
  usl=unlist(SegList)
  chosenVertices=sort(unique(usl))
  
  fc=file(file,open="at") # ie append, text mode
  on.exit(close(fc))
  cat("# vtk DataFile Version 2.0",
      title,
      "ASCII",
      "DATASET POLYDATA",
      paste("POINTS",length(chosenVertices),datatype),sep="\n",file=fc)
  
  write.table(points[chosenVertices,],col.names=F,row.names=F,file=fc)
  cat(paste("LINES", length(SegList), length(usl)+length(SegList)), "\n", sep="", file = fc)
  lapply(SegList,function(x) cat(length(x)," ", paste(x-1, collapse = " "), "\n", sep="", file=fc))
  invisible()
}
