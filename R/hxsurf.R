#' Read Amira surface (aka HxSurface or HyperSurface) files into hxsurf object
#' 
#' @param filename Character vector defining path to file
#' @param RegionNames Character vector specifying which regions should be read 
#'   from file. Default value of \code{NULL} => all regions.
#' @param RegionChoice Whether the \emph{Inner} or \emph{Outer} material should 
#'   define the material of the patch.
#' @param FallbackRegionCol Colour to set regions when no colour is defined
#' @param Verbose Print status messages during parsing when \code{TRUE}
#' @return A list with S3 class hxsurf with elements \itemize{
#'   
#'   \item{Vertices}{ A data.frame with columns \code{X, Y, Z, PointNo}}
#'   
#'   \item{Regions}{ A list with 3 column data.frames specifying triplets of 
#'   vertices for each region (with reference to \code{PointNo} column in
#'   \code{Vertices} element)}
#'   
#'   \item{RegionList}{ Character vector of region names (should match names of
#'   \code{RegionList} element)}
#'   
#'   \item{RegionColourList}{ Character vector specifying default colour to plot
#'   each region in R's \code{\link{rgb}} format}
#'   
#'   }
#' @export
#' @seealso \code{\link{plot3d.hxsurf}, \link{rgb}}
#' @aliases hxsurf
#' @family amira
read.hxsurf<-function(filename,RegionNames=NULL,RegionChoice="Inner",
                      FallbackRegionCol="grey",Verbose=FALSE){
  # Check for header confirming file type
  firstLine=readLines(filename,n=1)
  if(!any(grep("#\\s+hypersurface\\s+[0-9.]+\\s+ascii",firstLine,ignore.case=T,perl=T))){
    warning(paste(filename,"does not appear to be an Amira HyperSurface ASCII file"))
    return(NULL)
  }
  
  t=readLines(filename)
  nLines=length(t)
  if(Verbose) cat(nLines,"lines of text to parse\n")
  
  # Find the start of the Vertices
  dataStart=grep("^\\s*Vertices\\s*",t)[1]
  if(Verbose) cat("Data start line =",dataStart,"\n")
  headerLines=t[seq(dataStart-1)]
  trim=function(x) sub('^\\s+', '', sub('\\s+$', '', x, perl = TRUE), perl = TRUE)
  getfield=function(fName,textLines=headerLines,pos=2) unlist(strsplit(trim(textLines[grep(fName,textLines)]),"\\s+",perl=TRUE))[pos]
  nVertices=as.numeric(getfield("Vertices",t[dataStart],2))
  if(Verbose) cat("nVertices =",nVertices,"\n")
  
  d=list()
  d$Vertices=read.table(filename,skip=dataStart,nrows=nVertices,col.names=c("X","Y","Z"),colClasses=rep("numeric",3))
  d$Regions <- list()
  
  d$Vertices$PointNo=seq(nrow(d$Vertices))
  if(Verbose) cat("Finished processing Vertices\n")
  
  # Now read in Triangles that define patches:
  linesSkipped=dataStart+nVertices-1
  remainingLines=t[(dataStart+nVertices):nLines]
  PatchDefLine=grep("^\\s*Patches\\s*",remainingLines,perl=TRUE)
  if(Verbose) cat("PatchDefLine =",PatchDefLine,"\n")
  nPatches=as.numeric(getfield("Patches",remainingLines[PatchDefLine],2))
  if(Verbose) cat("nPatches =",nPatches,"\n")
  PatchStarts=grep("^\\s*{",remainingLines[PatchDefLine:length(remainingLines)],perl=TRUE)+PatchDefLine-1
  if(length(PatchStarts)>nPatches) PatchStarts=PatchStarts[1:nPatches]
  PatchEnds=grep("^\\s*}",remainingLines[PatchDefLine:length(remainingLines)],perl=TRUE)+PatchDefLine-1
  if(length(PatchEnds)>nPatches) PatchEnds=PatchEnds[1:nPatches]
  TriangleDeflines<-grep("Triangles",remainingLines)
  
  for(i in 1:nPatches){
    if(!any(TriangleDeflines[i])){
      warning(paste("Unable to find Triangle number in patch",i,"in",filename,"\n"))
      return (NULL)
    }
    if(Verbose) cat("TriangleDefline =",TriangleDeflines[i],"\n")
    PatchHeader<-remainingLines[PatchStarts[i]:TriangleDeflines[i]]
    if(Verbose) cat("PatchHeader is",length(PatchHeader),"lines long\n")
    # note use of RegionChoice to switch naming between inner and outer
    for(RegChoice in RegionChoice) {
      RegionName=getfield(paste(RegChoice,"Region",sep=""),PatchHeader,2)
      nTriangles=as.numeric(getfield("Triangles",PatchHeader,2))
      if(nTriangles<0 || nTriangles>100000){return(-1)}
      if(Verbose) cat("nTriangles =",nTriangles,"for patch =",i,"\n")
      # Check if we want to load in this region
      if( is.null(RegionNames) || RegionName%in%RegionNames ){
        # Ensure we do not try to add no triangles, or the exterior region
        if(nTriangles == 0 || RegionName == "Exterior") next
        start_of_patch=linesSkipped+TriangleDeflines[i]+1
        thispatch=read.table(filename,skip=linesSkipped+TriangleDeflines[i],nrows=nTriangles,
                             quote='',colClasses='integer',blank.lines.skip=FALSE,
                             fill=FALSE,comment.char="",
                             col.names=c("V1","V2","V3"))
        # scan no quicker in these circs, problem is repeated file access
        # specifying text directly also does not help dues to very slow textConnection
        # thispatch=matrix(scan(text=t[start_of_patch:(start_of_patch+nTriangles-1)],nlines=nTriangles),ncol=3,byrow=T)
        # check if we have already loaded a patch in this name
        if(RegionName%in%names(d$Regions)){
          # add to the old patch
          if(Verbose) cat("Adding to patch name",RegionName,"\n")
          d[['Regions']][[RegionName]]=rbind(d[['Regions']][[RegionName]],thispatch)
        } else {
          # new patch
          if(Verbose) cat("Making new patch name",RegionName,"\n")
          d[['Regions']][[RegionName]]=thispatch
        }
      }
    }
  }
  d$RegionList=names(d$Regions)
  
  # Handle colours for regions
  d$RegionColourList <- vector(length=length(d$RegionList))
  closeBraces <- grep("}", headerLines)
  for(regionName in d$RegionList) {
    # Find section in headerLines corresponding to this region
    headerSecStart <- grep(paste0(" ", regionName, " \\{"), headerLines)[1]
    headerSecEnd <- closeBraces[closeBraces > headerSecStart][1]
    # Extract colour information
    colorLine <- grep("Color", headerLines[headerSecStart:headerSecEnd], value=T)
    if(length(colorLine) > 0) {
      rgbValues <- strsplit(regmatches(colorLine, gregexpr("[0-9]$|[0-9][^\\.]|[0-9]\\.[0-9]+", colorLine, perl=T))[[1]], " ")
      color <- rgb(rgbValues[[1]], rgbValues[[2]], rgbValues[[3]])
    } else {
      color <- FallbackRegionCol
    } 
    d$RegionColourList[which(d$RegionList == regionName)] <- color
  }
  class(d) <- c('hxsurf',class(d))
  return(d)
}

#' Write Amira surface (aka HxSurface or HyperSurface) into .surf file.
#' 
#' @param surf hxsurf object to write to file.
#' @param filename character vector defining path to file.
#' @return \code{NULL} or integer status from \code{\link{close}}.
#' @export
#' @seealso \code{\link{plot3d.hxsurf}},\code{\link{read.hxsurf}}, \code{\link{rgb}}
#' @family amira
write.hxsurf <- function(surf, filename) {
  fc <- file(filename, open="at")
  cat("# HyperSurface 0.1 ASCII\n\n", file=fc)
  
  cat("Parameters {\n", file=fc)
  cat("    Materials {\n", file=fc)
  cat("        Exterior {\n            Id 1\n        }\n", file=fc)
  regionData <- cbind(surf$RegionList, surf$RegionColourList)
  for (i in 1:nrow(regionData)) {
    cat("        ", regionData[i, 1], " {\n", sep="", file=fc)
    cat("            Id ", i+1, ",\n", sep="", file=fc)
    cat("            Color ", paste(zapsmall(col2rgb(regionData[i, 2])/255), collapse=" "), "\n", sep="", file=fc)
    cat("        }\n", file=fc)
  }
  cat("    }\n", file=fc)
  cat("    BoundaryIds {\n        Name \"BoundaryConditions\"\n    }\n", file=fc)
  cat("}\n\n", file=fc)
  
  cat("Vertices ", nrow(surf$Vertices), "\n", sep="", file=fc)
  apply(surf$Vertices[, 1:3], 1, function(x) cat("    ", sprintf(x[1], fmt="%.6f"), " ", sprintf(x[2], fmt="%.6f"), " ", sprintf(x[3], fmt="%.6f"), "\n", sep="", file=fc))
  
  cat("NBranchingPoints 0\nNVerticesOnCurves 0\nBoundaryCurves 0\n", file=fc)
  cat("Patches ", length(surf$Regions), "\n", sep="", file=fc)
  
  for(i in 1:length(surf$Regions)) {
    region <- surf$Regions[[i]]
    cat("{\n", file=fc)
    cat("InnerRegion ", names(surf$Regions[i]), "\n", sep="", file=fc)
    cat("OuterRegion Exterior\n", file=fc)
    cat("BoundaryId 0\n", file=fc)
    cat("BranchingPoints 0\n\n", file=fc)
    cat("Triangles ", nrow(region), "\n", sep="", file=fc)
    apply(region, 1, function(x) cat("    ", paste(x, collapse=" "), "\n", sep="", file=fc))
    cat("}\n", file=fc)
  }
  close(fc)
}

#' Plot amira surface objects in 3d using rgl
#' 
#' @param x An hxsurf surface object
#' @param materials Character vector naming materials to plot (defaults to all 
#'   materials in x)
#' @param col Character vector specifying colors for the materials, or a 
#'   function that will be called with the number of materials to plot. When
#'   \code{NULL} (default) will use meterial colours defined in Amira (if
#'   available), or \code{rainbow} otherwise.
#' @param ... Additional arguments passed to 
#' @export
#' @method plot3d hxsurf
#' @importFrom rgl plot3d par3d triangles3d
#' @seealso \code{\link{read.hxsurf}}
plot3d.hxsurf<-function(x, materials=x$RegionList, col=NULL, ...){
  # skip so that the scene is updated only once per hxsurf object
  skip <- par3d(skipRedraw = TRUE)
  on.exit(par3d(skip))
  
  if(is.null(col)) {
    if(length(x$RegionColourList)){
      col=x$RegionColourList[match(materials,x$RegionList)]
    } else col=rainbow
  }
  if(is.function(col)) col=col(length(materials))
  if(is.factor(col)) col=rainbow(nlevels(col))[as.integer(col)]
  if(length(col)==1 && length(materials)>1) col=rep(col,length(materials))
  names(col)=materials
  rlist=list()
  for(mat in materials){
    # get order triangle vertices
    tri=as.integer(t(x$Regions[[mat]]))
    rlist[[mat]]=triangles3d(x[['Vertices']]$X[tri],x[['Vertices']]$Y[tri],
                             x[['Vertices']]$Z[tri],col=col[mat],...)
  }
  invisible(rlist)
}
