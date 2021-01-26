#' Read Amira surface (aka HxSurface or HyperSurface) files into hxsurf object
#'
#' @details Note that when \code{RegionChoice="both"} or
#'   \code{RegionChoice=c("Inner", "Outer")} both polygons in inner and outer
#'   regions will be added to named regions. To understand the significance of
#'   this, consider two adjacent regions, A and B, with a shared surface. For
#'   the polygons in both A and B, Amira will have a patch with (say)
#'   InnerRegion A and OuterRegion B. This avoids duplication in the file.
#'   However, it might be convenient to add these polygons to both regions when
#'   we read them into R, so that regions A and B in our R object are both
#'   closed surfaces. To achieve this when \code{RegionChoice="both"},
#'   \code{read.hxsurf} adds these polygons to region B (as well as region A)
#'   but swaps the order of the vertices defining the polygon to ensure that the
#'   surface directionality is correct.
#'
#'   As a rule of thumb, stick with \code{RegionChoice="both"}. If you get more
#'   regions than you wanted, then try switching to \code{RegionChoice="Inner"}
#'   or \code{RegionChoice="Outer"}.
#'
#'   Note that the support for reading Amira's binary mesh format (HxSurface
#'   binary) is less mature and in particular only a few multi region mesh files
#'   have been tested. Finally there is no support to read meshes from the newer
#'   "Amira Binary Surface format" although such files can be read into a list
#'   using the \code{read.amiramesh} function.
#'
#' @param filename Character vector defining path to file
#' @param RegionNames Character vector specifying which regions should be read
#'   from file. Default value of \code{NULL} => all regions.
#' @param RegionChoice Whether the \emph{Inner} or \emph{Outer} material, or
#'   \emph{both} (default), should define the material of the patch. See
#'   details.
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
#'   \code{Regions} element)}
#'
#'   \item{RegionColourList}{ Character vector specifying default colour to plot
#'   each region in R's \code{\link{rgb}} format}
#'
#'   }
#' @export
#' @seealso \code{\link{plot3d.hxsurf}, \link{rgb}}
#' @aliases hxsurf
#' @family amira
#' @family hxsurf
#' @examples
#' \dontrun{
#' read.hxsurf("my.surf", RegionChoice="both")
#' }
read.hxsurf<-function(filename,RegionNames=NULL,RegionChoice="both",
                      FallbackRegionCol="grey",Verbose=FALSE){
  # Check for header confirming file type
  firstLine=readLines(filename,n=1)
  if(!any(grep("#\\s+hypersurface\\s+[0-9.]+\\s+ascii",firstLine,ignore.case=T,perl=T))){
    if(!any(grep("#\\s+hypersurface\\s+[0-9.]+\\s+binary",firstLine,ignore.case=T,perl=T))){
      stop(filename," does not appear to be an Amira HyperSurface file!")
    }
    res = tryCatch(
      read.hxsurf.bin(
        filename = filename,
        FallbackRegionCol = FallbackRegionCol,
        Verbose = Verbose
      ),
      error = function(e)
        stop(
          "Support for reading binary Amira HyperSurface is still limited.\n",
          "See https://github.com/natverse/nat/issues/429. Detailed error message",
          as.character(e)
        )
    )
    return(res)
  }
  initialcaps<-function(x) {substr(x,1,1)=toupper(substr(x,1,1)); x}
  RegionChoice=match.arg(initialcaps(RegionChoice), c("Inner", "Outer", "Both"), 
                         several.ok = TRUE)
  if(RegionChoice[1]=="Both") RegionChoice=c("Inner", "Outer")
  t=readLines(filename)
  nLines=length(t)
  if(Verbose) cat(nLines,"lines of text to parse\n")
  
  # Find the start of the Vertices
  dataStart=grep("^\\s*Vertices\\s*",t)[1]
  if(Verbose) cat("Data start line =",dataStart,"\n")
  headerLines=t[seq(dataStart-1)]
  getfield=function(fName,textLines=headerLines,pos=2) unlist(strsplit(trimws(textLines[grep(fName,textLines)]),"\\s+",perl=TRUE))[pos]
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
  if(length(TriangleDeflines)!=nPatches)
    stop("Incorrect number of Triangle definition lines in",filename,"\n")
  
  for(i in 1:nPatches){
    if(Verbose) cat("TriangleDefline =",TriangleDeflines[i],"\n")
    PatchHeader<-remainingLines[PatchStarts[i]:TriangleDeflines[i]]
    # remove any opening braces - these would cause a problem if on same line
    PatchHeader=sub("^\\s*\\{\\s*","",PatchHeader)
    # convert all whitespace to single spaces
    PatchHeader=gsub("\\s+"," ",PatchHeader)
    if(Verbose) cat("PatchHeader is",length(PatchHeader),"lines long\n")
    # note use of RegionChoice to switch naming between inner and outer
    for(RegChoice in RegionChoice) {
      RegionName=getfield(paste(RegChoice,"Region",sep=""),PatchHeader,2)
      nTriangles=as.numeric(getfield("Triangles",PatchHeader,2))
      if(nTriangles<0 || nTriangles>100000) stop("Bad triangle number: ", nTriangles)
      if(Verbose) cat("nTriangles =",nTriangles,"for patch =",i,"\n")
      # Check if we want to load in this region
      if( is.null(RegionNames) || RegionName%in%RegionNames ){
        # Ensure we do not try to add no triangles, or the exterior region
        if(nTriangles == 0 || RegionName == "Exterior") next
        thispatch=read.table(filename,skip=linesSkipped+TriangleDeflines[i],nrows=nTriangles,
                             quote='',colClasses='integer',blank.lines.skip=FALSE,
                             fill=FALSE,comment.char="",
                             col.names=c("V1","V2","V3"))
        if(getfield(paste(RegChoice,"Region",sep=""),PatchHeader,1) == "OuterRegion") {
          thispatch <- thispatch[, c(1,3,2)]
          if(Verbose) message("Permuting vertices for ", RegionName, "...")
          colnames(thispatch) <- c("V1","V2","V3")
        }
        # scan no quicker in these circs, problem is repeated file access
        # specifying text directly also does not help dues to very slow textConnection

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
    headerSecStart <- grep(paste0("^\\s*", regionName, "(\\s+ \\{){0,1}"), headerLines)[1]
    headerSecEnd <- closeBraces[closeBraces > headerSecStart][1]
    # Extract colour information
    colorLine <- grep("Color", headerLines[headerSecStart:headerSecEnd], value=T)
    if(length(colorLine) > 0) {
      rgbValues <- strsplit(regmatches(colorLine, gregexpr("[0-9]$|[0-9][^\\.]|[0-9]\\.[0-9]+", colorLine, perl=T))[[1]], " ")
      # clean up any trailing commas
      rgbValues <- gsub("[,}]","", rgbValues)
      color <- rgb(rgbValues[[1]], rgbValues[[2]], rgbValues[[3]])
    } else {
      color <- FallbackRegionCol
    } 
    d$RegionColourList[which(d$RegionList == regionName)] <- color
  }
  class(d) <- c('hxsurf',class(d))
  return(d)
}

read.hxsurf.bin <- function(filename, return.raw=FALSE, FallbackRegionCol='grey', Verbose=FALSE) {
  con=file(filename, open='rb')
  on.exit(close(con))
  vertex_regex='^Vertices \\d+$'
  
  # read header
  h <- character()
  line <- readLines(con, n=1)
  while(!isTRUE(grepl(vertex_regex, line))) {
    h=c(h, line)
    line <- readLines(con, n=1)
  }
  
  params=.ParseAmirameshParameters(h)
  materials=names(params$Parameters$Materials)

  # read data blocks
  data_regex='^\\s*(\\w+)\\s+(\\d+)$'
  
  parse_data_line <- function(line) {
    tryCatch({
      res=stringr::str_match(line, data_regex)
      n=suppressWarnings(as.integer(res[,3]))
      checkmate::assert_int(n)
      label=checkmate::assert_character(res[,2])
      names(n)=label
      n
    }, error=function(e) {NA_integer_})
  }
  
  data <- list(header=h, params=params)
  
  curpatch=NA_integer_
  while(TRUE) {
    if(length(line)<1) break
    if(is.finite(curpatch)) {
      if(length(data[['PatchInfo']])<curpatch)
        data[['PatchInfo']][[curpatch]]=line
      else 
        data[['PatchInfo']][[curpatch]]=c(data[['PatchInfo']][[curpatch]], line)
    } else {
      data[['header']]=c(data[["header"]], line)
    }
    # is this a closing bracket at the end of a section
    firstchar=substr(trimws(line), 1, 1)
    if(isTRUE(firstchar=='}') && is.finite(curpatch)) curpatch=curpatch+1
    
    n=parse_data_line(line)
    if(is.na(n) || n==0) {
      line <- readLines(con, 1)
      next
    }
    label=names(n)
    if(label=='Vertices') {
      chunk=readBin(con, what='numeric', n=n*3, size=4, endian = 'big')
      data[['Vertices']]=matrix(chunk, ncol=3, byrow = T)
    } else if (label=='Triangles') {
      npatches=length(data[['Patches']])
      chunk=readBin(con, what='integer', n=n*3, size=4, endian = 'big')
      data[['Patches']][[npatches+1]]=matrix(chunk, ncol=3, byrow = T)
    } else if(label=='Patches') {
      curpatch=1
      if(is.null(data[['Patches']])) data[['Patches']]=list()
      if(is.null(data[['PatchInfo']])) data[['PatchInfo']]=list()
    } else {
      stop("Error parsing binary hxsurf file!")
    }
    
    line <- readLines(con, 1)
  }
  if(return.raw)
    data
  else parse.hxsurf.bin(data, FallbackRegionCol=FallbackRegionCol, Verbose=Verbose)
}

# FIXME: Ideally this would be harmonised with the code for read.hxsurf to 
# avoid duplication
parse.hxsurf.bin <- function(data, FallbackRegionCol, Verbose) {
  materials=data$params$Parameters$Materials
  d=list()
  d[['Vertices']]=as.data.frame(xyzmatrix(data$Vertices))
  d[['Vertices']][,'PointNo']=seq_len(nrow(d[['Vertices']]))
  
  d$Regions=list()
  for(p in seq_along(data$Patches)) {
    thispatch=as.data.frame(data$Patches[[p]])
    pi=.ParseAmirameshParameters(data$PatchInfo[[p]])
    RegionName <- pi$InnerRegion
    
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
  
  d$RegionList=names(d$Regions)
  
  d$RegionColourList <- vector(length=length(d$RegionList))
  for(regionName in d$RegionList) {
    rgbValues <-  materials[[regionName]][['Color']]
    if(isTRUE(length(rgbValues)==3)) {
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
#' @family hxsurf
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

#' Plot amira surface objects in 3D using rgl
#' 
#' @param x An hxsurf surface object
#' @param materials Character vector or \code{\link[base]{regex}} naming
#'   materials to plot (defaults to all materials in x). See
#'   \code{\link{subset.hxsurf}}.
#' @param col Character vector specifying colors for the materials, or a
#'   function that will be called with the number of materials to plot. When
#'   \code{NULL} (default) will use material colours defined in Amira (if
#'   available), or \code{rainbow} otherwise.
#' @param ... Additional arguments passed to \code{triangles3d}
#' @inheritParams plot3d.neuronlist
#' @export
#' @seealso \code{\link{read.hxsurf}}
#' @family hxsurf
#' @examples 
#' plot3d(kcs20)
#' plot3d(MBL.surf)
#' 
#' \donttest{
#' # plot only vertical lobe
#' nclear3d()
#' plot3d(MBL.surf, materials="VL", alpha=0.3)
#' 
#' # everything except vertical lobe
#' nclear3d()
#' plot3d(MBL.surf, alpha=0.3, 
#'   materials=grep("VL", MBL.surf$RegionList, value = TRUE, invert = TRUE))
#' }
plot3d.hxsurf<-function(x, materials=NULL, col=NULL, gridlines = FALSE, ...,
                        plotengine = getOption('nat.plotengine')){
  plotengine <- check_plotengine(plotengine)
  if (plotengine == 'rgl'){
    # skip so that the scene is updated only once per hxsurf object
    skip <- par3d(skipRedraw = TRUE)
    on.exit(par3d(skip))
  } 
  if (plotengine == 'plotly') {
    psh <- openplotlyscene()$plotlyscenehandle
    params=list(...)
    opacity <- if("alpha" %in% names(params)) params$alpha else 1
  }
  
  materials=subset(x, subset = materials, rval='names')
  
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
  for(mat in materials) {
    # get order triangle vertices
    tri=as.integer(t(x$Regions[[mat]]))
    if (plotengine == 'rgl'){
      rlist[[mat]]=triangles3d(x[['Vertices']]$X[tri],x[['Vertices']]$Y[tri],
                               x[['Vertices']]$Z[tri],col=col[mat], ...)
    } else {
      tmpx <- as.mesh3d.hxsurf(x, Regions = mat)
      psh <- psh %>% 
        plotly::add_trace(x = tmpx$vb[1,], 
                          y = tmpx$vb[2,], 
                          z = tmpx$vb[3,],
                          i = tmpx$it[1,]-1, 
                          j = tmpx$it[2,]-1, 
                          k = tmpx$it[3,]-1,
                          type = "mesh3d", opacity = opacity,
                          hovertext=mat,
                          hoverinfo="x+y+z+text",
                          facecolor = rep(col[mat],
                                          length(tmpx$it[1,])))
    }
  }
  if (plotengine == 'rgl'){
      invisible(rlist)
  } else {
    psh <- psh %>% plotly::layout(showlegend = FALSE, scene=list(camera=.plotly3d$camera))
    if(gridlines == FALSE){
      psh <- psh %>% plotly::layout(scene = list(xaxis=.plotly3d$xaxis,
                                                 yaxis=.plotly3d$yaxis,
                                                 zaxis=.plotly3d$zaxis))
    }
    assign("plotlyscenehandle", psh, envir=.plotly3d)
    psh
  }
}

#' Convert an object to an rgl mesh3d
#' 
#' Note that this provides a link to the Rvcg package
#' @param x Object to convert to mesh3d
#' @param ... Additional arguments for methods
#' @param Regions Character vector or regions to select from \code{hxsurf}
#'   object
#' @param material rgl materials such as \code{color}
#' @param drop Whether to drop unused vertices (default TRUE)
#' @export
#' @rdname as.mesh3d
#' @seealso \code{\link[rgl]{as.mesh3d}}, \code{\link[rgl]{tmesh3d}},
#'   \code{\link{as.hxsurf}}, \code{\link{read.hxsurf}}
#' @family hxsurf
as.mesh3d.hxsurf<-function(x, Regions=NULL, material=NULL, drop=TRUE, ...){
  if(is.null(Regions)) {
    Regions=x$RegionList
  }
  x=subset(x, Regions, drop=drop)
  if(length(Regions)==1 && is.null(material)){
    # find colour
    material=list(color=x$RegionColourList[match(Regions,x$RegionList)])
  } 
  verts=t(data.matrix(x$Vertices[,1:3]))
  inds=t(data.matrix(do.call(rbind, x$Regions)))
  tmesh3d(vertices=verts, indices=inds, homogeneous = FALSE, material = material, ...)
}

#' @description \code{as.mesh3d.boundingbox} converts a nat
#'   \code{\link{boundingbox}} object into an rgl compatible \code{mesh3d}
#'   object.
#' @rdname as.mesh3d
#' @export
#' @examples 
#' bb=boundingbox(kcs20)
#' mbb=as.mesh3d(bb)
#' \donttest{
#' plot3d(kcs20)
#' # simple plot
#' plot3d(bb)
#' shade3d(mbb, col='red', alpha=0.3)
#' 
#' }
as.mesh3d.boundingbox <- function(x, ...) {
  centroid=colMeans(x)
  size=diff(x)/2
  mat=scaleMatrix(size[1], size[2], size[3])%*%translationMatrix(centroid[1], centroid[2], centroid[3])
  cube3d(mat)
}

#' Convert an object to a nat hxsurf object
#' 
#' @details \code{hxsurf} objects are based on the format of Amira's surface 
#'   objects (see \code{\link{read.hxsurf}}). They have the ability to include 
#'   multiple distinct regions. However, at the moment the only method that we 
#'   provide converts \code{mesh3d} objects, which can only include one region.
#' @param x A surface object
#' @param ... Additional arguments passed to methods
#'   
#' @return A new surface object of class \code{hxsurf} (see 
#'   \code{\link{read.hxsurf}}) for details.
#' @export
#' @family hxsurf
#' @seealso \code{\link{as.mesh3d}}
#' @examples
#' tet=tetrahedron3d(col='red')
#' teth=as.hxsurf(tet)
#' \donttest{
#' plot3d(teth)
#' }
as.hxsurf <- function(x, ...) UseMethod('as.hxsurf')

#' @param region The default name for the surface region
#' @param col The surface colour (default value of NULL implies the colour
#'   specified in mesh3d object or \code{grey} when the \code{mesh3d} object has
#'   no colour.)
#' @export
#' @rdname as.hxsurf
as.hxsurf.mesh3d <- function(x, region="Interior", col=NULL, ...) {
  if (is.null(x$it))
    stop("This method only works for triangular mesh3d objects!")
  h=list()
  h$Vertices=data.frame(xyzmatrix(x))
  colnames(h$Vertices)=c("X","Y","Z")
  h$Vertices$PointNo=1:nrow(h$Vertices)
  h$Regions[[region]]=data.frame(t(x$it))
  colnames(h$Regions[[region]])=c("V1","V2","V3")
  h$RegionList=names(h$Regions)
  if(is.null(col)) col=x$material$col
  h$RegionColourList <- if(!is.null(col)) col else 'grey'
  class(h)=c("hxsurf","list")
  h
}


#' Subset hxsurf object to specified regions
#' 
#' @param x A dotprops object
#' @param subset Character vector specifying regions to keep. Interpreted as 
#'   \code{\link[base]{regex}} if of length 1 and no fixed match.
#' @param drop Whether to drop unused vertices after subsetting (default:
#'   \code{TRUE})
#' @param rval Whether to return a new \code{hxsurf} object or just the names of
#'   the matching regions
#' @param ... Additional parameters (currently ignored)
#' @return subsetted hxsurf object
#' @export
#' @family hxsurf
#' @examples
#' # plot only vertical lobe
#' vertical_lobe=subset(MBL.surf, "VL")
#' \donttest{
#' plot3d(vertical_lobe, alpha=0.3)
#' plot3d(kcs20)
#' 
#' # there is also a shortcut for this
#' nclear3d()
#' plot3d(MBL.surf, subset = "VL", alpha=0.3)
#' }
subset.hxsurf<-function(x, subset=NULL, drop=TRUE, rval=c("hxsurf","names"), ...){
  rval=match.arg(rval)
  if(!is.null(subset)){
    tokeep=integer(0)
    if(is.character(subset)){
      tokeep=match(subset,x$RegionList)
      if(is.na(tokeep[1]) && length(subset)==1){
        # try as regex
        tokeep=grep(subset,x$RegionList)
      }
    }
    if(!length(tokeep) || any(is.na(tokeep)))
      stop("Invalid subset! See ?subset.hxsurf")
    if(rval=='names') return(x$RegionList[tokeep])
    x$Regions=x$Regions[tokeep]
    x$RegionList=x$RegionList[tokeep]
    x$RegionColourList=x$RegionColourList[tokeep]
  } else if(rval=='names') return(x$RegionList)
  
  if(drop){
    # see if we need to drop any vertices
    vertstokeep=sort(unique(unlist(x$Regions)))
    # a vector where each position is the old vertex id and the value is the
    # new one i.e. newid=vert_table[oldid]
    vert_table=match(seq_len(nrow(x$Vertices)), vertstokeep)
    # convert all vertex ids from old to new sequence
    for(r in x$RegionList){
      for(i in seq_len(ncol(x$Regions[[r]]))){
        x$Regions[[r]][[i]]=vert_table[x$Regions[[r]][[i]]]
      }
    }
    # drop unused vertices
    x$Vertices=x$Vertices[vertstokeep, ]
  }
  x
}

#' Subset methods for different nat objects
#' 
#' These methods enable subsets of some nat objects including neurons and 
#' neuronlists to be obtained. See the help for each individual method for 
#' details.
#' 
#' @name subset
#' @seealso \code{\link{subset.neuron}}, \code{\link{subset.dotprops}},
#'   \code{\link{subset.hxsurf}}, \code{\link{subset.neuronlist}}
NULL

#' Find which points of an object are inside a surface
#'
#' @details Note that \code{hxsurf} surface objects will be converted to
#'   \code{mesh3d} before being passed to \code{Rvcg::vcgClostKD}, so if you are
#'   testing repeatedly against the same surface, it may make sense to
#'   pre-convert.
#'
#'   \code{pointsinside} depends on the face normals for each face pointing out
#'   of the object (see example). The face normals are defined by the order of
#'   the three vertices making up a triangular face. You can flip the face
#'   normal for a face by permuting the vertices (i.e. 1,2,3 -> 1,3,2). If you
#'   find for a given surface that points are outside when you expect them to be
#'   inside then the face normals are probably all the wrong way round. You can
#'   invert them yourself or use the \code{Morpho::invertFaces} function to fix
#'   this.
#'
#'   The \code{rval} argument determines the return value. These options should
#'   be fairly clear, but the difference between \code{logical} and
#'   \code{consistent_logical} needs some explanation. The \code{logical} method
#'   now does a pre-test to remove any points that are not in the 3D bounding
#'   box (cuboid) enclosing the surf object. This often results in a significant
#'   speed-up by rejecting distant points and has the additional benefit of
#'   rejecting distant points that sometimes are erroneously declared inside the
#'   mesh (see below). Regrettably it is not yet possible to extend this
#'   approach when distances are being returned, which means there will be a
#'   discrepancy between the results of \code{rval="logical"} and looking for
#'   points with distance >=0. If you want to ensure consistency between these
#'   approaches, use \code{rval="consistent_logical"}.
#'
#'   If you find that some points but not all points are not behaving as you
#'   would expect, then it may be that some faces are not coherently oriented.
#'   The \code{Rvcg::\link[Rvcg]{vcgClean}} function can sometimes be used to
#'   correct the orientation of the faces. Fixing more problematic cases may be
#'   possible by generating a new surface using
#'   \code{alphashape3d::\link[alphashape3d]{ashape3d}} (see examples).
#'
#' @param x an object with 3D points.
#' @param surf The reference surface - either a \code{mesh3d} object or any
#'   object that can be converted using \code{as.mesh3d} including \code{hxsurf}
#'   and \code{ashape3d} objects.
#' @param ... additional arguments for methods, eventually passed to
#'   \code{\link{as.mesh3d}}.
#' @export
#' @examples
#' # check if the vertices in these neurons are inside the mushroom body calyx
#' # surface object
#' inout=pointsinside(kcs20, surf=subset(MBL.surf, "MB_CA_L"))
#' table(inout)
#' # you can also check if points are inside a bounding box
#' mbcalbb=boundingbox(subset(MBL.surf, "MB_CA_L"))
#' inout2=pointsinside(kcs20, mbcalbb)
#' # compare those two
#' table(inout, inout2)
#' pts=xyzmatrix(kcs20)
#' # nb that colour expressions maps combinations of two logicals onto 1:4
#' plot(pts[,1:2], col=1+inout+inout2*2)
#' # the colours are defined by
#' palette()[1:4]
#' 
#' # be a bit more lenient and include points less than 5 microns from surface
#' MBCAL=subset(MBL.surf, "MB_CA_L")
#' inout5=pointsinside(kcs20, surf=MBCAL, rval='distance') > -5
#' table(inout5)
#' \donttest{
#' # show which points are in or out
#' # Hmm seems like there are a few red points in the vertical lobe
#' # that are well outside the calyx
#' points3d(xyzmatrix(kcs20), col=ifelse(inout5, 'red', 'black'))
#' plot3d(MBL.surf, alpha=.3)
#'
#' # Let's try to make an alphashape for the mesh to clean it up
#' library(alphashape3d)
#' MBCAL.as=ashape3d(xyzmatrix(MBCAL), alpha = 10)
#' # Plotting the points, we can see that is much better behaved
#' points3d(xyzmatrix(kcs20),
#'   col=ifelse(pointsinside(kcs20, MBCAL.as), 'red', 'black'))
#' }
#'
#' \dontrun{
#' # Show the face normals for a surface
#' if(require('Morpho')) {
#'   # convert to a mesh3d object used by rgl and Morpho packge
#'   MBCAL.mesh=as.mesh3d(subset(MBL.surf, "MB_CA_L"))
#'   fn=facenormals(MBCAL.mesh)
#'   wire3d(MBCAL.mesh)
#'   # show that the normals point out of the object
#'   plotNormals(fn, long=5, col='red')
#'
#'   # invert the faces of the mesh and show that normals point in
#'   MBCAL.inv=invertFaces(MBCAL.mesh)
#'   plotNormals(facenormals(MBCAL.inv), long=5, col='cyan')
#' }
#' }
pointsinside<-function(x, surf, ...) UseMethod('pointsinside')

#' @export
#' @param rval what to return.
#' @return A vector of logical values or distances (positive inside, negative
#'   outside) equal to the number of points in x or the \code{mesh3d} object
#'   returned by \code{Rvcg::vcgClostKD}.
#' @rdname pointsinside
pointsinside.default<-function(x, surf, ..., rval=c('logical','distance', 
                                                    'mesh3d', 'consistent_logical')) {
  rval=match.arg(rval)
  if(rval=='logical') {
    # use optimised contains_points approach
    return(contains_points(surf, x, ...))
  }
  
  if(inherits(surf, "boundingbox")) {
      stop("Only logical return values are currently possible ",
           "with boundingbox objects!")
  }
  
  if(!requireNamespace('Rvcg', quietly = TRUE))
    stop("Please install suggested library Rvcg to use pointsinside")
  
  if(!inherits(surf,'mesh3d')) {
    surf=as.mesh3d(surf, ...)
  }

  pts=xyzmatrix(x)
  rmesh=Rvcg::vcgClostKD(pts, surf, sign = TRUE)
  switch(rval,
    consistent_logical = rmesh$quality >= 0,
    distance = rmesh$quality,
    mesh3d = rmesh
  )
}

contains_points <- function(obj, points, ...) UseMethod("contains_points")

contains_points.boundingbox <- function(obj, points,  ...) {
  xyz=xyzmatrix(points)
  xyz[,1] >= obj[1,1] & xyz[,2] >= obj[1,2] & xyz[,3] >= obj[1,3] &
    xyz[,1] <= obj[2,1] & xyz[,2] <= obj[2,2] & xyz[,3] <= obj[2,3]
}

contains_points.mesh3d <- function(obj, points,  ...) {
  xyz=xyzmatrix(points)
  inbb=contains_points(boundingbox(obj), xyz, ...)
  # nb must call the original logical method to avoid infinite recursion
  iosurf=pointsinside(xyz[inbb,,drop=F], surf = obj, ..., rval='consistent_logical')
  res=inbb
  res[inbb]=iosurf
  res
}

contains_points.hxsurf<-contains_points.mesh3d

contains_points.ashape3d<-function(obj, points,  ...) {
  alphashape3d::inashape3d(obj, points=xyzmatrix(points), ...)
}
