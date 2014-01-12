ParseAMSurfToContourList<-function(filename,RegionNames="ALL",RegionChoice="Inner",Verbose=FALSE,FallbackRegionCol="grey"){
  # function to parse a an amira  HxSurface file
  # nb RegionChoice is a switch to allow the inneror outer region to
  # define the name of the region
  
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
  
  # round to 3dp to avoid any surprises (like v small -ve numbers)
  d$Vertices=round(d$Vertices,digits=3)
  d$Vertices$PointNo=seq(nrow(d$Vertices))
  if(Verbose) cat("Finished processing Vertices\n")
  
  # Now read in Triangles that define patches:
  linesSkipped=dataStart+nVertices-1
  remainingLines=t[(dataStart+nVertices):nLines]
  PatchDefLine=grep("^\\s*Patches\\s*",remainingLines,perl=TRUE)
  cat("PatchDefLine =",PatchDefLine,"\n")
  nPatches=as.numeric(getfield("Patches",remainingLines[PatchDefLine],2))
  cat("nPatches =",nPatches,"\n")
  PatchStarts=grep("^\\s*{",remainingLines[PatchDefLine:length(remainingLines)],perl=TRUE)+PatchDefLine-1
  if(length(PatchStarts)>nPatches) PatchStarts=PatchStarts[1:nPatches]
  PatchEnds=grep("^\\s*}",remainingLines[PatchDefLine:length(remainingLines)],perl=TRUE)+PatchDefLine-1
  if(length(PatchEnds)>nPatches) PatchEnds=PatchEnds[1:nPatches]
  #return(d)
  TriangleDeflines<-grep("Triangles",remainingLines)
  #myreadtable<-function(...) scan(...)
  for(i in 1:nPatches){
    if(!any(TriangleDeflines[i])){
      warning(paste("Unable to find Triangle number in patch",i,"in",filename,"\n"))
      return (NULL)
    }
    if(Verbose) cat("TriangleDefline =",TriangleDeflines[i],"\n")
    PatchHeader<-remainingLines[PatchStarts[i]:TriangleDeflines[i]]
    if(Verbose) cat("PatchHeader is",length(PatchHeader),"lines long\n")
    # note use of RegionChoice to switch naming between inner and outer
    RegionName=getfield(paste(RegionChoice,"Region",sep=""),PatchHeader,2)
    #RegionName=getfield("InnerRegion",PatchHeader,2)
    nTriangles=as.numeric(getfield("Triangles",PatchHeader,2))
    if(nTriangles<0 || nTriangles>100000){return(-1)}
    if(Verbose) cat("nTriangles =",nTriangles,"for patch =",i,"\n")
    # Check if we want to load in this region
    if( ("ALL"%in%RegionNames) || (RegionName%in%RegionNames) ){
      # Ensure we do not try to add no triangles
      if(nTriangles == 0) next
      # check if we have already loaded a patch in this name
      if(RegionName%in%names(d$Regions)){
        #return(d)
        # add to the old patch
        if(Verbose) cat("Adding to patch name",RegionName,"\n")
        d[['Regions']][[RegionName]]=rbind(d[['Regions']][[RegionName]],read.table(filename,skip=linesSkipped+TriangleDeflines[i],nrows=nTriangles,col.names=c("V1","V2","V3")))
      } else {
        # new patch
        if(Verbose) cat("Making new patch name",RegionName,"\n")
        # scan no quicker in these circs, problem is repeated file 
        # access
        #d[[RegionName]]=as.data.frame(matrix(scan(filename,skip=linesSkipped+TriangleDeflines[i],nlines=nTriangles),ncol=3,byrow=T))
        d[['Regions']][[RegionName]]=read.table(filename,skip=linesSkipped+TriangleDeflines[i],nrows=nTriangles,col.names=c("V1","V2","V3"))
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

plot3dsurface<-function(material,d,VertexComponent="Vertices",col=rainbow,...){
  # simple function to plot surfaces as read in using ParseAMSurfToContourList
  # handle multiple objects
  if(length(material)>1) {
    if(is.function(col)) col=col(length(material))
    if(is.factor(col)) col=rainbow(nlevels(col))[as.integer(col)]		
    
    invisible(mapply(
      plot3dsurface,material,VertexComponent=VertexComponent,col=col,...,MoreArgs=list(d=d)))
  } else {
    # get order triangle vertices
    tri=as.integer(t(d$Regions[[material]]))
    invisible(triangles3d(d[[VertexComponent]]$X[tri],
                          d[[VertexComponent]]$Y[tri],d[[VertexComponent]]$Z[tri],col=col,...))
  }
}
