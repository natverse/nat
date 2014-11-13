ReadLongairTraceData<-function(f, ..., Verbose=FALSE){
  doc=try(XML::xmlParse(f, ...))
  if(inherits(doc, 'try-error')) stop("Unable to parse file as neuroml")

  r<-XML::xmlRoot(doc)
  if(XML::xmlName(r)!="tracings") stop("This is not a Longair format tracing")
  
  l<-list()
  stopifnot(names(r)[1:2]==c("samplespacing","imagesize"))	
  attr(l,"samplespacing")<-r['samplespacing'][[1]]
  attr(l,"imagesize")<-r['imagesize'][[1]]
  
  tracings=r[-c(1:2)]
  if(length(tracings)==0) stop("No tracings in this file")
  if(Verbose) cat("There are",length(tracings),"tracings in this file\n")	
  
  for(i in 1:length(tracings)){
    l[[i]]=XML::xmlSApply(tracings[[i]],function(x) as.numeric(XML::xmlAttrs(x)[c("xd","yd","zd")]))
    l[[i]]=t(l[[i]])
    rownames(l[[i]])<-NULL
    colnames(l[[i]])<-c("X","Y","Z")
    pathAttributes=XML::xmlAttrs(tracings[[i]])
    attr(l[[i]],'pathAttributes')=pathAttributes
    # set the list item name to the tracing id 
    # (a number, but not necessarily from a perfect 0 indexed sequence)
    names(l)[i]=pathAttributes['id']
  }
  l
}

read.neuron.fiji<-function(f, ..., simplify=TRUE){
  l=ReadLongairTraceData(f,...)
  dflist=as.list(rep(NA,length(l)))
  MasterPath=seq(l)
  pointOffsets=rep(0,length(l))
  names(dflist)<-names(pointOffsets)<-names(MasterPath)<-names(l)	
  
  for(id in names(l)){
    d=l[[id]]
    df=seglist2swc(list(1:nrow(d)), d=xyzmatrix(d))
    pathAttributes=attr(l[[id]],"pathAttributes")
    
    if('startson'%in%names(pathAttributes)){
      # this path is joined to another, so find out which
      parentPathId=pathAttributes['startson']
      # now find the Master Path of that path
      MasterPath[id]=MasterPath[parentPathId]
      
      # make a note of the number of points by which we have to offset
      # points for this path
      pointOffsets[id]=nrow(dflist[[MasterPath[id]]])
      
      # adjust all the point ids by the number of rows in the master data fram
      # in the parent path data frame
      df[,c("Parent","PointNo")]=df[,c("Parent","PointNo")]+pointOffsets[id]
      
      # now find the index of the point in the parent path to which this path is corrected
      # nb Mark's paths are 0 indexed (R is 1 indexed)
      parentStartIndex=as.numeric(pathAttributes['startsindex'])+1
      # ... and correct this (in case that path was not path 0)
      parentStartIndex=parentStartIndex+pointOffsets[parentPathId]
      # now set the parent of this new path to the point on the parent path
      df$Parent[1]=parentStartIndex
      
      # finally add these data to the dataframe for the master path
      dflist[[MasterPath[id]]]=rbind(dflist[[MasterPath[id]]],df)
    } else {
      df$Parent[1]=-1 # if this is a new path set root's parent to -1
      dflist[[id]]=df
    }
  }
  
  # Actually make neurons from the dataframes of points
  neuronList=list()
  for(df in dflist){
    if(!is.data.frame(df)) next
    neuronList[[length(neuronList)+1]]=as.neuron(df, InputFileName=f)
  }
  names(neuronList)=paste(sub("\\.[^\\.]+$","",basename(f)),sep="-",seq(neuronList))
#   if(MergePaths) {
#     else MergeUnconnectedPathsToSingleNeuron(neuronList)
#   }
  if(simplify && length(neuronList)==1) return(neuronList[[1]])
  else neuronList
}
