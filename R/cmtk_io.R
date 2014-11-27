# Functions for input/outpot of CMTK's TYPEDSTREAM format
# which is used for registrations etc

#' Read a CMTK format registration
#' 
#' @param filename Path to a CMTK registration file
#' @param ReturnRegistrationOnly When FALSE (default) will not attempt to 
#'   extract the registration element from the registration file.
#' @param ... Additional arguments passed to read.cmtk
#' @export
#' @family cmtk-io
read.cmtkreg <- function(filename, ReturnRegistrationOnly=FALSE, ...){
  filename=cmtkreg(filename,returnDir=FALSE)
  r=read.cmtk(filename, ...)
  if(!is.null(r$registration) && ReturnRegistrationOnly) {
    rval=r$registration
    attr.r=attributes(r)
    attr.rval=attributes(rval)
    extra_attributes=setdiff(names(attr.r),names(attr.rval))
    attributes(rval)[extra_attributes]<-attributes(r)[extra_attributes]
    return(rval)
  }
  else return(r)
}

#' @title nat package internal functions
#' @description Utility function to trim whitespace at start and end of lines
#' @rdname nat-internal
#' @param t Character vector to trim
trim<-function(t) sub('[[:space:]]+$', '', sub('^[[:space:]]+', '', t))

#' Read CMTK TypedStream file to a list in memory
#' 
#' This function is primarily of developer interest. End users will typically 
#' want to use more specialised functions for reading registrations and 
#' landmarks.
#' @details This is the default format used by CMTK for registration, studylist,
#'   landmarks and image files. Although this is largely a generic function, 
#'   there is special handling of the coefficients and active members of the 
#'   spline warp component of a CMTK nonrigid registrartion.
#' @details Note that if an open connection is passed to read.cmtk the version 
#'   number of the CMTK TypedStream will not be checked or recorded.
#' @param con Path to (optionally gzipped) file or (open) connection.
#' @param CheckLabel Check, fix and warn for invalid or duplicate labels 
#'   (default TRUE)
#' @export
#' @family cmtk-io
read.cmtk<-function(con, CheckLabel=TRUE){
    
  l=list()
  
  if(is.character(con)) {
    filename=con
    con=file(filename,'rt')
    t=readLines(con,1)
    if( !any(grep("! TYPEDSTREAM",t[1],fixed=TRUE,useBytes=TRUE)) )
      stop(paste("This doesn't appear to be an IGS TypedStream:",filename))
    typedStreamVersion=numeric_version(sub("! TYPEDSTREAM ","",t[1],
                                           fixed=TRUE,useBytes=TRUE))
    attr(l,"version")<-typedStreamVersion
  }
  
  checkLabel=function(label) {
    if( any(names(l)==label)  ){
      newlabel=make.unique(c(names(l),label))[length(l)+1]
      warning(paste("Duplicate item",label,"renamed",newlabel))
      label=newlabel
    }
    label
  }
  # Should this check to see if the connection still exists?
  # in case we want to bail out sooner
  while ( isTRUE(isOpen(con)) ){
    thisLine<-readLines(con,1)
    # no lines returned - ie end of file
    if(length(thisLine)==0) break
    
    # trim and split it up by white space
    thisLine=trim(thisLine)
    
    # skip if this is a blank line
    if(nchar(thisLine)==0) next
    
    items=strsplit(thisLine," ",fixed=TRUE)[[1]]
    
    if(length(items)==0) next
    # get the label and items
    label=items[1]; items=items[-1]
    #cat("\nlabel=",label)
    #cat("; items=",items)
    
    # return list if this is the end of a section
    if(label=="}") {
      #cat("end of section - leaving this recursion\n")
      return (l)
    }		
    if(items[1]=="{"){
      # parse new subsection
      #cat("new subsection -> recursion\n")
      # set the list element!
      if(CheckLabel)
        label=checkLabel(label)
      
      l[[length(l)+1]]=read.cmtk(con,CheckLabel=CheckLabel)
      names(l)[length(l)]<-label
      next
    }
    
    if(label == "coefficients"){
      # process coefficients
      numItems=prod(l[["dims"]])*3
      #cat("numItemsToRead =",numItems)
      remainingItems=scan(con,n=numItems-3,quiet=TRUE)
      l[[label]]=matrix(c(as.numeric(items),remainingItems),ncol=3,byrow=T)
      
    } else if (label == "active"){
      # process active flags
      numItems=prod(l[["dims"]])*3
      #cat("numItemsToRead =",numItems)
      # nb -1 since we have read one line of 30 already
      numLinesToRead=ceiling(numItems/30)-1
      #cat("numLinesToRead=",numLinesToRead)
      x=c(items,trim(readLines(con,numLinesToRead,ok=FALSE)))
      #x=paste(x,collapse="")
      bits=strsplit(x,"")
      bits=as.raw(unlist(bits))
      l[[label]]=bits
    } else {
      # ordinary item
      # Check first item
      firstItemFirstChar=substr(items[1],1,1)		
      if(any(firstItemFirstChar==c("-",as.character(0:9)) )){
        # convert to numeric if not a string
        items=as.numeric(items)
      } else if (firstItemFirstChar=="\""){
        # dequote quoted string
        # can do this by using a textConnection
        tc=textConnection(thisLine)
        items=scan(tc,what="",quiet=TRUE)[-1]
        close(tc)
        attr(items,"quoted")=TRUE
      }
      # check if the list already has one of these
      
      # set the list element!
      if(CheckLabel)
        label=checkLabel(label)
      
      l[[length(l)+1]]=items
      names(l)[length(l)]<-label
    }
  }
  # we should only get here once if we parse a valid hierarchy
  close(con)
  if(isTRUE(try(file.exists(filename)))){
    attr(l,"file.info")=file.info(filename)
  }
  return(l)
}

#' Write out CMTK registration list to folder
#' 
#' @details Note that transformation in the forward direction (i.e. sample->ref)
#'   e.g. as calculated from a set of landmarks where set 1 is the sample is 
#'   considered an inverse transformation by the IGS software. So in order to
#'   use such a transformation as an initial affine with the registration
#'   command the switch --initial-inverse must be used specifying the folder
#'   name created by this function.
#' @details CMTK v2.4 fixed a long-standing bug in affine (de)composition to
#'   CMTK params. This resulted in a non-backwards compatible change marked by
#'   writing the TYPEDSTREAM as version 2.4. The R code in this package
#'   implements both the new and old compose/decompose functions, using the new
#'   by default.
#' @param reglist List specifying CMTK registration parameters
#' @param foldername Path to registration folder (usually ending in .list)
#' @param version CMTK version for registration (default 2.4)
#' @export
#' @family cmtk-io
write.cmtkreg<-function(reglist, foldername, version="2.4"){
  if(!is.null(attr(reglist, 'version')) && (attr(reglist, 'version') != version)) warning("Specified version (", version, ") is not the same as the version stored in the reglist object (", attr(reglist, 'version'), ").")
  dir.create(foldername, showWarnings=FALSE, recursive=TRUE)
  if(!is.list(reglist)) reglist=cmtkreglist(reglist)
  write.cmtk(reglist,file.path(foldername, "registration"),
                      version=version)
  
  studysublist=list(studyname=reglist$registration$reference_study)
  attr(studysublist$studyname, "quoted")=TRUE
  studysublist2=studysublist
  if ('model_study' %in% names(reglist$registration)) {
    studysublist2$studyname=reglist$registration$model_study
  } else {
    studysublist2$studyname=reglist$registration$floating_study
  }
  studylist=list(studylist=list(num_sources=2),
                 source=studysublist, source=studysublist2)
  
  write.cmtk(studylist, file.path(foldername,"studylist"),
                      version=version)
}

#' Write a suitable list to a CMTK TypedStream file on disk
#' 
#' @description This is probaly only of interest to developers. End users will 
#'   probably wish to use more specific functions such as write.cmtkreg for
#'   writing out registrations.
#' @details NB a version specified on the command line overrides one encoded as 
#'   an attribute in the input list.
#' @param l Appropriately formatted list
#' @param con A character string specifying a path or a connection
#' @param gzip Whether to gzip output file (default FALSE)
#' @param version TYPEDSTREAM version number, defaults to \code{"1.1"} if not 
#'   specified in the version attribute of \code{l}.
#' @export
#' @family cmtk-io
write.cmtk<-function(l, con, gzip=FALSE, version=NA_character_){
  if (is.character(con)) {
    con <- if(gzip) gzfile(con, "w") else file(con, "w")
    on.exit(close(con))
  }

  if(is.na(version)){
    version=as.character(attr(l,'version'))
    if(!length(version)) version='1.1'
  }
  
  cat("! TYPEDSTREAM ", version, "\n\n", file=con, sep="")
  write.cmtk.list(l,con)
}

# function for internal use by write.cmtk
write.cmtk.list<-function(x,con,tablevel=0){
  nn <- names(x)
  ll <- length(x)
  tabs=""
  for(i in seq(len=tablevel)) tabs=paste(tabs,sep="","\t")
  if (length(nn) != ll) 
    nn <- paste("Component", seq(ll))
  for (i in seq(length = ll)) {
    # cat("i=",i,"name=",nn[i],"mode=",mode(x[[i]]),"\n")
    if (is.list(x[[i]])) {
      cat(sep="",tabs,nn[i]," {\n",file=con,append=TRUE)
      write.cmtk.list(x[[i]],con,tablevel+1)
      cat(sep="",tabs,"}\n",file=con,append=TRUE)
    } else if(is.matrix(x[[i]])){
      cat(sep="",tabs,nn[i]," ")
      write(t(x[[i]]),file=con,append=TRUE,ncolumns=ncol(x[[i]]))
    } else {
      qsep=""
      if(isTRUE(attr(x[[i]],"quoted"))) qsep="\""
      thisline=paste(nn[i],sep=""," ",paste(qsep,x[[i]],qsep,sep="",collapse=" "),"\n")
      cat(sep="",tabs,thisline,file=con,append=TRUE)
    }
  }
}

#' Make in-memory CMTK registration list from affine matrix or CMTK parameters
#' 
#' @details Note that this uses the modern CMTK notation of floating_study 
#'   rather than model_study as used by IGSParamsToIGSRegistration (which 
#'   results in an implicit inversion by CMTK tools).
#' @details Note that the reference and floating fields have no impact on the 
#'   transformation encoded in the resultant .list folder and can be overridden 
#'   on the command line of CMTK tools.
#' @param x 5x3 matrix of CMTK registration parameters OR 4x4 homogeneous affine
#'   matrix
#' @param centre Optional centre of rotation passed to \code{affmat2cmtkparams} 
#'   when decomposing 4x4 affine matrix
#' @param reference,floating Path to refererence and floating images.
#' @return \code{list} of class \code{cmtkreg} containing registration
#'   parameters suitable for \code{\link{write.cmtkreg}}
#' @export
#' @seealso \code{\link{write.cmtkreg}, \link{affmat2cmtkparams}, \link{cmtkreg}}
cmtkreglist<-function(x,centre=c(0,0,0),reference="dummy",floating="dummy"){
  
  mat.ok=FALSE
  if(is.matrix(x)){
    if(isTRUE(all.equal(dim(x), c(4,4)))) x=affmat2cmtkparams(x,centre=centre)
    if(isTRUE(all.equal(dim(x), c(5,3)))) mat.ok=TRUE
  }
  if(!mat.ok)
    stop("Expects either a 5x3 (CMTK parameters) or ",
         "4x4 (homogeneous affine) matrix")
    
  affine_xform=unlist(apply(x,1,list),recursive=F)
  names(affine_xform)=c("xlate","rotate","scale","shear","center")
  
  l=list(registration=list(reference_study=reference,
                           floating_study=floating,
                           affine_xform=affine_xform))
  attr(l$registration$reference_study,"quoted")=TRUE
  attr(l$registration$floating_study,"quoted")=TRUE
  version=attr(x,'version')
  if(is.null(version)) version=numeric_version('2.4')
  attr(l,'version')=version
  as.cmtkreg(l)
}

#' Extract affine registration from CMTK registration file or in-memory list
#' 
#' @param r A registration list or path to file on disk
#' @param outdir Optional path to output file
#' @return When \code{outdir} is missing a list containing the registration
#'   paramers. Otherwise \code{NULL} invisibly.
#' @family cmtk-io
#' @seealso \code{\link{cmtkreglist}}
#' @export
cmtk.extract_affine<-function(r, outdir) {
  f=NULL
  if(is.character(r)) {
    if(!file.exists(r)) stop("Can't find registration:", r)
    f=r
    r=read.cmtkreg(r)
  }
  
  required_fields=c("reference_study", "floating_study", "affine_xform")
  missing_fields=setdiff(required_fields, names(r$registration))
  if(length(missing_fields))
    stop("The registration is missing fields: ", cat(missing_fields, collapse=", "))
  
  r2=r
  # set other fields to NULL
  r2$registration[setdiff(names(r$registration), required_fields)]=NULL

  version=as.character(attr(r,'version'))
  if(!length(version)) version='2.4'
  if(!missing(outdir))
    write.cmtkreg(r2, foldername = outdir, version = version)
  else r2
}

# Read and Write CMTK landmarks
# 
# @details CMTK landmarks are always unpaired i.e. only contain information for
#   one brain.
# @param con Character vector specifying path or a connection (passed straight
#   to \code{read.cmtk})
# @rdname cmtklandmarks
read.landmarks.cmtk<-function(con){
  l=read.cmtk(con,CheckLabel=FALSE)
  x=t(sapply(l,function(x) x[["location"]]))
  rn=sapply(l,function(x) x[["name"]])
  # nb this is necessary to avoid the names having names 
  # of the form landmarks.1, landmarks.1 ...
  names(rn)<-NULL
  rownames(x)=rn
  x
}

is.cmtklandmarks<-function(f, bytes=NULL){
  if(!is.null(bytes) && length(f)>1)
    stop("can only supply raw bytes to check for single file")
  if(length(f)>1) return(sapply(f, is.cmtklandmarks))
  
  tocheck=if(is.null(bytes)) f else bytes
  if(!generic_magic_check(tocheck, "! TYPEDSTREAM")) return(FALSE)
  # still not sure? Now we need to start reading in some lines
  h=readLines(f, n = 3)
  isTRUE(any(grepl("landmark",h, useBytes=T, fixed = T)))
}

# @description \code{cmtklandmarks} generates in memory list representation of
#   cmtk landmarks
# @param xyzs Nx3 matrix of landmarks
# @rdname cmtklandmarks
# @family cmtk-io
cmtklandmarks<-function(xyzs){
  # IGS Landmark lists are unpaired ie contain information for only 1 brain
  xyzs=data.matrix(xyzs)
  ns=rownames(xyzs)
  ll=list()
  for(i in 1:nrow(xyzs)){
    ll[[i]]=list(name=paste("\"",ns[i],"\"",sep=""),location=xyzs[i,])		
  }
  names(ll)=rep('landmark',length(ll))
  ll
}

# @param filename Path to write out cmtklandmarks
# @rdname cmtklandmarks
write.landmarks.cmtk<-function(xyzs,filename){
  ll=cmtklandmarks(xyzs)
  if(file.exists(filename) && file.info(filename)$isdir) filename=file.path(filename,"landmarks")
  write.cmtk(ll,filename)
}
