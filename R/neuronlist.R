#' Create and test objects of neuronlist class to store multiple neurons
#' 
#' @description \code{neuronlist} objects consist of a list of neuron objects 
#'   along with an optional attached dataframe containing information about the 
#'   neurons. \code{neuronlist} objects can be indexed using their name or the 
#'   number of the neuron like a regular list. If the \code{[} operator is used
#'   to index the list, the attached dataframe will also be subsetted.
#' @rdname neuronlist
#' @family neuronlist
#' @description \code{is.neuronlist} Test if object is a neuronlist
#'   
#' @details  \code{is.neuronlist} Uses a relaxed definition to cope with older 
#'   lists of neurons that do not have a class attribute of neuronlist
#' @param x A neuronlist object
#' @return return value
#' @export
is.neuronlist<-function(x) {
  inherits(x,"neuronlist") ||
    (is.list(x) && length(x)>1 && is.neuron(x[[1]]))
}

#' Create a neuronlist from zero or more neurons
#' 
#' It is perfectly acceptable not to pass any parameters, generating an empty 
#' neuronlist
#' @param ... objects to be turned into a list
#' @param DATAFRAME an optional data.frame to attach to the neuronlist
#'   containing information about each neuron.
#' @return return value
#' @export
#' @examples
#' # generate an empty neuronlist
#' nl=neuronlist()
#' # slice an existing neuronlist with regular indexing
#' kcs5=kcs20[1:5]
neuronlist <- function(..., DATAFRAME=NULL) as.neuronlist(list(...), df=DATAFRAME)

#' Make a list of neurons that can be used for coordinate plotting/analysis
#'
#' Note that it can cope with both neurons and dotprops but AddClassToNeurons
#' parameter will only apply to things that look like neurons but don't have
#' a class of neuron.
#' @param l An existing list or a single neuron to start a list
#' @param ... Additional arguments passed to methods
#' @return neuronlist with attr('df')
#' @export
#' @seealso \code{\link{is.neuronlist}},\code{\link{is.neuron}},\code{\link{is.dotprops}}
as.neuronlist<-function(l, ...) UseMethod("as.neuronlist")

#' @S3method as.neuronlist default
#' @method as.neuronlist default
as.neuronlist.default<-function(l, df, AddClassToNeurons=TRUE, ...){
  if(is.neuron(l)) {
    n<-l
    l<-list(n)
    names(l)<-n$NeuronName
  }
  if(!missing(df) &&!is.null(df)) {
    if(nrow(df)!=length(l)) 
      stop("data frame must have same number of rows as there are neurons")
    attr(l,"df")=df
    if(is.null(names(l)))
      names(l)=rownames(df)
    else if(any(names(l)!=rownames(df)))
      stop("mismatch between neuronlist names and dataframe rownames")
  }
  if(!inherits(l,"neuronlist")) class(l)<-c("neuronlist",class(l))
  if(!AddClassToNeurons) return(l)
  for(i in seq(l)){
    if(!is.neuron(l[[i]],Strict=TRUE) && is.neuron(l[[i]]))
      l[[i]]=as.neuron(l[[i]])
  }
  l
}

#' @method [ neuronlist
#' @S3method [ neuronlist
"[.neuronlist" <- function(x,i,...) {
  nl2=structure(NextMethod("["), class = class(x))
  df=attr(x,'df')
  if(!is.null(df)){
    attr(nl2,'df')=df[i,,...]
  }
  nl2
}

#' lapply for neuronlists
#'
#' Looks after class and any attached dataframe.
#' @param X A neuronlist
#' @param FUN Function to be applied to each element of X
#' @param ... Additional arguments for FUN
#' @return A neuronlist
#' @export
#' @seealso \code{\link{lapply}}
#' @family neuronlist
#' @examples
#' kcs.flipped=nlapply(kcs20,xform,reg=function(x, ...) x*c(-1,1,1) )
#' open3d()
#' plot3d(kcs20,col='red')
#' plot3d(kcs.flipped,col='green')
#' rgl.close()
nlapply<-function (X, FUN, ...){
  cl=if(is.neuronlist(X) && !inherits(X, 'neuronlistfh')) class(X) else c("neuronlist",'list')
  structure(lapply(X,FUN,...),class=cl,df=attr(X,'df'))
}

#' 3D plots of the elements in a neuronlist, optionally using a subset 
#' expression
#' 
#' @details The col and subset parameters are evaluated in the context of the 
#'   dataframe attribute of the neuronlist. If col evaluates to a factor and 
#'   colpal is a named vector then colours will be assigned by matching factor 
#'   levels against the named elements of colpal. If col evaluates to a factor 
#'   and colpal is a function then it will be used to generate colours with the 
#'   same number of levels as are used in col.
#' @param x a neuron list (where omitted will use MyNeurons as default)
#' @param subset Expression evaluating to logical mask for neurons. See details.
#' @param col An expression specifying a colour evaluated in the context of the 
#'   dataframe attached to nl (after any subsetting). See details.
#' @param colpal A vector of colours or a function that generates colours
#' @param skipRedraw When plotting more than this many (default 200) neurons 
#'   skip redraw for individual neurons (this is much faster for large number of
#'   neurons). Can also accept logical values TRUE (always skip) FALSE (never
#'   skip).
#' @param ... options passed on to plot3d (such as colours, line width etc)
#' @return list of values of \code{plot3d} with subsetted dataframe as attribute
#'   \code{'df'}
#' @export
#' @method plot3d neuronlist
#' @rdname plot3d.neuronlist
#' @examples
#' open3d()
#' plot3d(kcs20,type=='gamma',col='green')
#' clear3d()
#' plot3d(kcs20,col=type)
#' \dontrun{
#' plot3d(Cell07PNs,Glomerulus=="DA1",col='red')
#' plot3d(Cell07PNs,Glomerulus=="VA1d",col='green')
#' plot3d(Cell07PNs,Glomerulus%in%c("DA1",'VA1d'),
#'   col=c("red","green")[factor(Glomerulus)])
#' # the same but not specifying colours explicitly
#' plot3d(Cell07PNs,Glomerulus%in%c("DA1",'VA1d'),col=Glomerulus)
#' plot3d(jkn,col=sex,colpal=c(male='green',female='magenta'))
#' plot3d(jkn,col=cut(cVA2,20),colpal=jet.colors)
#' }
plot3d.neuronlist<-function(x,subset,col=NULL,colpal=rainbow,skipRedraw=200,...){
  # Handle Subset
  df=attr(x,'df')
  if(!missing(subset)){
    if(is.null(df)) stop("Can't use a subset unless neuronlist has an attached dataframe")
    # convert subset (which may a language expression) into an expression that won't get
    # evaluated until we say so
    e <- substitute(subset)
    # now evaluate it looking for variables first in the attached data frame and then 
    # in the environment of the function
    r <- eval(e, df, parent.frame())
    # match characters against neuron names
    if(is.character(r)){
      r=match(r,names(x))
      if(any(is.na(r))) {
        warning(sum(is.na(r)),' identifiers could not be',
                ' matched against names of neuronlist')
        r=na.omit(r)
      }
    }
    # check we got something back
    if((is.logical(r) && sum(r)==0) || length(r)==0){
      # no neurons left, so just return
      return()
    }
    # check that subset expression produced sensible result
    if(is.logical(r)){
      if(length(r)!=length(x)) stop("Subset result does not have same length as neuronlist x")
    } else if(is.integer(r)){
      if(any(r>length(x) | r<0)) stop("Subset evaluated to invalid integer index")
    } else stop("Subset did not evaluate to logical or character vector")
    # now just select the neurons we want
    x=x[r]
    df=df[r,]
  }
  # Handle Colours
  col.sub <- substitute(col)
  cols <- eval(col.sub, attr(x,'df'), parent.frame())
  if(!is.character(cols)){
    if(is.null(cols)) {
      if(is.function(colpal)) colpal=colpal(length(x))
      cols=colpal[seq(x)]
    }
    else if(is.function(cols)) cols=cols(length(x))
    else if(is.numeric(cols)) {
      if(is.function(colpal)) colpal=colpal(max(cols))
      cols=colpal[cols]
    }
    else if (is.factor(cols)) {
      # I think dropping missing levels is what we will always want
      cols=droplevels(cols)
      if(!is.null(names(colpal))) {
        # we have a named palette
        cols=colpal[as.character(cols)]
        if(any(is.na(cols))){
          # handle missing colours
          # first check if there is an unnamed entry in palette
          unnamed=which(names(colpal)=="")
          cols[is.na(cols)] = if(length(unnamed)) unnamed[1] else 'black'
        }
      } else {
        if(is.function(colpal)) colpal=colpal(nlevels(cols))
        cols=colpal[cols]
      }
    }
    else stop("Cannot evaluate col")
  }
  # Speed up drawing when there are lots of neurons
  if(is.numeric(skipRedraw)) skipRedraw=ifelse(length(x)>skipRedraw,TRUE,FALSE)
  if(is.logical(skipRedraw)) {
    op=par3d(skipRedraw=skipRedraw)
    on.exit(par3d(op))
  }
  rval=mapply(plot3d,x,col=cols,...)
  df=attr(x,'df')
  if(is.null(df)) {
    keys=names(x)
    if(is.null(keys)) keys=seq_along(x)
    df=data.frame(key=keys,stringsAsFactors=FALSE)
  }
  df$col=cols
  attr(rval,'df')=df
  invisible(rval)
}

#' @rdname plot3d.neuronlist
#' @method plot3d character
#' @S3method plot3d character
#' @description \code{plot3d.character} is a convenience method intended for
#'   exploratory work on the command line.
#' @details plot3d.character will check if options('nat.default.neuronlist') has
#'   been set and then use x as an identifier to find a neuron in that 
#'   neuronlist.
plot3d.character<-function(x, ...) {
  nl=get(getOption('nat.default.neuronlist'))
  if(!is.neuronlist(nl)) 
    stop("Please set options(nat.default.neuronlist='myfavneuronlist'). ',
         'See ?nat for details.")
  plot3d(nl, pmatch(x, names(nl)), ...)
}

#' Arithmetic for neuron coordinates applied to neuronlists
#'
#' If x is one number or 3-vector, multiply coordinates by that
#' If x is a 4-vector, multiply xyz and diameter
#' TODO Figure out how to document arithemtic functions in one go
#' @param x a neuronlist
#' @param y (a numeric vector to multiply coords in neuronlist members)
#' @return modified neuronlist
#' @export
#' @rdname neuronlist-arithmetic
#' @method * neuronlist
#' @examples
#' mn2<-Cell07PNs[1:10]*2
`*.neuronlist` <- function(x,y) {
  # TODO look into S3 generics for this functionality
  nlapply(x,`*`,y)
}

#' @export
#' @rdname neuronlist-arithmetic
#' @method + neuronlist
`+.neuronlist` <- function(x,y) nlapply(x,`+`,y)

#' @export
#' @rdname neuronlist-arithmetic
#' @method - neuronlist
`-.neuronlist` <- function(x,y) x+(-y)

#' @export
#' @rdname neuronlist-arithmetic
#' @method / neuronlist
`/.neuronlist` <- function(x,y) x*(1/y)

#' Methods for working with the dataframe attached to a neuronlist
#' 
#' @description \code{droplevels} Remove redundant factor levels in dataframe 
#'   attached to neuronlist
#' @inheritParams base::droplevels.data.frame
#' @S3method droplevels neuronlist
#' @name neuronlist-dataframe-methods
#' @aliases droplevels.neuronlist
#' @return the attached dataframe with levels dropped (NB \strong{not} the
#'   neuronlist)
#' @seealso droplevels
droplevels.neuronlist<-function(x, except, ...){
  droplevels(attr(x,'df'))
}

#' @description \code{with} Evaluate expression in the context of dataframe
#'   attached to a neuronlist
#' 
#' @param data A neuronlist object
#' @param expr The expression to evaluate
#' @rdname neuronlist-dataframe-methods
#' @S3method with neuronlist
#' @method with neuronlist
#' @seealso with
with.neuronlist<-function(data, expr, ...) {
  eval(substitute(expr), attr(data,'df'), enclos = parent.frame())
}

#' @description \code{head} Return the first part dataframe attached to
#'   neuronlist
#' 
#' @param x A neuronlist object
#' @param ... Further arguments passed to default methods (and usually ignored)
#' @rdname neuronlist-dataframe-methods
#' @S3method head neuronlist
#' @importFrom utils head
#' @seealso head
head.neuronlist<-function(x, ...) {
  head(attr(x,'df'), ...)
}

#' Subset neuronlist returning either new neuronlist or names of chosen neurons
#' 
#' @details 
#' The subset expression should evaluate to one of
#' \itemize{
#'   \item character vector of names
#'   \item logical vector
#'   \item vector of numeric indices
#' }
#'   Any missing names are dropped with a warning. The \code{filterfun}
#'   expression is wrapped in a try. Neurons returning an error will be dropped
#'   with a warning.
#' @param x a neuronlist
#' @param subset An expression that can be evaluated in the context of the 
#'   dataframe attached to the neuronlist. See details.
#' @param filterfun a function which can be applied to each neuron returning
#'   \code{TRUE} when that neuron should be included in the return list.
#' @param rval What to return (character vector, default='neuronlist')
#' @param ... additional arguments passed to \code{filterfun}
#' @return A \code{neuronlist}, character vector of names or the attached
#'   data.frame according to the value of \code{rval}
#' @export
#' @method subset neuronlist
#' @seealso \code{\link{neuronlist}, \link{subset.data.frame}}
#' @examples
#' da1pns=subset(Cell07PNs,Glomerulus=='DA1')
#' with(da1pns,stopifnot(all(Glomerulus=='DA1')))
#' gammas=subset(kcs20,type=='gamma')
#' with(gammas,stopifnot(all(type=='gamma')))
#' # define a function that checks whether a neuron has points in a region in 
#' # space, specifically the tip of the mushroom body alpha' lobe
#' aptip<-function(x) {xyz=xyzmatrix(x);any(xyz[,'X']>350 & xyz[,'Y']<40)}
#' # this should identify the alpha'/beta' kenyon cells only
#' apbps=subset(kcs20,filterfun=aptip)
#' # look at which neurons are present in the subsetted neuronlist
#' head(apbps)
#' # combine global variables with dataframe columns
#' odds=rep(c(TRUE,FALSE),10)
#' stopifnot(all.equal(subset(kcs20,type=='gamma' & odds),
#'             subset(kcs20,type=='gamma' & rep(c(TRUE,FALSE),10))))
#' \dontrun{
#' # make a 3d selection function using interactive rgl::select3d() function
#' s3d=select3d()
#' # Apply a 3d search function to the first 100 neurons in the neuronlist dataset
#' subset(dps[1:100],filterfun=function(x) {sum(s3d(xyzmatrix(x)))>0},
#'   rval='names')
#' # combine a search by metadata, neuropil location and 3d location
#' subset(dps, Gender=="M" & rAL>1000, function(x) sum(s3d(x))>0, rval='name')
#' # The same but specifying indices directly, which can be considerably faster
#' # when neuronlist is huge and memory is in short supply
#' subset(dps, names(dps)[1:100],filterfun=function(x) {sum(s3d(xyzmatrix(x)))>0},
#'   rval='names')
#' }
subset.neuronlist<-function(x, subset, filterfun, 
                            rval=c("neuronlist","names",'data.frame'), ...){
  rval=match.arg(rval)
  nx=names(x)
  df=attr(x,'df')
  if(missing(subset)){
    r=nx
  } else {
    # handle the subset expression by turning it into names
    e <- substitute(subset)
    r <- eval(e, df, parent.frame())
    if(is.function(r)) stop("Use of subset with functions is deprecated. ",
                            "Please use filterfun argument")
    if(is.logical(r) || is.integer(r) ){
      r=nx[r]
    } else if(is.character(r)) {
      # check against names
      missing_names=setdiff(r,nx)
      if(length(missing_names)) warning("There are ",length(missing_names),' missing names.')
      r=setdiff(r,missing_names)
    }
  }
  # now apply filterfun to remaining neurons
  if(length(r) && !missing(filterfun)) {
    filter_results=rep(NA,length(r))
    # use for loop because neuronlists are normally large but not long
    for(i in seq_along(r)){
      tf=try(filterfun(x[[r[i]]]))
      if(!inherits(tf,'try-error')) filter_results[i]=tf
    }
    r=r[filter_results]
    if(any(is.na(filter_results))) {
      warning("filterfun failed to evaluate for",sum(is.na(r)),'entries in neuronlist')
      r=na.omit(r)
    }
  }
  
  switch(rval,neuronlist=x[r],names=r,data.frame=df[r,])
}
