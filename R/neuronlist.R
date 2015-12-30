#' Create a neuronlist from zero or more neurons
#' 
#' @description \code{neuronlist} objects consist of a list of neuron objects 
#'   along with an optional attached dataframe containing information about the 
#'   neurons. \code{neuronlist} objects can be indexed using their name or the 
#'   number of the neuron like a regular list. Both the \code{list} itself and 
#'   the attached \code{data.frame} must have the same unique (row)names. If the
#'   \code{[} operator is used to index the list, the attached dataframe will 
#'   also be subsetted.
#'   
#'   It is perfectly acceptable not to pass any parameters, generating an empty 
#'   neuronlist
#' @param ... objects to be turned into a list
#' @param DATAFRAME an optional \code{data.frame} to attach to the neuronlist 
#'   containing information about each neuron.
#' @return A new neuronlist object.
#' @family neuronlist
#' @seealso \code{\link{as.data.frame.neuronlist}},
#'   \code{\link{neuronlist-dataframe-methods}}
#' @export
#' @examples
#' # generate an empty neuronlist
#' nl=neuronlist()
#' # slice an existing neuronlist with regular indexing
#' kcs5=kcs20[1:5]
#' # list all methods for neuronlist objects
#' methods(class='neuronlist')
neuronlist <- function(..., DATAFRAME=NULL) as.neuronlist(list(...), df=DATAFRAME)

#' Test objects of neuronlist class to store multiple neurons
#' 
#' Tests if object is a neuronlist.
#' 
#' @details \code{is.neuronlist} uses a relaxed definition to cope with older 
#'   lists of neurons that do not have a class attribute of neuronlist.
#' @param x the object to test
#' @return A logical indicating whether the object is a neuronlist.
#' @family neuronlist
#' @export
is.neuronlist<-function(x) {
  inherits(x,"neuronlist") ||
    (is.list(x) && length(x)>1 && is.neuron(x[[1]]))
}

#' Make a list of neurons that can be used for coordinate plotting/analysis
#' 
#' @details Note that \code{as.neuronlist} can cope with both \code{neurons} and
#'   \code{dotprops} objects but \code{AddClassToNeurons} will only apply to 
#'   things that look like neurons but don't have a class of \code{neuron}.
#'   
#'   See \code{\link{neuronlist}} details for more information.
#' @param l An existing list or a single neuron to start a list
#' @param ... Additional arguments passed to methods
#' @return neuronlist with attr('df')
#' @export
#' @seealso 
#' \code{\link{is.neuronlist}},\code{\link{is.neuron}},\code{\link{is.dotprops}}
as.neuronlist<-function(l, ...) UseMethod("as.neuronlist")

#' @export
#' @param df the data.frame to attach with additional metadata.
#' @param AddClassToNeurons Whether to ensure neurons have class \code{neuron}
#'   (see details).
#' @method as.neuronlist default
#' @rdname as.neuronlist
as.neuronlist.default<-function(l, df=NULL, AddClassToNeurons=TRUE, ...){
  if(is.neuron(l)) {
    n<-l
    l<-list(n)
    names(l)<-n$NeuronName
  }
  if(!is.null(df)) {
    if(nrow(df)!=length(l)) 
      stop("data frame must have same number of rows as there are neurons")
    attr(l,"df")=df
    # null or empty string names
    if(is.null(names(l)) || all(!nzchar(names(l))))
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
#' @description \code{[.neuronlist} and \code{[<-.neuronlist} behave like the 
#'   corresponding base methods (\code{[.data.frame}, \code{[<-.data.frame}) 
#'   allowing extraction or replacement of parts of the data.frame attached to
#'   the neuronlist.
#' @export
#' @param i,j elements to extract or replace. Numeric or character or, for [ 
#'   only, empty. Numeric values are coerced to integer as if by 
#'   \code{as.integer}. See \code{\link{[.data.frame}} for details.
#' @inheritParams base::`[.data.frame`
#' @name neuronlist-dataframe-methods
#' @seealso \code{\link{[.data.frame}}, @seealso \code{\link{[<-.data.frame}}
#' @family neuronlist
#' @examples
#' ## treat kcs20 as data.frame
#' kcs20[1, ]
#' kcs20[1:3, ]
#' kcs20[, 1:4]
#' kcs20[, 'soma_side']
#' # alternative to as.data.frame(kcs20)
#' kcs20[, ]
#' 
#' ## can also set columns
#' kcs13=kcs20[1:3]
#' kcs13[,'side']=as.character(kcs13[,'soma_side'])
#' head(kcs13)
#' # or parts of columns
#' kcs13[1,'soma_side']='R'
#' kcs13['FruMARCM-M001205_seg002','soma_side']='L'
#' # remove a column
#' kcs13[,'side']=NULL
#' all.equal(kcs13, kcs20[1:3])
#' 
#' # can even replace the whole data.frame like this
#' kcs13[,]=kcs13[,]
#' all.equal(kcs13, kcs20[1:3])
#' 
#' ## get row/column names of attached data.frame 
#' # (unfortunately implementing ncol/nrow is challenging)
#' rownames(kcs20)
#' colnames(kcs20)
"[.neuronlist" <- function(x, i, j, drop) {
  
  # x[1] => 2 args
  # x[1, ] or x[, 1] => 3 args
  if(nargs()>2) {
    # treat as data.frame
    df=as.data.frame(x)
    if(missing(drop)) return(df[i, j])
    else return(df[i, j, drop=drop])
  }
  # treat as neuronlist
  nl2=structure(NextMethod("["), class = class(x))
  df=attr(x,'df')
  if(!is.null(df)){
    df=df[i, ]
  }
  attr(nl2,'df')=df
  nl2
}

#' @export
#' @rdname neuronlist-dataframe-methods
"[<-.neuronlist" <- function(x, i, j, value) {
  if(nargs()<4) return(NextMethod())
  # special case if we are replacing the whole 
  if(missing(i) && missing(j)) {
    # `data.frame<-.neuronlist` is supposed to return the neuronlist but 
    # actually returns the data.frame for reasons that I haven't yet fathomed
    data.frame(x)<-value
    return(x)
  }
  df=as.data.frame(x)
  df[i,j]=value
  attr(x,'df')=df
  x
}

#' @export
dimnames.neuronlist<-function(x) {
  # this only makes sense for the attached df
  dimnames(as.data.frame(x))
}

#' Combine multiple neuronlists into a single list
#' 
#' @details Uses \code{\link[plyr]{rbind.fill}} to join any attached dataframes,
#'   so missing values are replaced with NAs.
#' @param ... neuronlists to combine
#' @param recursive Presently ignored
#' @export
#' @seealso \code{\link[base]{c}}
#' @examples
#' stopifnot(all.equal(kcs20[1:2],c(kcs20[1],kcs20[2])))
c.neuronlist<-function(..., recursive = FALSE){
  args=list(...)
  if(!all(sapply(args, inherits, "neuronlist")))
    stop("method only applicable to multiple neuronlist objects")
  
  neuron_names=unlist(lapply(args, names))
  if(any(duplicated(neuron_names))) 
    stop("Cannot join neuronlists containing neurons with the same name!")
  
  old.dfs=lapply(args, attr, 'df')
  null_dfs=sapply(old.dfs, is.null)
  if(any(null_dfs) && !all(null_dfs)){
    # we need to ensure that the eventual data.frame has suitable rownames
    # so add dummy empty data.frame with appropriate rownames
    old.dfs[null_dfs]=lapply(args[null_dfs],
                             function(x) data.frame(row.names = names(x)))
  }
  new.df=plyr::rbind.fill(old.dfs)
  # rbind.fill doesn't seem to look after rownames
  if(!is.null(new.df))
    rownames(new.df)=neuron_names

  as.neuronlist(NextMethod(args), df = new.df)
}

#' Get or set the attached data.frame of a neuronlist
#' 
#' For \code{as.data.frame}, when there is no attached data.frame the result 
#' will be a data.frame with 0 columns but an appropriate number of rows, named by
#' the objects in the neuronlist.
#' 
#' @param x neuronlist to convert
#' @param row.names row names (defaults to names of objects in neuronlist, which
#'   is nearly always what you want.)
#' @param optional ignored in this method
#' @param ... additional arguments passed to \code{\link{data.frame}} (see 
#'   examples)
#' @export
#' @return for \code{as.data.frame.neuronlist}, a \code{data.frame} with
#'   length(x) rows, named according to names(x) and containing the columns from
#'   the attached data.frame, when present.
#' @seealso \code{\link{data.frame}}, \code{\link{neuronlist}}
#' @rdname get-set-neuronlist-data.frame
#' @examples 
#' head(as.data.frame(kcs20))
#' 
#' # add additional variables
#' str(as.data.frame(kcs20, i=seq(kcs20), abc=LETTERS[seq(kcs20)]))
#' # stop character columns being turned into factors
#' newdf <- as.data.frame(kcs20, i=seq(kcs20), abc=LETTERS[seq(kcs20)], 
#'   stringsAsFactors=FALSE)
#' str(newdf)
#' data.frame(kcs20)=newdf
as.data.frame.neuronlist<-function(x, row.names = names(x), optional = FALSE, ...) {
  df=attr(x, 'df')
  if(is.null(df)) {
    data.frame(row.names = row.names, ...)
  } else {
    data.frame(df, row.names = row.names, ...)
  }
}

#' @description \code{data.frame<-} methods set the data frame attached to an 
#'   object. At present this is only used for neuronlist objects.
#' @param value The new data.frame to be attached to \code{x}
#' @rdname get-set-neuronlist-data.frame
#' @export
`data.frame<-`<-function(x, value) UseMethod("data.frame<-")

#' @return for \code{data.frame<-.neuronlist}, a neuronlist with the attached
#'   data.frame.
#' @rdname get-set-neuronlist-data.frame
#' @export
`data.frame<-.neuronlist`<-function(x, value) {
  if(is.null(value)){
    # already null, so no change
    if(is.null(attr(x, 'df'))) return(x)
    # already exists but now want to set to NULL
    attr(x,'df')=NULL
    return(x)
  }
  nn=names(x)
  matching_rows=intersect(nn, rownames(value))
  if(length(matching_rows)){
    missing_rows=setdiff(nn, matching_rows)
    if(length(missing_rows))
      stop("Some neurons are not recorded in data.frame: ",
           paste(missing_rows, collapse=" "))
    missing_neurons=setdiff(matching_rows, nn)
    if(length(missing_neurons))
      warning(length(missing_neurons),
              " rows in data.frame do not have a matching neuron.")
  } else {
    stop("data.frame rownames do not match neuron names.")
  }
  if(!isTRUE(all.equal(rownames(value), matching_rows))) {
    # we need to reorder the rows and/or subset the incoming data.frame
    # we do it like this so that we don't touch the incoming data.frame
    # unless strictly necessary
    value=value[matching_rows,]
  }
  attr(x,'df')=value
  x
}

#' lapply and mapply for neuronlists (with optional parallelisation)
#' 
#' @description versions of lapply and mapply that look after the class and 
#'   attached dataframe of neuronlist objects. \code{nlapply} can apply a 
#'   function to only a \code{subset} of elements in the input neuronlist. 
#'   Internally \code{nlapply} uses \code{plyr::llply} thereby enabling progress
#'   bars and simple parallelisation (see plyr section and examples).
#'   
#' @details When \code{OmitFailures} is not \code{NA}, \code{FUN} will be 
#'   wrapped in a call to \code{try} to ensure that failure for any single 
#'   neuron does not abort the nlapply/nmapply call. When 
#'   \code{OmitFailures=TRUE} the resultant neuronlist will be subsetted down to
#'   return values for which \code{FUN} evaluated successfully. When 
#'   \code{OmitFailures=FALSE}, "try-error" objects will be left in place. In 
#'   either of the last 2 cases error messages will not be printed because the 
#'   call is wrapped as \code{try(expr, silent=TRUE)}.
#'   
#' @section plyr: The arguments of most interest from plyr are:
#'   
#'   \itemize{
#'   
#'   \item \code{.inform} set to \code{TRUE} to give more informative error 
#'   messages that should indicate which neurons are failing for a given applied
#'   function.
#'   
#'   \item \code{.progress} set to \code{"text"} for a basic progress bar
#'   
#'   \item \code{.parallel} set to \code{TRUE} for parallelisation after
#'   registering a parallel backend (see below).
#'   
#'   \item \code{.paropts} Additional arguments for parallel computation. See 
#'   \code{\link[plyr]{llply}} for details.
#'   
#'   }
#'   
#'   Before using parallel code within an R session you must register a suitable
#'   parallel backend. The simplest example is the multicore option provided by 
#'   the \code{doMC} package that is suitable for a spreading computational load
#'   across multiple cores on a single machine. An example is provided below.
#'   
#'   Note that the progess bar and parallel options cannot be used at the same 
#'   time. You may want to start a potentially long-running job with the 
#'   progress bar option and then abort and re-run with \code{.parallel=TRUE} if
#'   it looks likely to take a very long time.
#'   
#' @param X A neuronlist
#' @param FUN Function to be applied to each element of X
#' @param ... Additional arguments for FUN (see details)
#' @param subset Character, numeric or logical vector specifying on which subset
#'   of \code{X} the function \code{FUN} should be applied. Elements outside the
#'   subset are passed through unmodified.
#' @param OmitFailures Whether to omit neurons for which \code{FUN} gives an 
#'   error. The default value (\code{NA}) will result in nlapply stopping with 
#'   an error message the moment there is an eror. For other values, see 
#'   details.
#' @param .progress Character vector specifying the type of progress bar (see 
#'   \code{\link[plyr]{create_progress_bar}} for options.) The default value of 
#'   \code{"auto"} shows a progress bar in interactive use when there are >=10
#'   elements in \code{X}.
#' @return A neuronlist
#' @export
#' @seealso \code{\link{lapply}}
#' @family neuronlist
#' @examples
#' ## nlapply example
#' kcs.reduced=nlapply(kcs20,function(x) subset(x,sample(nrow(x$points),50)))
#' open3d()
#' plot3d(kcs.reduced,col='red', lwd=2)
#' plot3d(kcs20,col='grey')
#' rgl.close()
#' 
#' \dontrun{
#' # example of using plyr's .inform argument for debugging error conditions
#' xx=nlapply(Cell07PNs, prune_strahler)
#' # oh dear there was an error, let's get some details about the neuron
#' # that caused the problem
#' xx=nlapply(Cell07PNs, prune_strahler, .inform=TRUE)
#' }
#' 
#' \dontrun{
#' ## nlapply example with plyr
#' ## dotprops.neuronlist uses nlapply under the hood
#' ## the .progress and .parallel arguments are passed straight to 
#' system.time(d1<-dotprops(kcs20,resample=1,k=5,.progress='text'))
#' ## plyr+parallel
#' library(doMC)
#' # can also specify cores e.g. registerDoMC(cores=4)
#' registerDoMC()
#' system.time(d2<-dotprops(kcs20,resample=1,k=5,.parallel=TRUE))
#' stopifnot(all.equal(d1,d2))
#' }
#' 
#' ## nmapply example
#' # flip first neuron in X, second in Y and 3rd in Z
#' xyzflip=nmapply(mirror, kcs20[1:3], mirrorAxis = c("X","Y","Z"),
#'  mirrorAxisSize=c(400,20,30))
#' open3d()
#' plot3d(kcs20[1:3])
#' plot3d(xyzflip)
#' rgl.close()
nlapply<-function (X, FUN, ..., subset=NULL, OmitFailures=NA, .progress='auto'){
  
  if(.progress=='auto') {
    if(length(X)>=10 && interactive()) .progress="text"
    else .progress="none"
  }
  cl=if(is.neuronlist(X) && !is.neuronlistfh(X)) class(X) 
  else c("neuronlist", 'list')
  
  if(!is.null(subset)){
    if(!is.character(subset)) subset=names(X)[subset]
    Y=X
    X=X[subset]
  }
  TFUN = if(is.na(OmitFailures)) FUN 
  else function(...) try(FUN(...), silent=TRUE)
  rval=structure(plyr::llply(X, TFUN, ..., .progress=.progress), class=cl, df=attr(X, 'df'))
  
  if(isTRUE(OmitFailures))
    failures=sapply(rval, inherits, 'try-error')
    
  if(is.null(subset)){
    if(isTRUE(OmitFailures) && any(failures)) rval[!failures]
    else rval
  } else {
    if(isTRUE(OmitFailures) && any(failures)){
      Y[subset[!failures]]=rval[!failures]
      Y=Y[setdiff(names(Y),subset[failures])]
    } else {
      Y[subset]=rval
    }
    Y
  }
}

#' @inheritParams base::mapply
#' @rdname nlapply
#' @seealso \code{\link{mapply}}
#' @export
nmapply<-function(FUN, X, ..., MoreArgs = NULL, SIMPLIFY = FALSE,
                  USE.NAMES = TRUE, subset=NULL, OmitFailures=NA){
  if(!is.neuronlist(X))
    stop("X must be a neuronlist!")
  cl=if(is.neuronlist(X) && !is.neuronlistfh(X)) class(X)
  else c("neuronlist",'list')
  if(!is.null(subset)){
    if(!is.character(subset)) subset=names(X)[subset]
    Y=X
    X=X[subset]
  }
  
  TFUN = if(is.na(OmitFailures)) FUN else function(...) try(FUN(...), silent=TRUE)
  rval=structure(mapply(TFUN, X, ..., MoreArgs = MoreArgs, SIMPLIFY = SIMPLIFY,
                        USE.NAMES = USE.NAMES), class=cl, df=attr(X, 'df'))
  if(isTRUE(OmitFailures))
    failures=sapply(rval, inherits, 'try-error')

  if(is.null(subset)){
    if(isTRUE(OmitFailures) && any(failures)) rval[!failures]
    else rval
  } else {
    if(isTRUE(OmitFailures) && any(failures)){
      Y[subset[!failures]]=rval[!failures]
      Y=Y[setdiff(names(Y),subset[failures])]
    } else {
      Y[subset]=rval
    }
    Y
  }
}

#' 3D plots of the elements in a neuronlist, optionally using a subset 
#' expression
#' 
#' @details The col and subset parameters are evaluated in the context of the 
#'   dataframe attribute of the neuronlist. If col evaluates to a factor and 
#'   colpal is a named vector then colours will be assigned by matching factor 
#'   levels against the named elements of colpal. If there is one unnamed level,
#'   this will be used as catch-all default value (see examples).
#'   
#'   If col evaluates to a factor and colpal is a function then it will be used 
#'   to generate colours with the same number of levels as are used in col.
#'   
#'   WithNodes is \code{FALSE} by default when using \code{plot3d.neuronlist} 
#'   but remains \code{TRUE} by default when plotting single neurons with 
#'   \code{\link{plot3d.neuron}}. This is because the nodes quickly make plots 
#'   with multiple neurons rather busy.
#'   
#'   When \code{soma} is \code{TRUE} or a vector of numeric values (recycled as 
#'   appropriate), the values are used to plot cell bodies. For neurons the 
#'   values are passed to \code{plot3d.neuron} for neurons. In contrast 
#'   \code{dotprops} objects still need special handling. There must be columns 
#'   called \code{X,Y,Z} in the data.frame attached to \code{x}, that are then 
#'   used directly by code in \code{plot3d.neuronlist}.
#'   
#'   Whenever plot3d.neuronlist is called, it will add an entry to an 
#'   environment \code{.plotted3d} in \code{nat} that stores the ids of all the 
#'   plotted shapes (neurons, cell bodies) so that they can then be removed by a
#'   call to \code{npop3d}.
#' @param x a neuron list or, for \code{plot3d.character}, a character vector of
#'   neuron names. The default neuronlist used by plot3d.character can be set by
#'   using \code{options(nat.default.neuronlist='mylist')}. See 
#'   ?\code{\link{nat}} for details. \code{\link{nat-package}}.
#' @param subset Expression evaluating to logical mask for neurons. See details.
#' @param col An expression specifying a colour evaluated in the context of the 
#'   dataframe attached to nl (after any subsetting). See details.
#' @param colpal A vector of colours or a function that generates colours
#' @param skipRedraw When plotting more than this many (default 200) neurons 
#'   skip redraw for individual neurons (this is much faster for large number of
#'   neurons). Can also accept logical values TRUE (always skip) FALSE (never 
#'   skip).
#' @param WithNodes Whether to plot points for end/branch points. Default: 
#'   \code{FALSE}.
#' @param ... options passed on to plot3d (such as colours, line width etc)
#' @param SUBSTITUTE Whether to \code{substitute} the expressions passed as 
#'   arguments \code{subset} and \code{col}. Default: \code{TRUE}. For expert 
#'   use only, when calling from another function.
#' @inheritParams plot3d.neuron
#' @return list of values of \code{plot3d} with subsetted dataframe as attribute
#'   \code{'df'}
#' @export
#' @method plot3d neuronlist
#' @seealso \code{\link{nat-package}}
#' @examples
#' open3d()
#' plot3d(kcs20,type=='gamma',col='green')
#' clear3d()
#' plot3d(kcs20,col=type)
#' \dontrun{
#' plot3d(Cell07PNs,Glomerulus=="DA1",col='red')
#' plot3d(Cell07PNs,Glomerulus=="VA1d",col='green')
#' # Note use of default colour for non DA1 neurons
#' plot3d(Cell07PNs,col=Glomerulus, colpal=c(DA1='red', 'grey'))
#' # a subset expression
#' plot3d(Cell07PNs,Glomerulus%in%c("DA1",'VA1d'),
#'   col=c("red","green")[factor(Glomerulus)])
#' # the same but not specifying colours explicitly
#' plot3d(Cell07PNs,Glomerulus%in%c("DA1",'VA1d'),col=Glomerulus)
#' 
#' ## more complex colouring strategies for a larger neuron set
#' # see https://github.com/jefferis/frulhns for details
#' library(frulhns)
#' # notice the sexually dimorphic projection patterns for these neurons
#' plot3d(jkn,cluster=='aSP-f' &shortGenotype=='JK1029', 
#'   col=sex,colpal=c(male='green',female='magenta'))
#'
#' ## colour neurons of a class by input resistance
#' jkn.aspg=subset(jkn, cluster=='aSP-g')
#' # NB this comes in as a factor
#' Ri=with(jkn.aspg,as.numeric(as.character(Ri..GOhm.)))
#' # the matlab jet palette
#' jet.colors<-colorRampPalette(c('navy','cyan','yellow','red'))
#' plot3d(jkn.aspg,col=cut(Ri,20),colpal=jet.colors)
#' }
plot3d.neuronlist<-function(x, subset=NULL, col=NULL, colpal=rainbow, skipRedraw=200,
                            WithNodes=FALSE, soma=FALSE, ..., SUBSTITUTE=TRUE){
  # Handle Subset
  if(!missing(subset)){
    # handle the subset expression - we still need to evaluate right away to
    # avoid tricky issues with parent.frame etc, but we can then leave 
    # subset.neuronlist to do the rest of the work
    e <- if(SUBSTITUTE) substitute(subset) else subset
    r <- eval(e, attr(x,'df'), parent.frame())
    x <- subset.neuronlist(x, r)
  }
  
  # Handle Colours
  col.sub <- if(SUBSTITUTE) substitute(col) else col
  cols <- eval(col.sub, attr(x,'df'), parent.frame())
  cols=makecols(cols, colpal, length(x))
  
  # Speed up drawing when there are lots of neurons
  if(is.numeric(skipRedraw)) skipRedraw=ifelse(length(x)>skipRedraw,TRUE,FALSE)
  if(is.logical(skipRedraw)) {
    if(par3d()$skipRedraw) skipRedraw=TRUE
    op=par3d(skipRedraw=skipRedraw)
    on.exit(par3d(op))
  }
  
  rval=mapply(plot3d,x,col=cols,soma=soma,..., MoreArgs = list(WithNodes=WithNodes),
              SIMPLIFY=FALSE)
  df=as.data.frame(x)
  if( (length(soma)>1 || soma) && isTRUE(is.dotprops(x[[1]])) &&
               all(c("X","Y","Z") %in% colnames(df))){
    if(is.logical(soma)) soma=2
    rval <- c(rval, spheres3d(df[, c("X", "Y", "Z")], radius = soma, col = cols))
  }
  assign(".last.plot3d", rval, envir=.plotted3d)
  df$col=cols
  attr(rval,'df')=df
  invisible(rval)
}

#' @rdname plot3d.neuronlist
#' @method plot3d character
#' @export
#' @param db A neuronlist to use as the source of objects to plot. If
#'   \code{NULL}, the default, will use the neuronlist specified by
#'   options('nat.default.neuronlist')
#' @description \code{plot3d.character} is a convenience method intended for 
#'   exploratory work on the command line.
#' @details plot3d.character will check if options('nat.default.neuronlist') has
#'   been set and then use x as an identifier to find a neuron in that 
#'   neuronlist.
plot3d.character<-function(x, db=NULL, ...) {
  if(is.null(db))
    db=get(getOption('nat.default.neuronlist',
                     default=stop('Option "nat.default.neuronlist" is not set.",
                                  " See ?nat for details.')))
  if(!is.neuronlist(db)) 
    stop("Please set options(nat.default.neuronlist='myfavneuronlist'). ',
         'See ?nat for details.")
  plot3d(db, pmatch(x, names(db)), ...)
}

#' 2D plots of the elements in a neuronlist, optionally using a subset 
#' expression
#' 
#' @details The col and subset parameters are evaluated in the context of the 
#'   dataframe attribute of the neuronlist. If col evaluates to a factor and 
#'   colpal is a named vector then colours will be assigned by matching factor 
#'   levels against the named elements of colpal. If there is one unnamed level,
#'   this will be used as catch-all default value (see examples).
#'   
#'   If col evaluates to a factor and colpal is a function then it will be used
#'   to generate colours with the same number of levels as are used in col.
#' @param x a neuron list or, for \code{plot3d.character}, a character vector of
#'   neuron names. The default neuronlist used by plot3d.character can be set by
#'   using \code{options(nat.default.neuronlist='mylist')}. See 
#'   ?\code{\link{nat}} for details.
#' @param col An expression specifying a colour evaluated in the context of the 
#'   dataframe attached to nl (after any subsetting). See details.
#' @param add Logical specifying whether to add data to an existing plot or make
#'   a new one. The default value of \code{NULL} creates a new plot with the 
#'   first neuron in the neuronlist and then adds the remaining neurons.
#' @param ... options passed on to plot (such as colours, line width etc)
#' @inheritParams plot3d.neuronlist
#' @inheritParams plot.neuron
#' @return list of values of \code{plot} with subsetted dataframe as attribute 
#'   \code{'df'}
#' @export
#' @seealso \code{\link{nat-package}, \link{plot3d.neuronlist}}
#' @examples
#' # plot 4 cells
#' plot(Cell07PNs[1:4])
#' # modify some default plot arguments
#' plot(Cell07PNs[1:4], ylim=c(140,75), main='First 4 neurons')
#' # plot one class of neurons in red and all the others in grey
#' plot(Cell07PNs, col=Glomerulus, colpal=c(DA1='red', 'grey'), WithNodes=FALSE)
#' # subset operation
#' plot(Cell07PNs, subset=Glomerulus%in%c("DA1", "DP1m"), col=Glomerulus,
#'   ylim=c(140,75), WithNodes=FALSE)
plot.neuronlist<-function(x, subset=NULL, col=NULL, colpal=rainbow, add=NULL, 
                          boundingbox=NULL, ..., SUBSTITUTE=TRUE){
  # Handle Subset
  if(!missing(subset)){
    # handle the subset expression - we still need to evaluate right away to
    # avoid tricky issues with parent.frame etc, but we can then leave 
    # subset.neuronlist to do the rest of the work
    e <- if(SUBSTITUTE) substitute(subset) else subset
    r <- eval(e, attr(x,'df'), parent.frame())
    x <- subset.neuronlist(x, r)
  }
  
  # Handle Colours
  col.sub <- if(SUBSTITUTE) substitute(col) else col
  cols <- eval(col.sub, attr(x,'df'), parent.frame())
  cols=makecols(cols, colpal, length(x))
  
  if(is.null(add)){
    add=c(FALSE, rep(TRUE, length(x)-1))
  }
  
  # check bounding box for data
  if(is.null(boundingbox)) boundingbox=boundingbox(x, na.rm=T)
  rval=mapply(plot, x, col=cols, add=add, 
              MoreArgs = list(boundingbox=boundingbox, ...), SIMPLIFY = F)
  
  df=as.data.frame(x)
  df$col=cols
  attr(rval,'df')=df
  invisible(rval)
}

# internal utility function to handle colours for plot(3D).neuronlist
# see plot3d.neuronlist for details
# @param nitems Number of items for which colours must be made
makecols<-function(cols, colpal, nitems) {
  if(!is.character(cols)){
    if(is.null(cols)) {
      if(is.function(colpal)) colpal=colpal(nitems)
      cols=colpal[seq_len(nitems)]
    }
    else if(is.function(cols)) cols=cols(nitems)
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
          cols[is.na(cols)] = if(length(unnamed)) colpal[unnamed[1]] else 'black'
        }
      } else {
        if(is.function(colpal)) colpal=colpal(nlevels(cols))
        cols=colpal[cols]
      }
    }
    else stop("Cannot evaluate col")
  }
  cols
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
#' @family neuronlist
`+.neuronlist` <- function(x,y) nlapply(x,`+`,y)

#' @export
#' @rdname neuronlist-arithmetic
#' @method - neuronlist
`-.neuronlist` <- function(x,y) {
  if(missing(y)) x*-1
  else x+(-y)
}

#' @export
#' @rdname neuronlist-arithmetic
#' @method / neuronlist
`/.neuronlist` <- function(x,y) x*(1/y)

#' Methods for working with the dataframe attached to a neuronlist
#' 
#' @description \code{droplevels} Remove redundant factor levels in dataframe 
#'   attached to neuronlist
#' @inheritParams base::droplevels.data.frame
#' @export
#' @name neuronlist-dataframe-methods
#' @aliases droplevels.neuronlist droplevels
#' @return the attached dataframe with levels dropped (NB \strong{not} the
#'   neuronlist)
#' @seealso \code{\link{droplevels}}
droplevels.neuronlist<-function(x, except=NULL, ...){
  droplevels(attr(x,'df'), except, ...)
}

#' @description \code{with} Evaluate expression in the context of dataframe
#'   attached to a neuronlist
#' 
#' @param data A neuronlist object
#' @param expr The expression to evaluate
#' @rdname neuronlist-dataframe-methods
#' @aliases with.neuronlist with
#' @export
#' @method with neuronlist
#' @seealso \code{\link{with}}
with.neuronlist<-function(data, expr, ...) {
  eval(substitute(expr), attr(data,'df'), enclos = parent.frame())
}

#' @description \code{head} Return the first part of data.frame attached to
#'   neuronlist
#' 
#' @param x A neuronlist object
#' @param ... Further arguments passed to default methods (and usually ignored)
#' @rdname neuronlist-dataframe-methods
#' @aliases head.neuronlist head
#' @export
#' @seealso \code{\link{head}}
head.neuronlist<-function(x, ...) {
  head(as.data.frame(x), ...)
}

#' @description \code{tail} Return the last part of data.frame attached to 
#'   neuronlist
#'   
#' @rdname neuronlist-dataframe-methods
#' @aliases tail.neuronlist tail
#' @export
#' @seealso \code{\link{tail}}
tail.neuronlist<-function(x, ...) {
  tail(as.data.frame(x), ...)
}

#' Subset neuronlist returning either new neuronlist or names of chosen neurons
#' 
#' @details The subset expression should evaluate to one of \itemize{
#'   
#'   \item character vector of names
#'   
#'   \item logical vector
#'   
#'   \item vector of numeric indices
#'   
#'   }
#'   
#'   Any missing names are dropped with a warning. The \code{filterfun}
#'   expression is wrapped in a try. Neurons returning an error will be dropped
#'   with a warning.
#'   
#'   You may also be interested in \code{\link{find.neuron}}, which enables 
#'   objects in a neuronlist to be subsetted by a 3D selection box. In addition 
#'   \code{\link{subset.neuron}}, \code{\link{subset.dotprops}} methods exist: 
#'   these are used to remove points from neurons (rather than to remove neurons
#'   from neuronlists).
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
#' @seealso \code{\link{neuronlist}, \link{find.neuron}, 
#'   \link{subset.data.frame}, \link{subset.neuron}, \link{subset.dotprops}}
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
#' # make a 3D selection function using interactive rgl::select3d() function
#' s3d=select3d()
#' # Apply a 3D search function to the first 100 neurons in the neuronlist dataset
#' subset(dps[1:100],filterfun=function(x) {sum(s3d(xyzmatrix(x)))>0},
#'   rval='names')
#' # combine a search by metadata, neuropil location and 3D location
#' subset(dps, Gender=="M" & rAL>1000, function(x) sum(s3d(x))>0, rval='name')
#' # The same but specifying indices directly, which can be considerably faster
#' # when neuronlist is huge and memory is in short supply
#' subset(dps, names(dps)[1:100],filterfun=function(x) {sum(s3d(xyzmatrix(x)))>0},
#'   rval='names')
#' }
#' @importFrom stats na.omit
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
    if(is.null(r)){
      r=nx
    } else if(is.logical(r)){
      r=nx[r & !is.na(r)]
    } else if(is.numeric(r)){
      r=nx[na.omit(r)]
    } else if(is.character(r)) {
      # check against names
      missing_names=setdiff(r, nx)
      if(length(missing_names))
        warning("There are ",length(missing_names),' missing names.')
      r=setdiff(r, missing_names)
    }
  }
  # now apply filterfun to remaining neurons
  if(length(r) && !missing(filterfun)) {
    filter_results=rep(NA, length(r))
    # use for loop because neuronlists are normally large but not long
    for(i in seq_along(r)){
      tf=try(filterfun(x[[r[i]]], ...))
      if(!inherits(tf, 'try-error')) filter_results[i]=tf
    }
    r=r[filter_results]
    if(any(is.na(filter_results))) {
      warning("filterfun failed to evaluate for ", sum(is.na(r)),
              ' entries in neuronlist')
      r=na.omit(r)
    }
  }
  
  switch(rval, neuronlist=x[r], names=r, data.frame=df[r, ])
}
