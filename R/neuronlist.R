#' Create and test objects of neuronlist class to store multiple neurons
#'  
#' @description \code{neuronlist} objects consist of a list of neuron objects along with an optional
#' attached dataframe containing information about the neurons. Relevant
#' functions include
#' plot3d.neuronlist
#' write.neuronlist
#' subset.neuronlist
#' read.neurons can generate a neuronlist 
#' @rdname neuronlist
#' @family neuronlist
#' @description \code{is.neuronlist} Test if object is a neuronlist
#'
#' @details  \code{is.neuronlist} Uses a relaxed definition to cope with older lists of neurons that do not
#' have a class attribute of neuronlist
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
#' @return return value
#' @export
#' @examples
#' # generate an empty neuronlist
#' nl=neuronlist()
neuronlist <- function(...) as.neuronlist(list(...))

#' Make a list of neurons that can be used for coordinate plotting/analysis
#'
#' Note that it can cope with both neurons and dotprops but AddClassToNeurons
#' parameter will only apply to things that look like neurons but don't have
#' a class of neuron.
#' @param l An existing list or a single neuron to start a list
#' @param df A dataframe with one row of information per neuron
#' @param AddClassToNeurons make sure that list elements have class neuron.
#' @return neuronlist with attr('df')
#' @export
#' @seealso \code{\link{is.neuronlist}},\code{\link{is.neuron}},\code{\link{is.dotprops}}
as.neuronlist<-function(l,df,AddClassToNeurons=TRUE){
  if(is.neuron(l)) {
    n<-l
    l<-list(n)
    names(l)<-n$NeuronName
  }
  if(!missing(df)) {
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
