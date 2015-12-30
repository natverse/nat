#' Find neurons within a 3D selection box (usually drawn in rgl window)
#' 
#' @details Uses \code{\link{subset.neuronlist}}, so can work on dotprops or 
#'   neuron lists.
#' @param sel3dfun A \code{\link{select3d}} style function to indicate if points
#'   are within region
#' @param indices Names of neurons to search (defaults to all neurons in list)
#' @param db \code{neuronlist} to search. Can also be a character vector naming 
#'   the neuronlist. Defaults to \code{options('nat.default.neuronlist')}.
#' @param threshold More than this many points must be present in region
#' @param invert Whether to return neurons outside the selection box (default 
#'   \code{FALSE})
#' @param rval What to return (character vector, default='names')
#'   
#' @return Character vector of names of selected neurons, neuronlist, or 
#'   data.frame of attached metadata according to the value of \code{rval}.
#' @export
#' @seealso \code{\link{select3d}, \link{find.soma}, \link{subset.neuronlist}}
#' @examples
#' \dontrun{
#' plot3d(kcs20)
#' # draw a 3D selection e.g. around tip of vertical lobe when ready
#' find.neuron(db=kcs20)
#' # would return 9 neurons
#' # make a standalone selection function
#' vertical_lobe=select3d()
#' find.neuron(vertical_lobe, db=kcs20)
#' # use base::Negate function to invert the selection function 
#' # i.e. choose neurons that do not overlap the selection region
#' find.neuron(Negate(vertical_lobe), db=kcs20)
#' }
find.neuron<-function(sel3dfun=select3d(), indices=names(db), 
                      db=getOption("nat.default.neuronlist"), threshold=0,
                      invert=FALSE, rval=c("names",'data.frame',"neuronlist")){
  
  if(is.null(db))
    stop("Please pass a neuronlist in argument db or set options",
         "(nat.default.neuronlist='myfavneuronlist'). See ?nat for details.")
  if(is.character(db)) db=get(db)
  selfun=function(x){
    pointsinside=sel3dfun(na.omit(xyzmatrix(x)))
    sum(pointsinside, na.rm=T)>threshold
  }
  if(invert) selfun=Negate(selfun)
  rval=match.arg(rval)
  subset(db, subset=indices, filterfun=selfun, rval=rval)
}

#' Find neurons with soma inside 3D selection box (usually drawn in rgl window)
#' 
#' @details Can work on \code{neuronlist}s containing \code{neuron} objects 
#'   \emph{or} \code{neuronlist}s whose attached data.frame contains soma 
#'   positions specified in columns called X,Y,Z  .
#' @inheritParams find.neuron
#' @return Character vector of names of selected neurons
#' @export
#' @seealso \code{\link{select3d}, \link{subset.neuronlist}, \link{find.neuron}}
find.soma <- function (sel3dfun = select3d(), indices = names(db), 
                       db = getOption("nat.default.neuronlist"),
                       invert=FALSE,
                       rval=c("names", "neuronlist", "data.frame"))
{
  if (is.null(db)) 
    stop("Please pass a neuronlist in argument db or set options", 
         "(nat.default.neuronlist='myfavneuronlist'). See ?nat for details.")
  if (is.character(db)) 
    db = get(db)
  df=attr(db, 'df')
  rval=match.arg(rval)
  if(all(c("X","Y","Z") %in% colnames(df))){
    # assume these represent soma position
    somapos=df[indices,c("X","Y","Z")]
    sel_neurons=indices[sel3dfun(somapos)]
    if(invert) sel_neurons=setdiff(indices, sel_neurons)
    if(rval=='names') sel_neurons else subset(db, indices=sel_neurons, rval=rval)
  } else {
    selfun = function(x) {
      somapos=x$d[x$StartPoint, c("X","Y","Z")]
      isTRUE(sel3dfun(somapos))
    }
    if(invert) selfun=Negate(selfun)
    subset(db, subset = indices, filterfun = selfun, rval = rval)
  }
}


#' Scan through a set of neurons, individually plotting each one in 3D
#' 
#' Can also choose to select specific neurons along the way and navigate 
#' forwards and backwards.
#' 
#' @param neurons a \code{neuronlist} object or a character vector of names of 
#'   neurons to plot from the neuronlist specified by \code{db}.
#' @inheritParams plot3d.character
#' @param col the color with which to plot the neurons (default \code{'red'}).
#' @param Verbose logical indicating that info about each selected neuron should
#'   be printed (default \code{TRUE}).
#' @param Wait logical indicating that there should be a pause between each 
#'   displayed neuron.
#' @param sleep time to pause between each displayed neuron when 
#'   \code{Wait=TRUE}.
#' @param extrafun an optional function called when each neuron is plotted, with
#'   two arguments: the current neuron name and the current \code{selected} 
#'   neurons.
#' @param selected_file an optional path to a \code{yaml} file that already 
#'   contains a selection.
#' @param selected_col the color in which selected neurons (such as those 
#'   specified in \code{selected_file}) should be plotted.
#' @param yaml a logical indicating that selections should be saved to disk in 
#'   (human-readable) \code{yaml} rather than (machine-readable) \code{rda} 
#'   format.
#' @param ... extra arguments to pass to \code{\link{plot3d}}.
#'   
#' @return A character vector of names of any selected neurons, of length 0 if 
#'   none selected.
#' @importFrom yaml yaml.load_file as.yaml
#' @seealso \code{\link{plot3d.character}}, \code{\link{plot3d.neuronlist}}
#' @export
#' @examples
#' \dontrun{
#' # scan a neuronlist
#' nlscan(kcs20)
#' 
#' # using neuron names
#' nlscan(names(kcs20), db=kcs20)
#' # equivalently using a default neuron list
#' options(nat.default.neuronlist='kcs20')
#' nlscan(names(kcs20))
#' }
#' # scan without waiting
#' nlscan(kcs20[1:4], Wait=FALSE, sleep=0)
#' \dontrun{
#' # could select e.g. the gamma neurons with unbranched axons
#' gammas=nlscan(kcs20)
#' clear3d()
#' plot3d(kcs20[gammas])
#' 
#' # plot surface model of brain first
#' # nb depends on package only available on github
#' devtools::install_github(username = "jefferislab/nat.flybrains")
#' library(nat.flybrains)
#' plot3d(FCWB)
#' # could select e.g. the gamma neurons with unbranched axons
#' gammas=nlscan(kcs20)
#' clear3d()
#' plot3d(kcs20[gammas])
#' }
nlscan <- function(neurons, db=NULL, col='red', Verbose=T, Wait=T, sleep=0.1,
                   extrafun=NULL, selected_file=NULL, selected_col='green',
                   yaml=TRUE, ...) {
  if(is.neuronlist(neurons)) {
    db=neurons
    neurons=names(db)
  }
  frames <- length(neurons)
  if(length(col)==1) col <- rep(col,frames)
  selected <- character()
  i <- 1
  if(!is.null(selected_file) && file.exists(selected_file)) {
    selected <- yaml.load_file(selected_file)
    if(!all(names(selected) %in% neurons)) stop("Mismatch between selection file and neurons.")
  }
  
  savetodisk <- function(selected, selected_file) {
    if(is.null(selected_file)) selected_file <- file.choose(new=TRUE)
    if(yaml){
      if(!grepl("\\.yaml$",selected_file)) selected_file <- paste(selected_file,sep="",".yaml")
      message("Saving selection to disk as ", selected_file, ".")
      writeLines(as.yaml(selected), con=selected_file)
    } else {
      if(!grepl("\\.rda$", selected_file)) selected_file <- paste(selected_file, sep="", ".rda")
      save(selected, file=selected_file)
      message("Saving selection to disk as ", selected_file)
    }
    selected_file
  }
  chc <- NULL
  while(TRUE){
    if(i > length(neurons) || i < 1) break
    n <- neurons[i]
    cat("Current neuron:", n, "(", i, "/", length(neurons), ")\n")
    pl <- plot3d(n, db=db, col=substitute(ifelse(n %in% selected, selected_col, col[i])), ..., SUBSTITUTE=FALSE)
    # call user supplied function
    more_rgl_ids <- list()
    if(!is.null(extrafun))
      more_rgl_ids <- extrafun(n, selected=selected)
    if(Wait){
      chc <- readline("Return to continue, b to go back, s to select, d [save to disk], t to stop, c to cancel (without returning a selection): ")
      if(chc=="c" || chc=='t'){
        sapply(pl, rgl.pop, type='shape')
        sapply(more_rgl_ids, rgl.pop, type='shape')
        break
      }
      if(chc=="s") {
        if(n %in% selected) {
          message("Deselected: ", n)
          selected <- setdiff(selected, n)
        } else selected <- union(selected, n)
      }
      if(chc=="b") i <- i-1
      else if (chc=='d') savetodisk(selected, selected_file)
      else i <- i+1
    } else {
      Sys.sleep(sleep)
      i <- i+1
    }
    sapply(pl, rgl.pop, type='shape')
    sapply(more_rgl_ids, rgl.pop, type='shape')
  }
  if(is.null(chc) || chc=='c') return(NULL)
  if(!is.null(selected_file)) savetodisk(selected, selected_file)
  selected
}
