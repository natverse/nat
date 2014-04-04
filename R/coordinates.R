#' Find 1D index given n-dimensional indices
#' 
#' Emulates the MATLAB function \code{sub2ind}.
#' @param dims vector of dimensions of object to index into.
#' @param indices vector of n-dimensional indices.
#' @export
sub2ind<-function(dims,indices){  
  # convert vector containing 1 coordinate into matrix
  if(!is.matrix(indices))
    indices=matrix(indices,byrow=TRUE,ncol=length(indices))
  if(length(dims)!=ncol(indices)){
    stop("indices must have the same number of columns as dimensions in dims")
  }
  k=cumprod(c(1,dims[-length(dims)]))
  ndx=1
  for(i in 1:length(dims)){
    v=indices[,i]
    if(any(v<1) || any(v>dims[i]))
      stop("index out of range")
    ndx=ndx+(v-1)*k[i]
  }
  ndx
}
