#' Decompose homogeneous affine matrix to CMTK registration parameters
#' 
#' @details The version attribute of the resultant matrix marks this as
#'   compliant with CMTK>v2.4 (~ Dec 2013) when a bug in affine matrix
#'   (de)composition was fixed.
#' @param matrix 4x4 homogeneous affine matrix
#' @param centre Rotation centre
#' @return 5x3 matrix of CMTK registration parameters with a version attribute
#' @export
#' @family cmtk-geometry
affmat2cmtkparams<-function(matrix,centre=c(0,0,0)){
  # C matrices indexed [C][R] (vs [R,C] in R)
  # so it seems easiest to transpose for use in R
  matrix=t(matrix)
  # params will contain Torsten's transformation parameters
  params=numeric(15)
  
  # translation entries
  params[1:3] = matrix[4,1:3]
  
  cM=c(
    sum(centre[1:3]*matrix[1:3,1]),
    sum(centre[1:3]*matrix[1:3,2]),
    sum(centre[1:3]*matrix[1:3,3]) )
  
  params[1:3] = params[1:3] + cM[1:3] - centre[1:3]
  params[13:15]=centre
  
  # QR decomposition
  matrix2d=t(matrix[1:3,1:3])
  qr.res=qr(matrix2d)
  Q=qr.Q(qr.res)
  R=qr.R(qr.res)
  R[lower.tri(R)]=0
  
  for (k in 1:3) {
    # if scale is negative, make positive and correct Q and R accordingly (we will figure out later if the overall transformation is a true rotation or has a negative determinant)
    if ( R[k,k] < 0 ) {
      R[k,1:3] = -R[k,1:3]
      Q[1:3,k] = -Q[1:3,k]
    }
    
    # scale
    params[6 + k] = R[k,k]
    
    # report error on singular matrices.
    if ( params[6+k]	< .Machine$double.eps ) stop("singular matrix")
    
    # shear: i,j index the upper triangle of aMat, which is R from QR
    i = (c(0, 0, 1)+1)[k]  # i.e. i := { 0, 0, 1 }
    j = (c(1, 2, 2)+1)[k]  # i.e. j := { 1, 2, 2 }
    params[9+k] = R[i,j]
  }
  
  # =========================================================================
  # 
  # THE FOLLOWING CODE WAS ADOPTED AND MODIFIED FROM VTK, The Visualization
  # Toolkit.
  # 
  #		Program:	 Visualization Toolkit
  #		Language:	 C++
  #		Thanks:		 Thanks to David G. Gobbi who developed this class.
  # 
  # Copyright (c) 1993-2001 Ken Martin, Will Schroeder, Bill Lorensen 
  # All rights reserved.
  # 
  # Redistribution and use in source and binary forms, with or without
  # modification, are permitted provided that the following conditions are met:
  # 
  #	 * Redistributions of source code must retain the above copyright notice,
  #		 this list of conditions and the following disclaimer.
  # 
  #	 * Redistributions in binary form must reproduce the above copyright notice,
  #		 this list of conditions and the following disclaimer in the documentation
  #		 and/or other materials provided with the distribution.
  # 
  #	 * Neither name of Ken Martin, Will Schroeder, or Bill Lorensen nor the names
  #		 of any contributors may be used to endorse or promote products derived
  #		 from this software without specific prior written permission.
  # 
  #	 * Modified source versions must be plainly marked as such, and must not be
  #		 misrepresented as being the original software.
  # 
  # THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS''
  # AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  # IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  # ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
  # ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  # DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
  # SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
  # CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
  # OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  # OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  # 
  # =========================================================================
  
  if (det(matrix[1:3,1:3])<0){
    # negative determinant, not a true rotation
    # negate x scale
    params[6+1] = -params[6+1];
    # also negative shears related to x
    params[9:10+1] = -params[9:10+1];
  }
  
  VTK.AXIS.EPSILON=1E-8
  
  # FIXED FROM HERE
  # rotation
  # first rotate about y axis
  x2 = Q[2,1] / params[7]
  y2 = Q[3,1] / params[7]
  z2 = Q[1,1] / params[7]
  
  x3 = Q[2,3] / params[9]
  y3 = Q[3,3] / params[9]
  z3 = Q[1,3] / params[9]
  
  dot = x2 * x2 + z2 * z2
  d1 = sqrt (dot)
  
  if (d1 < VTK.AXIS.EPSILON) {
    cosTheta = 1.0
    sinTheta = 0.0
  } else {
    cosTheta = z2 / d1
    sinTheta = x2 / d1
  }
  rad2deg=function(theta) theta/(2*pi)*360
  
  params[6] = rad2deg(-atan2 (sinTheta, cosTheta)) # theta
  
  # now rotate about x axis
  dot = x2 * x2 + y2 * y2 + z2 * z2
  d = sqrt (dot)
  
  if (d < VTK.AXIS.EPSILON) {
    sinPhi = 0.0
    cosPhi = 1.0
  } else 
    if (d1 < VTK.AXIS.EPSILON) {
      sinPhi = y2 / d
      cosPhi = z2 / d
    } else {
      sinPhi = y2 / d
      cosPhi = ( x2 * x2 + z2 * z2) / (d1 * d)
    }
  
  params[5] = rad2deg(-atan2 (sinPhi, cosPhi)) # phi 
  
  # finally, rotate about z
  x3p = x3 * cosTheta - z3 * sinTheta
  y3p = - sinPhi * sinTheta * x3 + cosPhi * y3 - sinPhi * cosTheta * z3
  dot = x3p * x3p + y3p * y3p
  d2 = sqrt (dot)
  if (d2 < VTK.AXIS.EPSILON) {
    cosAlpha = 1.0
    sinAlpha = 0.0
  } else {
    cosAlpha = y3p / d2
    sinAlpha = x3p / d2
  }
  
  params[4] = rad2deg(-atan2 (sinAlpha, cosAlpha)) # alpha
  
  # /** END OF ADOPTED VTK CODE **/
  param_matrix=matrix(params,ncol=3,byrow=TRUE)
  attr(param_matrix,'version')=numeric_version('2.4')
  return (param_matrix)
}

#' Compose homogeneous affine matrix from CMTK registration parameters
#' 
#' @details If the \code{legacy} parameter is not set explicitly, then it will 
#'   be set to \code{TRUE} if params has a version attribute <2.4 or FALSE 
#'   otherwise.
#' @param params 5x3 matrix of CMTK registration parameters or list of length 5.
#' @param legacy Whether to assume that parameters are in the format used by 
#'   CMTK <=2.4.0 (default value NA implies FALSE, see details).
#' @param tx,ty,tz Translation along x, y and z axes (default 0)
#' @param rx,ry,rz Rotation about x, y and z axes (in degrees, default 0)
#' @param sx,sy,sz Scale for x, y and z axes (default 1)
#' @param shx,shy,shz Shear for x,y,z axes (default 0)
#' @param cx,cy,cz Centre for rotation
#' @return 4x4 homogeneous affine transformation matrix
#' @details translation and centre components are assumed to be in physical 
#'   coordinates.
#' @export
#' @family cmtk-geometry
cmtkparams2affmat<-function(params=NULL, tx=0, ty=0, tz=0,
                                     rx=0, ry=0, rz=0, 
                                     sx=1, sy=1, sz=1,
                                     shx=0, shy=0, shz=0,
                                     cx=0, cy=0, cz=0,
                                     legacy=NA){
  
  if(is.na(legacy)) legacy=isTRUE(attr(params,'version')<numeric_version('2.4'))
  
  if(!is.null(params)){
    if(is.list(params)) params=unlist(params)
    if(is.vector(params)) params=matrix(params,ncol=3,byrow=T)
    if(identical(dim(params), as.integer(c(3,5)))) params = t(params) else {
      if(!identical(dim(params),as.integer(c(5,3)))) stop("unable to parse params")
    }
    tx=params[1,1];ty=params[1,2];tz=params[1,3]
    rx=params[2,1];ry=params[2,2];rz=params[2,3]
    sx=params[3,1];sy=params[3,2];sz=params[3,3]
    shx=params[4,1];shy=params[4,2];shz=params[4,3]
    cx=params[5,1];cy=params[5,2];cz=params[5,3]
  }
  
  DegToRad=function(theta) theta/360*2*pi
  
  alpha = DegToRad(rx)
  theta = DegToRad(ry)
  phi = DegToRad(rz)
  
  cos0 = cos(alpha)
  sin0 = sin(alpha)
  cos1 = cos(theta)
  sin1 = sin(theta)
  cos2 = cos(  phi)
  sin2 = sin(  phi)
  
  sin0xsin1 = sin0 * sin1
  cos0xsin1 = cos0 * sin1
  
  rval=matrix(0,4,4)
  diag(rval)<-1
  # nb in R matrices are indexed m[row,col]
  # whereas in C looks like T indexed them
  # m[col-1][row-1]
  # Regexps to transform these forms:
  # \[\d\]\[\d\]
  # (\2+1,\1+1)
  # 
  rval[0+1,0+1] =  cos1*cos2
  rval[1+1,0+1] = -cos1*sin2
  rval[2+1,0+1] = -sin1
  rval[0+1,1+1] =  (sin0xsin1*cos2 + cos0*sin2)
  rval[1+1,1+1] = (-sin0xsin1*sin2 + cos0*cos2)
  rval[2+1,1+1] =  sin0*cos1
  rval[0+1,2+1] =  (cos0xsin1*cos2 - sin0*sin2)
  rval[1+1,2+1] = (-cos0xsin1*sin2 - sin0*cos2)
  rval[2+1,2+1] =  cos0*cos1
  
  if(legacy){
    rval[1:3,1]=rval[1:3,1]*sx
    rval[1:3,2]=rval[1:3,2]*sy
    rval[1:3,3]=rval[1:3,3]*sz
    shears=c(shx,shy,shz)
    # generate shears in broken CMTK <2.4.0 style
    for (i in 3:1 ) {
      shear=matrix(0,4,4)
      diag(shear)<-1
      # i/2 {0,0,1} for i={0,1,2}
      # (i/2)+(i%2)+1 {1,2,2} for i={0,1,2}
      # shear[i/2][(i/2)+(i%2)+1] = dofs[9+i];
      shear[c(2,3,3)[i],c(1,1,2)[i]]=shears[i]
      rval = shear%*%rval
    }
  } else {
    # generate scales and shears according to CMTK >=v.2.4.0 / svn r5050
    scaleShear=matrix(0,4,4)
    diag(scaleShear)=c(sx,sy,sz,1)
    scaleShear[0+1,1+1]=shx
    scaleShear[0+1,2+1]=shy
    scaleShear[1+1,2+1]=shz
    
    # NB matrix multiplication must be in opposite order from C original
    rval = rval%*%scaleShear
  }
  
  # transform rotation center
  cM = c(  cx*rval[0+1,0+1] + cy*rval[0+1,1+1] + cz*rval[0+1,2+1],
           cx*rval[1+1,0+1] + cy*rval[1+1,1+1] + cz*rval[1+1,2+1],
           cx*rval[2+1,0+1] + cy*rval[2+1,1+1] + cz*rval[2+1,2+1]  )
  
  # set translations
  rval[0+1,3+1] = tx - cM[1] + cx
  rval[1+1,3+1] = ty - cM[2] + cy
  rval[2+1,3+1] = tz - cM[3] + cz
  
  return(rval)
}
