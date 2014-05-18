#===================================================================
# GJ 2005-11-27 
# R translation of code by softSurfer 
# from: http://softsurfer.com/Archive/algorithm_0106/algorithm_0106.htm
#
# Copyright 2001, softSurfer (www.softsurfer.com)
# This code may be freely used and modified for any purpose
# providing that this copyright notice is included with it.
# SoftSurfer makes no warranty for this code, and cannot be held
# liable for any real or imagined damage resulting from its use.
# Users of this code must verify correctness for their application.
#===================================================================

# dist3D_Segment_to_Segment():
#  Input:  two 3D line segments S1 and S2
#  Return: the shortest distance between S1 and S2
dist3D_Segment_to_Segment<-function(s1,s2,SMALL.NUM=1e-6)
{
  p0=s1[1:3];p1=s1[4:6]
  q0=s2[1:3];q1=s2[4:6]
  
  u = p1-p0
  v = q1 - q0
  w = p0 - q0
  a = dotprod(u,u)	# always >= 0
  b = dotprod(u,v)
  c = dotprod(v,v)	# always >= 0
  d = dotprod(u,w)
  e = dotprod(v,w)
  D = a*c - b*b	 # always >= 0
  sD = D	# sc = sN / sD, default sD = D >= 0
  tD = D	# tc = tN / tD, default tD = D >= 0
  
  # compute the line parameters of the two closest points
  if (D < SMALL.NUM) { # the lines are almost parallel
    sN = 0.0	# force using point P0 on segment S1
    sD = 1.0	# to prevent possible division by 0.0 later
    tN = e
    tD = c
  }
  else {# get the closest points on the infinite lines
    sN = (b*e - c*d)
    tN = (a*e - b*d)
    if (sN < 0.0) {	 # sc < 0 => the s=0 edge is visible
      sN = 0.0
      tN = e
      tD = c
    }
    else if (sN > sD) {	# sc > 1 => the s=1 edge is visible
      sN = sD
      tN = e + b
      tD = c
    }
  }
  
  if (tN < 0.0) {	# tc < 0 => the t=0 edge is visible
    tN = 0.0
    # recompute sc for this edge
    if (-d < 0.0)
      sN = 0.0
    else if (-d > a)
      sN = sD
    else {
      sN = -d
      sD = a
    }
  }
  else if (tN > tD) {	# tc > 1 => the t=1 edge is visible
    tN = tD
    # recompute sc for this edge
    if ((-d + b) < 0.0)
      sN = 0
    else if ((-d + b) > a)
      sN = sD
    else {
      sN = (-d + b)
      sD = a
    }
  }
  # finally do the division to get sc and tc
  sc = if(abs(sN) < SMALL.NUM) 0.0 else (sN / sD)
  tc = if(abs(tN) < SMALL.NUM) 0.0 else (tN / tD)
  
  # get the difference of the two closest points
  dP = w + (sc * u) - (tc * v)	# = S1(sc) - S2(tc)
  
  return(sqrt(sum(dP^2)))	 # return the closest distance
}
