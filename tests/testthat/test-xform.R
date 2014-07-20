context("xform")

test_that("xform can use a function to define a registration", {
  n=Cell07PNs[[1]]
  expect_equal(xform(n,reg=function(x,...) -x),n*c(-1,-1,-1))
})

if(!is.null(cmtk.bindir())){
test_that("xform gives same result as xformpoints", {
  reg2="testdata/cmtk/dofv1.1wshears.list"
  creg2=cmtkreg(reg2)
  
  n=Cell07PNs[[1]]
  expect_equal(data.matrix(xform(n,reg=creg2)$d[,c("X","Y","Z")]),
               xformpoints(creg2,n$d[,c("X","Y","Z")]))
})
}

test_that("xform with affine matrix gives same result as neuron arithmetic", {
  n=Cell07PNs[[1]]
  scalefacs=c(1,1.1,1.2)
  affmat=matrix(0,4,4)
  diag(affmat)=c(scalefacs,1)
  expect_equal(xform(n,reg=affmat),n*scalefacs)
  expect_equal(xform(n,reg=affmat),scale(n,scale=1/scalefacs,center=FALSE))
})

test_that("mirror with flip only gives same result as neuron arithmetic", {
  n=Cell07PNs[[1]]
  mn1=(n*c(-1,1,1))+c(168,0,0)
  mn2=mirror(n,mirrorAxisSize=168)
  expect_equal(mn2,mn1)
})

test_that("mirroring twice returns original object", {
  n=Cell07PNs[[1]]
  f=function(n) mirror(n,mirrorAxisSize=168)
  expect_equal(f(f(n)),n)
  k=kcs20[[1]]
  g=function(x) mirror(x,mirrorAxisSize=564.2532)
  expect_equal(g(g(k)),k)
  k12=kcs20[1:2]
  expect_equal(g(g(k12)),k12)
})

test_that("we can mirror raw points", {
  k=kcs20[[1]]
  g=function(x) mirror(x,mirrorAxisSize=564.2532)
  xyz=data.matrix(k$points)
  expect_equal(g(g(xyz)),xyz)
})

test_that("we can mirror using different forms of axis specification", {
  n=Cell07PNs[[1]]
  expect_equal(mirror(n,mirrorAxisSize=0,mirrorAxis=1),mirror(n,mirrorAxisSize=0))
  expect_equal(mirror(n,mirrorAxisSize=0,mirrorAxis=2),mirror(n,mirrorAxisSize=0,mirrorAxis="Y"))
  expect_error(mirror(n,mirrorAxisSize=0,mirrorAxis=1:2))
})

test_that("we can mirror a neuron list", {
  k5=kcs20[1:5]
  expect_equal(mirror(mirror(k5,mirrorAxisSize=0),mirrorAxisSize=0),k5)
  
  # some members of a list only
  expect_equal((m<-mirror(kcs20,subset=1:5,mirrorAxisSize=0))[1:5],
               mirror(k5,mirrorAxisSize=0))
  expect_equal(m[6:10],kcs20[6:10])
})

context('xyzmatrix')

test_that("can extract xyz coords from a matrix",{
  mx=matrix(1:24,ncol=3)
  expect_equivalent(xyzmatrix(mx),mx)
  colnames(mx)=c("X","Y","Z")
  expect_equal(xyzmatrix(mx),mx)
  mx2=mx
  colnames(mx2)=c("x","y","z")
  expect_equal(xyzmatrix(mx2),mx)
  
  df=data.frame(X=1:4,Y=2:5,Z=3:6,W=1)
  expect_equal(xyzmatrix(df),data.matrix(df[,1:3]))
})

test_that("can replace xyz coords of a matrix",{
  mx=matrix(1:24,ncol=3)
  colnames(mx)=c("X","Y","Z")
  mx2=mx
  colnames(mx2)=c("x","y","z")
  
  expect_equivalent(xyzmatrix(mx)<-xyzmatrix(mx2), mx)
})

test_that("can extract xyz coords from a neuronlist",{
  xyz12=rbind(xyzmatrix(kcs20[[1]]),xyzmatrix(kcs20[[2]]))
  expect_is(xyzmatrix(kcs20[1:2]),'matrix')
  expect_equal(xyzmatrix(kcs20[1:2]), xyz12)
})
