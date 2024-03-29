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

test_that("we can xform a data.frame",{
  scalefacs=c(1,1.1,1.2)
  affmat=matrix(0,4,4)
  diag(affmat)=c(scalefacs,1)
  df=as.data.frame(kcs20)
  expect_is(tdf <- xform(df, affmat), 'data.frame')
  expect_equivalent(xyzmatrix(tdf), scale(xyzmatrix(df), center = F, scale = 1/scalefacs))
})


if(!is.null(cmtk.bindir())){
test_that("we can xform a neuronlist with multiple registrations", {
  # construct a pair of CMTK affine registrations where f2 is the
  # inverse of f1
  scalefacs=c(1,1.1,1.2)
  m1=matrix(0,4,4)
  diag(m1)=c(scalefacs,1)
  m1[1:3,4]=c(1,2,3)
  m2=solve(m1)
  cmtk.mat2dof(m1, f = f1<-tempfile(fileext = ".reg"))
  cmtk.mat2dof(m2, f = f2<-tempfile(fileext = ".reg"))
  
  # nb subset is redundant - just to check 
  expect_equal(xform(Cell07PNs[1:3], c(f1, f2), subset=1:3), Cell07PNs[1:3])
  expect_equal(xform(Cell07PNs[1:3], c(f2, f1)), Cell07PNs[1:3])
  
  expect_equal(xform(Cell07PNs[1:2], c(f1, f1), subset=1:2,
                     VectoriseRegistrations = T),
               xform(Cell07PNs[1:2], f1))
  
  # now use reglist to wrap these transforms and check that works
  expect_equal(xform(Cell07PNs[1:2], reglist(m1,m2)), Cell07PNs[1:2])
  expect_equal(xform(Cell07PNs[1:2], reglist(f1, f1, swap=c(F,T))), Cell07PNs[1:2])
  
  # check inversion
  rl=reglist(m1,m2)
  expect_equal(simplify_reglist(rl), simplify_reglist(invert_reglist(rl)))
  
  unlink(c(f1,f2))
})
}

if(!is.null(cmtk.bindir())){
  test_that("we can xform points with non-rigid reg and handle bad points", {
    reg="testdata/cmtk/FCWB_JFRC2_01_warp_level-01.list"
    m=matrix(c(4:9, -200,-200,-200), ncol=3, byrow = T)
    baselinem=matrix(NA_real_, ncol=3, nrow=3)
    baselinem[2,]=c(2.65463188, 13.857304, 14.7245891)
    expect_warning(tm<-xform(m, reg), "2 points .*not.*transformed")
    expect_equal(tm, baselinem)
    expect_error(xform(m, reg, na.action='error'), "2 points")
    # nb should not drop dimensions
    expect_equal(xform(m, reg, na.action='drop'), baselinem[2, , drop=FALSE])
  })
}

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

if(!is.null(cmtk.bindir())){
  test_that("we can mirror with a non-rigid registration", {
    reg="testdata/cmtk/FCWB_JFRC2_01_warp_level-01.list"
    # nb this registration doesn't actually make any sense as mirroring
    # registration but it does produce some valid data
    m=matrix(c(0,0,0,300,200,50), ncol=3, byrow = T)
    expect_warning(tm<-mirror(m, mirrorAxisSize = 636.396, warpfile = reglist(diag(4), reg)))
    baseline=matrix(c(NA,NA,NA,300.953189,183.401797,40.7112503), 
                    ncol=3, byrow = T)
    expect_equal(tm, baseline)
  })
  
  test_that("we can mirror some image data", {
    reg="testdata/cmtk/FCWB_mirror_level-01.list"
    img="testdata/nrrd/FCWB_2um_mask.nrrd"
    bim=boundingbox(read.im3d(img, ReadData = F))
    tf=tempfile(fileext = '.nrrd')
    on.exit(unlink(tf))
    mirror(img, warpfile=reglist(diag(4), reg), output=tf, target=img, Verbose=FALSE)
    expect_true(file.exists(tf))
    expect_equal(boundingbox(tf), bim)
  })
}

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

test_that("we can extract/replace coords and xform shape3d objects",{
  m=as.mesh3d(MBL.surf)
  xyz=xyzmatrix(m)
  expect_is(xyz,'matrix')
  expect_equal(dim(xyz), c(1068L, 3L))
  # dummy transformation
  expect_equal(xform(m, function(x,...) x), m)
  expect_equal(xform(shapelist3d(m, plot = F), function(x,...) x)[[1]], m)
})
