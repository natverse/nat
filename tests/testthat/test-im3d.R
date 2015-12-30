context("im3d io")

test_that("we can read im3d files",{
  expect_is(d<-read.im3d("testdata/nrrd/LHMask.nrrd"),'im3d')
  expect_is(d,'array')
  expect_true(is.integer(d))
  expect_equal(sum(d!=0), 28669)
  
  expect_equal(read.im3d("testdata/nrrd/LHMask.nhdr"), d)
  
  expect_is(d0<-read.im3d("testdata/nrrd/LHMask.nrrd", ReadData=FALSE),'im3d')
  expect_equal(dim(d0), dim(d))
  expect_equal(length(d0), 0L)
  
  amfile="testdata/amira/AL-a_M.am"
  expect_is(d<-read.im3d(amfile), 'im3d')
  expect_is(d,'array')
  expect_equivalent(dim(d), c(154L, 154L, 87L))
  expect_is(d0<-read.im3d(amfile, ReadData=FALSE), 'im3d')
  expect_equivalent(dim(d0), c(154L, 154L, 87L))
  
  amfilenoam=tempfile()
  file.copy(normalizePath(amfile),amfilenoam)
  on.exit(unlink(amfilenoam))
  expect_equal(d,read.im3d(amfilenoam))
  
  expect_error(read.im3d("testdata/nrrd/LHMask.rhubarb"))
  
  v3drawfile1ch='testdata/v3draw/L1DS1_crop_straight_crop_ch1.v3draw'
  v3drawfile2ch='testdata/v3draw/L1DS1_crop_straight_crop.v3draw'
  v3drawfile2chslice='testdata/v3draw/L1DS1_crop_straight_crop_slice.v3draw'
  
  expect_error(read.im3d(v3drawfile2ch), "im3d is restricted to 3D")
  expect_equal(x<-read.im3d(v3drawfile2ch, chan=1), y<-read.im3d(v3drawfile1ch))
  expect_equal(x[,,1], read.im3d(v3drawfile2chslice)[,,1])
  
  # nb we can't test for strict equality because read.im3d.vaa3draw adds a
  # boundingbox in this case whereas read.nrrd does not
  expect_equal(dim(read.im3d('testdata/v3draw/L1DS1_crop_straight_crop_ch1.nhdr')),
               dim(y))
  
  # check that we can read metadata only
  expect_equal(boundingbox(read.im3d(v3drawfile1ch, ReadData = F)), 
               boundingbox(x))
})

test_that("round trip test for im3d is successful",{
  expect_is(d<-read.im3d("testdata/nrrd/LHMask.nrrd"),'im3d')
  dir.create(td<-tempfile())
  tf=tempfile(tmpdir = td, fileext='.nrrd')
  on.exit(unlink(td, recursive = TRUE))
  
  write.im3d(d, tf, dtype='byte')
  expect_is(d2<-read.im3d(tf),'im3d')
  expect_equal(d2, d, tol=1e-6)
  tf2=tempfile(fileext='.rhubarb')
  expect_error(write.im3d(d, tf2))
  
  tf3=tempfile(tmpdir = td, fileext='.nhdr')
  # also check detached nrrd
  expect_is(write.im3d(d, tf3, dtype='byte'), 'character')
  expect_equal(d3<-read.im3d(tf3), d, tol=1e-6)
  expect_true(file.exists(sub("\\.nhdr$",".raw.gz",tf3)))
  
  # check nrrd header fields as well in detail
  h1=attr(d,'header')
  expect_equal(attr(d3,'header')[names(h1)], h1[names(h1)], tol=1e-6)
})

context("im3d")

test_that("we can set bounding box",{
  z=im3d(,BoundingBox=c(0,1,0,2,0,4), dims=c(2,3,4))
  
  z1=z
  boundingbox(z1)<-boundingbox(z)
  expect_equal(z, z1)
  # set bounding box with an im3d object
  z2=z
  boundingbox(z2)<-z
  expect_equal(z, z2)
  
  boundingbox(z2)<-NULL
  expect_true(is.null(attr(z2,'BoundingBox')))
  
  expect_is(d<-read.im3d("testdata/nrrd/LHMask.nrrd"),'im3d')
  z3=z
  boundingbox(z3)<-boundingbox(d)
  expect_equal(boundingbox(z3), boundingbox(d))
  z4=z
  boundingbox(z4)<-boundingbox("testdata/nrrd/LHMask.nrrd")
  expect_equal(boundingbox(z4), boundingbox(d))
})

test_that("we can construct an im3d using an im3d to supply attributes",{
  d=rnorm(1000)
  x=im3d(d, dims=c(10, 10, 10), BoundingBox=c(20,200,100,200,200,300))
  expect_equal(x, im3d(x))
  expect_equal(x, im3d(d, x))
  x2=x
  boundingbox(x2)=boundingbox(x)*2
  # override bounding box
  expect_equal(x2, im3d(x, BoundingBox=c(20,200,100,200,200,300)*2))
})

test_that("we can construct an im3d with additional attributes",{
  d=rnorm(1000)
  x=im3d(d, dims=c(10, 10, 10), BoundingBox=c(20,200,100,200,200,300),
         units='microns',
         materials=data.frame(name='Exterior',id=0,col=rgb(1,0,0)))
  expect_is(x, "im3d")
  expect_equal(attr(x, 'units'), 'microns')
})

context("converting points to volumes")

test_that("we can construct an im3d from a set of points",{
  expect_is(im<-as.im3d(xyzmatrix(kcs20), voxdims=c(1,1,1)), "im3d")
  dims=c(122, 100, 83)
  expect_equivalent(dim(im), dims)
  expect_equal(voxdims(im), c(1, 1, 1))
  orig=apply(xyzmatrix(kcs20), 2, min)
  expect_equal(boundingbox(im), structure(matrix(c(orig, orig+dims-1), ncol=3, byrow = T),
                                          class='boundingbox'), tol=1e-6)
  
  expect_is(im<-as.im3d(xyzmatrix(kcs20), voxdims=c(1, 1, 1), 
                        BoundingBox=c(250, 410, 0, 130, 0, 120)), "im3d")
  expect_equal(dim(im), c(161, 131, 121))
  testim=im3d(dims = c(256, 128, 105), 
                voxdims = c(0.622087976539589, 0.622088062622309, 0.62208801843318))
  expect_is(im2<-as.im3d(xyzmatrix(kcs20), testim), 'im3d')
  expect_equal(boundingbox(im2), boundingbox(testim))
  expect_equal(dim(im2), dim(testim))
  expect_warning(as.im3d(xyzmatrix(kcs20), testim, origin = c(3,4,5)))
})

context("im3d boundingbox and friends")

test_that("dim, voxdims and boundingbox work",{
  
  expect_equal(boundingbox(c(x0=0,x1=10,y0=0,y1=20,z0=0,z1=30), dims=c(1,2,3), 
                           input='bounds'),
               boundingbox(c(5, 5, 5, 15, 5, 25)))
  
  expect_is(d<-read.im3d("testdata/nrrd/LHMask.nrrd"), 'im3d')
  expect_equal(dim(d),c(50,50,50))
  
  expect_is(d0<-read.im3d("testdata/nrrd/LHMask.nrrd", ReadData=FALSE), 'im3d')
  expect_equal(dim(d0),c(50,50,50))
  
  expect_equal(voxdims(d), c(1.4, 1.4, 1.4))
  
  bb_base=structure(c(0, 68.6, 0, 68.6, 0, 68.6), .Dim = 2:3, class='boundingbox')
  expect_equal(boundingbox(d), bb_base)
  expect_equal(boundingbox.character("testdata/nrrd/LHMask.nrrd"), bb_base)
  
  bbdf=as.data.frame(unclass(bb_base))
  expect_equal(boundingbox(bbdf),bb_base)
  
  expect_is(am<-read.im3d("testdata/amira/VerySmallLabelField.am", 
                          SimplifyAttributes=TRUE), 'im3d')
  expect_equivalent(dim(am),c(2L,2L,1L))
  expect_equal(voxdims(am),c(0.5,0.5,2))
  # somewhat oddly, Amira decides that if dim=1 for any axis, the bounding
  # box will not be 0 or infinite, but the size that would be expected for dim=2
  expect_equal(boundingbox(am),structure(c(0, 0.5, 0, 0.5, 0, 2), .Dim = 2:3,
                                         class='boundingbox'))
  
  expect_is(nrrd<-read.im3d("testdata/amira/VerySmallLabelField.nrrd",
                            SimplifyAttributes=TRUE), 'im3d')
  expect_equivalent(dim(am),c(2L,2L,1L))
  expect_equal(voxdims(am),c(0.5,0.5,2))
  
  # these should be equal when SimplifyAttributes=TRUE
  expect_equal(nrrd, am)

  expect_true(is.raw(nrrdraw<-read.im3d(ReadByteAsRaw=TRUE,
    "testdata/amira/VerySmallLabelField.nrrd", SimplifyAttributes=TRUE,)))
  expect_true(is.raw(amraw<-read.im3d(ReadByteAsRaw=TRUE,
    "testdata/amira/VerySmallLabelField.am", SimplifyAttributes=TRUE)))
  # ... and again
  expect_equal(nrrdraw, amraw)
  
  kcs20bb=structure(c(284.594, 404.6951, 24.1869, 122.9557, 21.4379, 102.8015
  ), .Dim = 2:3, class = "boundingbox")
  expect_equal(boundingbox(kcs20), kcs20bb, tol=1e-4)
})

context("im3d flip, slice and projection")

test_that("we can flip arrays",{
  m=matrix(1:4, ncol=2, nrow=2, byrow=TRUE)
  # NB the orientation is determined by matching x to 
  mf1=rbind(c(3,4),c(1,2))
  mf2=rbind(c(2,1),c(4,3))
  expect_equal(flip(m), mf1)
  expect_equal(flip(m,flipdim=2), mf2)
  expect_equal(flip(m,flipdim='y'), mf2)
  expect_error(flip(m,flipdim='z'))
  
  a6=array(1:6,1:3)
  # singleton x dimension so flip has no effect
  expect_equal(flip(a6), a6)
  expect_equal(flip(a6, 2), array(c(2,1,4,3,6,5),1:3))
  expect_equal(flip(a6, 3), array(c(5,6,3,4,1,2),1:3))
})

test_that("we can slice out subarray from image",{
  i=im3d(array(1:6,1:3),voxdims=c(2,3,4))
  i2=im3d(array(1:4,c(1,2,2)),voxdims=c(2,3,4))
  expect_equal(imslice(i, 1:2, drop=FALSE), i2)
  
  i4=im3d(array(1:6,2:3),dims=c(1,2,3),voxdims=c(2,3,4))
  expect_equal(imslice(i, 1, 'x'), i4)

  i3=im3d(array(1:4,c(2,2)),voxdims=c(2,3,4))
  expect_equal(imslice(i, 1:2), i3)
})

test_that("we can make projections",{
  expect_is(d<-read.im3d("testdata/nrrd/LHMask.nrrd"), 'im3d')
  expect_equal(dim(d),c(50,50,50))
  
  pd<-projection(d,projfun='sum')
  sd=read.im3d("testdata/nrrd/LHMask_sum.nrrd")
  expect_equal(pd, sd)
})

context("im3d unmask, threshold")

test_that("unmask works",{
  i=im3d(array(1:6,1:3),voxdims=c(2,3,4))
  # unmask a vector of im3d contents by original im3d returns original
  expect_equal(unmask(as.vector(i),i),i)
})

test_that("threshold works",{
  i=im3d(array(rep(TRUE, 6), 1:3),voxdims=c(2, 3, 4))
  # threshold a vector of logicals gives back the vector
  expect_equal(threshold(i, 0), i)
  
  # threshold a vector of integers gives appropriate logical vector
  i2=im3d(array(1:6, 1:3), voxdims=c(2, 3, 4))
  expect_equal(threshold(i2, 0), i)
  
  # also works with logica input
  expect_equal(threshold(i2, i2>0), i)
  
  # can also use integer or raw modes
  iraw=i
  mode(iraw)='raw'
  expect_equal(threshold(i2, 0, mode='raw'), iraw)
  
  iint=i
  mode(iint)='integer'
  expect_equal(threshold(i2, 0, mode='integer'), iint)
})

context("im3d coordinate utilities")

test_that("xyzpos, ijkpos and imexpand.grid work",{
  d=im3d(,dim=c(20,30,40),origin=c(10,20,30),voxdims=c(1,2,3))
  o=origin(d)
  expect_equal(ijkpos(d,o), c(1,1,1))
  expect_equal(xyzpos(d,c(1,1,1)), o)
  
  far_corner=boundingbox(d)[c(2,4,6)]
  expect_equal(ijkpos(d,far_corner), dim(d))
  expect_equal(xyzpos(d,dim(d)), far_corner)
  
  # round trip for 10 random points
  set.seed(42)
  ijks=mapply(sample,dim(d),10)
  expect_equal(ijkpos(d,xyzpos(d,ijks)), ijks)
  
  # check that imexpand.grid coords match direct translation of all indices
  # by xyzpos
  all_ijks=arrayInd(seq.int(prod(dim(d))), dim(d))
  expect_equal(imexpand.grid(d), xyzpos(d,all_ijks))
})

context("clampmax")

test_that("clampmax works",{
  # basic tests
  expect_is(cf<-clampmax(-10, 10),'function')
  expect_equal(cf(10, 20, Inf), NA_real_)
  expect_equal(cf(5, 10, 20, Inf, na.rm = TRUE), 10)
  expect_equal(cf(c(5, 10, 20, Inf), na.rm = TRUE), 10)
  expect_is(cf2<-clampmax(-10, 10, replace.infinite = FALSE),'function')
  expect_equal(cf2(10, 20, Inf), 10)
  expect_equal(cf2(10, 20, NA, Inf, na.rm=TRUE), 10)
  expect_equal(cf2(10, 20, NA, Inf, na.rm=FALSE), NA_real_)
  
  # in combination with projection
  LHMask=read.im3d('testdata/nrrd/LHMask.nrrd')
  d=unmask(rnorm(sum(LHMask),mean=5,sd=5),LHMask)
  p=projection(d,projfun=clampmax(0,10))
  expect_true(max(p, na.rm=T)<=10)
  expect_true(min(p, na.rm=T)>=0)
})


context("im3d plotting")
test_that("image.im3d works",{
  LHMask=read.im3d('testdata/nrrd/LHMask.nrrd')
  op=par(no.readonly = TRUE)
  layout(matrix(c(1, 2), ncol = 2L), widths = c(1, 0.2))
  
  baseline=list(zlim = 0:1, nlevels.actual = 21L, nlevels.orig = 20, 
                levels = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 
                           0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 
                           1),
                colors = c("#000080", "#002894", "#0050A8", "#0078BC", 
                           "#00A1D0", "#00C9E4", "#00F1F8", "#1AFFE4", "#43FFBB",
                           "#6BFF93", "#93FF6B", "#BBFF43", "#E4FF1A", "#FFF100",
                           "#FFC900", "#FFA100", "#FF7800", "#FF5000", "#FF2800",
                           "#FF0000")
                )
  expect_equal(rval<-image(imslice(LHMask,10), asp=TRUE), baseline)
  expect_null(imscalebar(rval))
  par(op)
})
