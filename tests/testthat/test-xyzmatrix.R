test_that("can extract xyz coords from a matrix and other objects",{
  # special case of 3-vector
  expect_equal(xyzmatrix(1:3), xyzmatrix(1,2,3))
  expect_error(xyzmatrix(1:4))
  
  mx=matrix(1:24,ncol=3)
  expect_equivalent(xyzmatrix(mx),mx)
  colnames(mx)=c("X","Y","Z")
  expect_equal(xyzmatrix(mx),mx)
  mx2=mx
  colnames(mx2)=c("x","y","z")
  expect_equal(xyzmatrix(mx2),mx)
  
  df=data.frame(X=1:4,Y=2:5,Z=3:6,W=1)
  expect_equal(xyzmatrix(df),data.matrix(df[,1:3]))
  # check handling of 1 row data.frames / matrices
  expect_is(xyz<-xyzmatrix(df[1,]), 'matrix')
  expect_equal(xyzmatrix(data.matrix(df[1,])), xyz)
  
  fake_neuron=list(SegList=list(1:2, 3:4), d=data.frame(X=1,Y=1,Z=1))
  real_neuron=neuron(SegList=list(1:2, 3:4), d=data.frame(X=1,Y=1,Z=1))
  xyz1=matrix(1,ncol=3, dimnames = list(NULL, c("X","Y","Z")))
  expect_equal(xyzmatrix(fake_neuron), xyz1)
  expect_equal(xyzmatrix(1,1,1), xyz1)
  expect_equal(xyzmatrix(real_neuron), xyzmatrix(fake_neuron))
  
  # check that if the input has columns of type character we interpret
  # as numeric (not factors, which is what data.matrix does)
  dfc=as.data.frame(sapply(df, as.character, simplify = F))
  expect_equal(xyzmatrix(dfc), xyzmatrix(df))
  # make sure we get a warning if we have non-numeric input
  dfc$X[1]="a"
  expect_warning(xyzmatrix(dfc))
  
  l=list(1:3, list(), NULL, 4:6, 2:4, 3:5, list())
  df=data.frame(id=seq_along(l))
  df$position=l
  baseline=matrix(c(1:3, rep(NA, 6), 4:6, 2:4, 3:5, NA, NA, NA),
                  byrow = T, ncol = 3)
  expect_equal(xyzmatrix(df$position), xyzmatrix(baseline))
  
  arrowdf <- structure(
    list(
      id = c(
        7393349L,
        7416439L,
        7415038L,
        7415013L,
        7415848L,
        7415851L,
        7415718L,
        7415838L,
        7415441L,
        4282686L
      ),
      pt_position = structure(
        list(
          c(709888L, 227744L, 57160L),
          c(710592L, 263392L, 129800L),
          c(722528L, 234656L, 77000L),
          c(722912L, 244032L, 65200L),
          c(721984L, 229792L, 119560L),
          c(722432L, 239520L, 98440L),
          c(720000L, 263360L, 115520L),
          c(722144L, 244224L, 110320L),
          c(715264L, 262720L,
            109880L),
          c(508896L, 63808L, 119080L)
        ),
        ptype = integer(0),
        class = c("list")
      )
    ),
    row.names = c(NA,-10L),
    class = c("tbl_df", "tbl", "data.frame")
  )
  baseline = structure(
    c(
      709888L,
      710592L,
      722528L,
      722912L,
      721984L,
      722432L,
      720000L,
      722144L,
      715264L,
      508896L,
      227744L,
      263392L,
      234656L,
      244032L,
      229792L,
      239520L,
      263360L,
      244224L,
      262720L,
      63808L,
      57160L,
      129800L,
      77000L,
      65200L,
      119560L,
      98440L,
      115520L,
      110320L,
      109880L,
      119080L
    ),
    .Dim = c(10L, 3L),
    .Dimnames = list(NULL,
                     c("X", "Y", "Z"))
  )
  expect_equal(
    xyzmatrix(arrowdf$pt_position),
    baseline,
    tolerance = 1e-6
  )
  
  xyzmatrix(arrowdf$pt_position) <- xyzmatrix(arrowdf$pt_position)+1
  expect_equal(xyzmatrix(arrowdf$pt_position), 
               baseline+1, tolerance = 1e-6)
})

test_that("can replace xyz coords of a matrix",{
  mx=matrix(1:24,ncol=3)
  colnames(mx)=c("X","Y","Z")
  mx2=mx
  colnames(mx2)=c("x","y","z")
  
  expect_equivalent(xyzmatrix(mx)<-xyzmatrix(mx2), mx)
  mx3=cbind(mx, W=1)
  mx3.saved=mx3
  expect_is(xyzmatrix(mx3)<-xyzmatrix(mx2), 'matrix')
  expect_equal(mx3, mx3.saved)
})

test_that("can extract xyz coords of a character vector",{
  mx=matrix((1:24)/3-5,ncol=3)
  coordstr=paste("(", paste(mx[,1], mx[,2], mx[,3], sep=", "), ")")
  expect_equal(xyzmatrix(mx), xyzmatrix(coordstr))
  
  coordstrna=c(coordstr, "c(NA,NA,NA)", "rhubarb", "1e-3 2")
  mxna=rbind(mx, matrix(NA, ncol=3, nrow=2), cbind(1e-3, 2, NA))
  expect_equal(xyzmatrix(mxna), xyzmatrix(coordstrna))
  
  tricky=c(1e-1, 1E+3, -1.01)
  expect_equal(xyzmatrix("1e-1, 1E+3, -1.01"), 
               xyzmatrix(matrix(tricky, ncol=3)))
  
  mx2=round(mx*-3, digits=6)
  coordstr2=paste("(", paste(mx2[,1], mx2[,2], mx2[,3], sep=", "), ")")
  expect_equivalent(xyzmatrix(coordstr) <- mx*-3, xyzmatrix(coordstr2))
  expect_equal(coordstr, coordstr2)
  
  xyzmatrix(coordstrna) <- mxna*-3
  expect_equivalent(xyzmatrix(coordstrna), mxna*-3)
  
  # now let's actually change some of the replacement values
  rep=mxna*-3
  rep[9,]=rep[1,]
  xyzmatrix(coordstrna) <- rep
  expect_equivalent(xyzmatrix(coordstrna), rep)
  # make sure we infer correct pattern
  expect_equal(coordstrna[9], coordstrna[1])
  
  # replace with more values than were there previously
  rep[11,]=c(4,5,6)
  expect_warning(xyzmatrix(coordstrna) <- rep)
  expect_equivalent(xyzmatrix(coordstrna), rep)
  # the next time there should no longer be any mismatches
  expect_silent(xyzmatrix(coordstrna) <- rep)
  
  # empty target
  empty_target=rep("", length(coordstrna))
  expect_silent(xyzmatrix(empty_target) <- rep)
  expect_equal(xyzmatrix(empty_target), xyzmatrix(rep))
})

test_that("can generate character representation of XYZ coords", {
  m=matrix(1:6, ncol=3, byrow = T)
  expect_equal(xyzmatrix2str(m),
               c("1,2,3", "4,5,6"))
  expect_equal(xyzmatrix2str(m), xyzmatrix2str(m, sep=","))
  expect_equal(xyzmatrix2str(m, sep=", "), 
               c("1, 2, 3", "4, 5, 6"))
  expect_error(xyzmatrix2str(m, sep=0))
  expect_equal(xyzmatrix2str(m, format="(%g;%g;%g)"), 
               c("(1;2;3)", "(4;5;6)"))
})

test_that("can replace xyz coords of a data.frame",{
  # we just need to handle some edge cases with 0 row data here
  
  df <- df2 <- data.frame(x=numeric(), y=numeric(), z=numeric())
  expect_silent(xyzmatrix(df) <- xyzmatrix(df)+1)
  # check no change
  expect_equal(df, df2)
  # nb generates both error and warning
  expect_warning(expect_error(xyzmatrix(df) <- 1:3))
})


test_that("can get/replace xyz coords in a list",{
  xyzm <- xyzmatrix(kcs20)
  df <- data.frame(a=1:sum(nvertices(kcs20)))
  expect_true(is.list(xyzml <- xyzmatrix2list(kcs20)))
  expect_true(all(lengths(xyzml)==3L))
  df$pos=xyzmatrix2list(kcs20)
  xyzmatrix(df$pos)=xyzmatrix(df$pos)+1
  expect_equal(xyzmatrix(df$pos), xyzmatrix(kcs20+1))
})


test_that("can extract xyz coords from a neuronlist",{
  xyz12=rbind(xyzmatrix(kcs20[[1]]),xyzmatrix(kcs20[[2]]))
  expect_is(xyzmatrix(kcs20[1:2]),'matrix')
  expect_equal(xyzmatrix(kcs20[1:2]), xyz12)
  kcs1_5=kcs20[1:5]
  xyzmatrix(kcs1_5) <- xyzmatrix(kcs1_5)
  expect_equal(kcs1_5, kcs20[1:5])
})

test_that("we can count number of vertices", {
  expect_equal(nvertices(kcs20[[1]]), 284L)
  expect_equal(nvertices(kcs20), sapply(kcs20, function(x) nrow(x$points)))
  expect_equal(nvertices(Cell07PNs), sapply(Cell07PNs, function(x) nrow(x$d)))
})
