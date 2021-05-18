context("neuron fileformats")

test_that("we can query fileformats",{
  expect_equal(fileformats(ext='swc',rval='names'), c('swc','swcng'))
  expect_equal(fileformats(ext='am', class='neuron', rval='names'),
               c('hxlineset','hxskel'))
  expect_is(fileformats(class='neuron',rval='info'),'data.frame')
  
  expect_is(fw<-getformatwriter(file='test.rds', class='neuron'),'list')
  expect_equal(fw$ext,'.rds')
  expect_equal(fw$read,readRDS)
  expect_equal(fw$write,saveRDS)
  
  expect_equal(getformatwriter(file='test.am', format='rds', class='neuron')$file,'test.am')
  
  expect_equal(getformatwriter(file='test.am', format='rds', ext='.rds', class='neuron')$file,'test.rds')
  expect_equal(getformatwriter(file='test', format='rds', ext='.rds', class='neuron')$file,'test.rds')
  
  expect_equal(getformatwriter(file='test.am', format='rds', ext=NA, class='neuron')$file,'test.am')
  expect_equal(getformatwriter(file='test.am', format='rds', ext=NULL, class='neuron')$file,'test.am')
  
  expect_equal(getformatwriter(file='test', format='rds', ext=NULL, class='neuron')$file,'test.rds')
  expect_equal(getformatwriter(file='test', format='rds', ext=NA, class='neuron')$file,'test')
  
  expect_equal(getformatwriter(file='test.am', ext='.rds', class='neuron')$ext,'.rds')
  
  expect_error(getformatwriter(file='test.rds', ext='.rhubarb', class='neuron'))
  
  expect_equal(fileformats(format='hxl', ext='_skel.am', class='neuron'),
               'hxlineset')
})

test_that("we can use optional brotli format", {
  expect_is(fw<-getformatwriter(file='test.rdsb', class='neuron'),'list')
  expect_equal(fw$ext,'.rdsb')
  expect_equal(fw$read, readBrotli)
  expect_equal(fw$write, saveBrotli)
  skip_if_not_installed('brotli')
  tf <- tempfile(fileext = ".zip")
  tf2 <- tempfile(fileext = ".zip")
  write.neurons(kcs20, dir = tf, format='rdsb')
  write.neurons(kcs20, dir = tf2, format='rds')
  expect_equal(read.neurons(tf), read.neurons(tf2))
})

test_that("we can use optional qs format", {
  expect_is(fw<-getformatwriter(file='test.qs', class='neuron'),'list')
  expect_equal(fw$ext,'.qs')
  expect_equal(fw$read, readqs)
  expect_equal(fw$write, saveqs)
  skip_if_not_installed('qs')
  tf <- tempfile(fileext = ".zip")
  tf2 <- tempfile(fileext = ".zip")
  write.neurons(Cell07PNs[1:5], dir = tf, format='qs', include.data.frame = T)
  expect_equal(read.neurons(tf), Cell07PNs[1:5])
  expect_equal(read.neurons(tf, nl = Cell07PNs[6:10]), Cell07PNs[c(6:10,1:5)])
})


test_that("we can set new fileformats",{
  expect_error(registerformat('rhubarb'), 'provide.*read or write')
  # returns null on success
  expect_null(registerformat('rhubarb', class='crumble', read=read.table))
  expect_warning(registerformat('rhubarb', class='crumble', read=read.table),
                 'already been registered')
})
  
test_that("is.swc works", {
  expect_false(is.swc("testdata/neuron/EBT7R.am"))
  expect_false(is.swc("testdata/neuron/SequentiallyBranchingTrace.traces"))
  expect_true(is.swc("testdata/neuron/XT6L2.CNG.swc"))
  file.copy("testdata/neuron/XT6L2.CNG.swc", tf<-tempfile())
  on.exit(unlink(tf))
  expect_true(is.swc(tf))
  expect_is(read.neuron(tf), 'neuron')
})

context("neurons reading")

test_that("we can read single neurons in rda or rds format", {
  rda=tempfile(fileext='.rda')
  rds=tempfile(fileext='.rds')
  on.exit(unlink(c(rda,rds)))
  
  n=Cell07PNs[[1]]
  save(n,file=rda)
  saveRDS(n,file=rds)
  expect_equivalent(n,read.neuron(rda))
  expect_equivalent(n,read.neuron(rds))
  
  # check that we can read neurons in rda or rds format
  # even if they do not have an appropriate file extension
  file.copy(rds,tfrds<-tempfile())
  file.copy(rda,tfrda<-tempfile())
  on.exit(unlink(c(tfrda,tfrds)),add=TRUE)
  
  expect_equivalent(n,read.neuron(tfrds,format='rds'))
  expect_equivalent(n,read.neuron(tfrda,format='rda'))
  
  # check that a length 1 neuronlist works ok
  expect_equivalent(n,read.neurons(tfrds,format='rds')[[1]])
})

test_that("we can read single dotprops objects in rda or rds format", {
  rda=tempfile(fileext='.rda')
  rds=tempfile(fileext='.rds')
  on.exit(unlink(c(rda,rds)))
  
  n=kcs20[[1]]
  save(n,file=rda)
  saveRDS(n,file=rds)
  expect_equivalent(n,read.neuron(rda))
  expect_equivalent(n,read.neuron(rds))
  
  # check that we can read dotprops objects in rda or rds format
  # even if they do not have an appropriate file extension
  file.copy(rds,tfrds<-tempfile())
  file.copy(rda,tfrda<-tempfile())
  on.exit(unlink(c(tfrda,tfrds)),add=TRUE)
  expect_equivalent(n,read.neuron(tfrds,format='rds'))
  expect_equivalent(n,read.neuron(tfrda,format='rda'))
  
  expect_equivalent(n,read.neurons(tfrds,format='rds')[[1]])
})

test_that("we can read neurons in swc format", {
  swc='testdata/neuron/EBT7R.CNG.swc'
  expect_is(n<-read.neuron(swc),'neuron')
  expect_equal(n$NeuronName,'EBT7R.CNG')
})

test_that("we can read swc data into an ngraph object", {
  swc='testdata/neuron/EBT7R.CNG.swc'
  expect_is(ng<-read.neuron(swc, class='ngraph'),'ngraph')
  expect_equal(as.neuron(ng), read.neuron(swc))
})

test_that("we get an error when trying to read a non-neuron file", {
  nrrd="testdata/nrrd/LHMask.nrrd"
  expect_error(read.neuron(nrrd))
})

test_that("we can set the NeuronName field when reading a file", {
  swc='testdata/neuron/EBT7R.CNG.swc'
  n<-read.neuron(swc, NeuronName="rhubarb")
  expect_equal(n$NeuronName,'rhubarb')
  # check that we can use a user defined function to define the NeuronName
  nfun=function(x) sub("\\..*","",basename(x))
  n<-read.neuron(swc, NeuronName=nfun)
  expect_equal(n$NeuronName,'EBT7R')
})

test_that("we can read in neurons as a neuronlist",{
  expect_is(nl<-read.neurons(paths='testdata/neuron/',pattern='\\.CNG\\.swc$',
               neuronnames=function(x) sub("\\..*","",basename(x))),'neuronlist')
  expect_equal(length(nl),2)
  
  # check that InputFileName field is not mangled
  expect_true('InputFileName'%in%names(nl[[1]]))
  
  fieldsToIgnore=c("CreatedAt",'InputFileStat')
  n.read.neurons=nl[[1]]
  n.read.neurons[fieldsToIgnore]=NULL
  n.read.neuron=read.neuron(n.read.neurons$InputFileName)
  n.read.neuron[fieldsToIgnore]=NULL
  expect_equal(unclass(n.read.neuron),unclass(n.read.neurons), 
               info = 'check equality of neuron read by read.neuron & read.neurons')
  
  # check that problem files are named on error/warning
  expect_message(suppressWarnings(read.neurons('testdata/neuron/Neurites.am')),
                 regexp = 'While reading file.*Neurites\\.am')
})


test_that('we can deal with bad input to read.neurons', {
  # by definition tempfile() doesn't exist
  expect_warning(read.neurons(tempfile()), regexp = "No such file or directory")
})

test_that("we can read hxlineset format neurons",{
  expect_known_value(read.neuron(test_path('testdata/neuron/EBT7R.am')),
                     test_path("testdata/neuron/EBT7R.rds"))
})

test_that("we can read hxskel format neurons",{
  # NB this neuron does not have an origin set
  expect_warning(n<-read.neuron('testdata/neuron/Neurites.am'), 
                 regexp = 'No valid origin found')
  expect_is(n,'neuron')
  Neurites <- readRDS(test_path('testdata/neuron/Neurites.rds'))
  g1<-as.ngraph(Neurites)
  g2<-as.ngraph(n)
  expect_true(igraph::graph.isomorphic(g1,g2))
  expect_equal(n, Neurites)
  tmpfile=tempfile(fileext='.wurgle')
  on.exit(unlink(tmpfile))
  file.copy('testdata/neuron/Neurites.am',tmpfile)
  
  expect_equal(suppressWarnings(read.neuron(tmpfile)), n,
               fieldsToExclude='NeuronName')
})

test_that("we can read multiple neurons from a zip archive", {
  files_to_zip <- c("testdata/neuron/testneuron_am3d.am", "testdata/neuron/testneuron_lineset.am")
  # swallow extraneous warning
  expect_warning(neurons <- read.neurons(files_to_zip,
                                         neuronnames = function(f) tools::file_path_sans_ext(basename(f))),
                 regexp = "specifies radius")
  zip_file <- paste0(tempfile(), ".zip")
  on.exit(unlink(zip_file, recursive=TRUE))
  zip(zip_file, files_to_zip)
  expect_warning(zip_neurons <- read.neurons(zip_file, format="zip",
                                             neuronnames = function(f) tools::file_path_sans_ext(basename(f))),
                 regexp = "specifies radius")
  expect_equal(neurons, zip_neurons)
})

test_that("we can write multiple neurons to a zip archive", {
  dir.create(td<-tempfile())
  owd=setwd(td)
  zip_file <- "test.zip"
  on.exit(unlink(td, recursive = TRUE))
  file.create(zip_file)
  expect_error(write.neurons(Cell07PNs[1:5], zip_file, format="swc"), 
               'already exists')
  write.neurons(Cell07PNs[1:5], zip_file, format="swc", Force=T, subdir=Glomerulus)
  nat.utils::zipinfo(zip_file)
  zip_neurons <- read.neurons(zip_file, format="zip")
  # fix names and compare
  names(zip_neurons)=sub("\\.swc","",names(zip_neurons))
  expect_equivalent(Cell07PNs[1:5], zip_neurons[names(Cell07PNs)[1:5]])
  
  zip_file2 <- "test2.zip"
  write.neurons(Cell07PNs[1:5], zip_file2, format="rds", include.data.frame = T)
  expect_equal(read.neurons(zip_file2), Cell07PNs[1:5])
  setwd(owd)
})

test_that("we can identify Amira hxskel neurons",{
  # hxlineset neuron
  expect_false(is.hxskel('testdata/neuron/EBT7R.am'))
  # swc neuron
  expect_false(is.hxskel('testdata/neuron/EBT7R.CNG.swc'))
  # hxskel neuron
  expect_true(is.hxskel('testdata/neuron/Neurites.am'))
  # hxskel
  p='testdata/neuron/Neurites.am'
  expect_true(is.hxskel(p,bytes=readBin(p,what=raw(),n=80)))
})

test_that("reading identical neuron in 2 Amira formats and 3 encodings works",{
  expect_warning(l<-read.neuron("testdata/neuron/testneuron_lineset.am"),
                 regexp = 'Data section 3 .* specifies radius')
  expect_is(l,'neuron')
  expect_equal(l,read.neuron("testdata/neuron/testneuron_am3d.am"),
               fieldsToExclude='NeuronName')
  expect_equal(l,read.neuron("testdata/neuron/testneuron_am3d_ascii.am.gz"),
               fieldsToExclude='NeuronName')
  expect_equal(l,read.neuron("testdata/neuron/testneuron_am3d.am.gz"),fieldsToExclude='NeuronName')
})

test_that("reading gzipped binary format AmiraMesh neurons works",{
  library(nat.utils)
  expect_true(is.gzip("testdata/neuron/testneuron_am3d_ascii.am.gz"))
  expect_true(is.gzip("testdata/neuron/testneuron_am3d.am.gz"))
  expect_false(is.gzip("testdata/neuron/testneuron_am3d.am"))
})

test_that("we can identify Amira hxlineset neurons",{
  # hxlineset neuron
  expect_true(is.hxlineset('testdata/neuron/EBT7R.am'))
  # swc neuron
  expect_false(is.hxlineset('testdata/neuron/EBT7R.CNG.swc'))
  # hxskel neuron
  expect_false(is.hxlineset('testdata/neuron/Neurites.am'))
  # hxlineset via byte array
  p='testdata/neuron/EBT7R.am'
  expect_true(is.hxlineset(p,bytes=readBin(p,what=raw(),n=80)))
  q='testdata/neuron/Neurites.am'
  expect_false(is.hxlineset(q,bytes=readBin(p,what=raw(),n=80)))
  # gzipped direct
  expect_true(is.hxlineset('testdata/neuron/testneuron_fclineset.am.gz'))
  # gzipped via byte array
  # nb this relies on decompression which is looked after by getformatfuns
  r='testdata/neuron/testneuron_fclineset.am.gz'
  gzf=gzfile(r, open='rb')
  on.exit(close(gzf))
  expect_true(is.hxlineset(r,bytes=readBin(gzf,what=raw(),n=80)))
})

test_that("we can read a flycircuit lineset neuron w/o radius info",{
  f="testdata/neuron/testneuron_fclineset.am.gz"
  expect_warning(n<-read.neuron(f), regexp = 'No width data')
  expect_is(n, 'neuron')
})

test_that("we can update an existing neuronlist",{
  dir.create(td<-tempfile())
  owd=setwd(td)
  on.exit({setwd(owd); unlink(td,recursive=TRUE)})
  
  write.neurons(Cell07PNs[1:3], dir='.', format = 'swc')
  expect_is(nl3<-read.neurons(dir(pattern='swc$')), 'neuronlist')
  write.neurons(Cell07PNs[4], dir='.', format='swc')
  
  expect_message(nl4<-read.neurons(rev(dir(pattern='swc$')), nl = nl3, 
                                   SortOnUpdate = TRUE), 
                 '0 modified.* 1 new')
  # note that is the order of neurons specified in paths _not_ the order of
  # neurons specified in the neuron list that counts.
  expect_equal(names(nl4)[4:2], names(nl3))
  # overwrite the last file with a different neuron
  write.neurons(Cell07PNs[5], dir='.', format='swc', 
                files = names(Cell07PNs)[4], Force = T)
  expect_message(nl4<-read.neurons(dir(pattern='swc$'), nl = nl4),
                 '1 modified.* 0 new')
})

context("neurons writing")

test_that("neuron write without filename",{
  y=Cell07PNs[[1]]
  td=tempfile()
  on.exit(unlink(td,recursive=TRUE))
  if ("InputFileName" %in% names(y)){y$InputFileName <- NULL}
  expect_error(write.neuron(y, dir=td, MakeDir = F),
               'No file specified and neuron does not have an InputFileName')
})

test_that("neuron writing format",{
  y=Cell07PNs[[1]]
  td=tempfile()
  dir.create(td)
  on.exit(unlink(td,recursive=TRUE))
  #Try writing with default format
  expect_warning(f <- write.neuron(y, dir=td, MakeDir = F),'write.neuron: using default format="swc"')
  expect_equal(paste0(y$NeuronName, '.swc'),basename(f))
  
  f <- write.neuron(y, dir=td, MakeDir = F, format="rds")
  expect_equal(paste0(y$NeuronName, '.rds'),basename(f))
  #Try writing an incorrect format
  expect_error(f <- write.neuron(y, dir=td, MakeDir = F, format = 'zzz'))
})

test_that("we can write neuron/dotprops to rds file",{
  x=kcs20[[1]]
  td=tempfile()
  on.exit(unlink(td,recursive=TRUE))
  expect_error(f<-write.neuron(x, dir=td, MakeDir = F), 'does not exist')
  expect_equal(f<-write.neuron(x, dir=td), 
               file.path(td,'FruMARCM-M001205_seg002_03.rds'))
  # can't overwrite get a warning and an NA back
  expect_warning(f2<-write.neuron(x, f))
  expect_true(is.na(f2))
  # can overwrite with force
  expect_equal(write.neuron(x, f, Force=TRUE), f)
  unlink(f)
  
  expect_equal(write.neuron(x, dir=td, ext='.RDS'),
               file.path(td,'FruMARCM-M001205_seg002_03.RDS'))
  
  y=Cell07PNs[[1]]
  #expect_error(write.neuron(y, dir=td),'Ambiguous file format') #replaced as default format is now 'swc'
  expect_equal(write.neuron(y, dir=td, format='rds', ext='.RDS'),
               file.path(td,'EBH11R.RDS'))
  expect_equal(write.neuron(y, dir=td, format='rds', ext='_skel.rds'),
               file.path(td,'EBH11R_skel.rds'))
  
})

url_ok<-function(x) identical(httr::status_code(httr::HEAD(x)), 200L)


test_that("we can write neuron to swc file",{
  y=Cell07PNs[[1]]
  td=tempfile()
  dir.create(td)
  on.exit(unlink(td,recursive=TRUE))
  
  expect_equal(f<-write.neuron(y, dir=td, ext='.swc'),
               file.path(td,'EBH11R.swc'))
  
  expect_equal(f<-write.neuron(y, dir=td, format = 'swc', Force = TRUE),
               file.path(td,'EBH11R.swc'))
  swc_data <- read.delim(f, stringsAsFactors = FALSE)
  
  url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  
  m <- gregexpr(url_pattern, swc_data[[1]][1])
  swc_url <- regmatches(swc_data[[1]][1], m)[[1]]
  
  #Check if the URL exists
  if(nzchar(Sys.getenv("NAT_INTERNET_TESTS"))) {
    expect_true(url_ok(swc_url))}
  
  
  expect_equal(write.neuron(y, dir=td, format='swc', file='rhubarb'),
               file.path(td,'rhubarb.swc'))
  expect_equal(write.neuron(y, dir=td, format='swc', ext='.swcreally', file='rhubarb'),
               file.path(td,'rhubarb.swcreally'))
  expect_equal(f<-write.neuron(y, dir=td, format='swc', ext='_skel.swc'),
               file.path(td,'EBH11R_skel.swc'))
  expect_equal(read.neuron(f),y,fieldsToExclude='NeuronName')
  
  
  
  # construct a neuron with point ids in the wrong order
  z=y
  set.seed(42)
  z$d$PointNo=as.integer(sample(nvertices(y))^2)
  z$d$Parent[-1]=z$d$PointNo[z$d$Parent[-1]]
  write.neuron(z, dir=td, file = file.path(td,'EBH11R-fixed.swc'), 
               normalise.ids=T, Force=T)
  expect_equivalent(tools::md5sum(file.path(td,'EBH11R-fixed.swc')), 
               tools::md5sum(file.path(td,'EBH11R.swc')))
})

test_that("we can write dotprops objects to SWC format",{
  
  # setup
  td=tempfile()
  dir.create(td)
  on.exit(unlink(td,recursive=TRUE))
  
  # write, testing out veclength param to double segment length
  veclength=2
  expect_is(written <- write.neurons(kcs20[1:3], dir=td, files = Name, 
                                     format='swc', veclength=veclength),
            'character')
  expect_is(x <- read.neuron(written[1]), 'neuron')
  # The key feature is that the parents are set up properly
  # for head to tail segments
  expect_equal(x$d$Parent[1:6], c(-1, 1, -1, 3, -1, 5))
  # and that the segments define the original tangent vectorS
  # multiplied by veclength as appropriate
  seg1=data.matrix(xyzmatrix(x)[1:2,])
  vec1=as.vector(diff(seg1))
  expect_equivalent(vec1/veclength, kcs20[[1]]$vect[1,])
})

test_that("we can write neuron to Amira hxskel file",{
  y=Cell07PNs[[1]]
  td=tempfile()
  dir.create(td)
  on.exit(unlink(td,recursive=TRUE))
  
  expect_equal(f<-write.neuron(y, dir=td, format='hxskel'),
               file.path(td,'EBH11R.am'))
  expect_equal(read.neuron(f),y,fieldsToExclude='NeuronName')
})

test_that("we can write neuron to Amira hxlineset file",{
  y=Cell07PNs[[1]]
  td=tempfile()
  dir.create(td)
  on.exit(unlink(td,recursive=TRUE))
  
  expect_equal(f<-write.neuron(y, dir=td, format='hxlineset'),
               file.path(td,'EBH11R.am'))
  expect_equal(read.neuron(f),y,fieldsToExclude='NeuronName')
  
  y$d$W[2]=NA
  expect_warning(write.neuron(y, file=file.path(td,'EBH11R_narad.am'), format='hxlineset'))
})

test_that("we get an error when writing neuron to unknown format",{
  expect_error(write.neuron(Cell07PNs[[1]], dir=td, format='rhubarb'))
})

test_that("write.neurons works",{
  td=tempfile()
  dir.create(td)
  on.exit(unlink(td,recursive=TRUE))
  neurons_to_write=subset(Cell07PNs,Scored.By%in%c("ACH","CJP"),rval='names')
  expect_is(written_files<-write.neurons(Cell07PNs, dir=td,
                                         INDICES=neurons_to_write,
                                         format='hxlineset'),'character')
  files_found=dir(td,recursive=T,pattern='am$')
  expect_true(all(basename(written_files)%in%basename(files_found)))
  
  expect_is(written_files<-write.neurons(Cell07PNs, dir=td, subdir="CellType",
                                         INDICES=neurons_to_write,ext='.am3d',
                                         format='hxskel'),'character')
  files_found=dir(td,recursive=T,pattern='am3d$')
  expect_true(all(basename(written_files)%in%basename(files_found)))
  expect_equal(with(Cell07PNs[neurons_to_write],as.character(Glomerulus)),
               basename(dirname(written_files)))

  expect_is(written_files<-write.neurons(Cell07PNs, dir=td, subdir="CellType",
                                         INDICES=neurons_to_write, files=basename(TraceFile),
                                         ext='.am3d', format='hxskel', Force=T),
            'character')
  files_found=dir(td,recursive=T,pattern='am3d$')
  expect_true(all(basename(written_files)%in%basename(files_found)), 
              'specify output file names directly')

  expect_is(written_files<-write.neurons(Cell07PNs, dir=td, subdir="CellType",
                                         INDICES=neurons_to_write, files=basename(TraceFile),
                                         ext='.am3d', format='hxskel', Force=T),
            'character')
  files_found=dir(td,recursive=T,pattern='am3d$')
  expect_true(all(basename(written_files)%in%basename(files_found)), 'specify files')
  
  expect_is(written_files<-write.neurons(Cell07PNs, dir=td, subdir=Glomerulus,
                                         INDICES=neurons_to_write,ext='.amm',
                                         format='hxskel'),'character')
  files_found=dir(td,recursive=T,pattern='amm$')
  expect_true(all(basename(written_files)%in%basename(files_found)))
  expect_equal(with(Cell07PNs[neurons_to_write],as.character(Glomerulus)),
               basename(dirname(written_files)))
  
  nldf=subset(Cell07PNs, neurons_to_write, rval="data.frame")
  nl=Cell07PNs[neurons_to_write]
  attr(nl,'df')=NULL
  expect_is(written_files<-write.neurons(nl, dir=td,
                INDICES=neurons_to_write,
                subdir=nldf$Glomerulus,format='swc'),'character',
            info='use variable from calling environment to specify subdir')
  files_found=dir(td,recursive=T,pattern='swc$')
  expect_true(all(basename(written_files)%in%basename(files_found)))
})


context("vtk-io")

test_that("we can write a neuron to vtk format", {
  testd=data.frame(PointNo=1:6,Label=2L,
                   X=c(1:5,3),Y=c(rep(1,5),2),Z=0,W=NA,
                   Parent=c(-1,1:4,3))
  testn=as.neuron(testd)
  tf <- tempfile(fileext = '.vtk')
  write.vtk(testn, tf)
  expect_true(length(readLines(tf))>10)
})
