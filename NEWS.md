# nat 1.10.4

* `xyzmatrix<-` can now set list columns containing coordinates in forms like
  `list(c(1,2,3), c(4,5,6)) (#485)
* a new function `xyzmatrix2list()` is added to achieve this output format
  directly
* `xyzmatrix()` can now extract coordinates from a 3-vector like `c(1,3,5)` 
  (#486)

https://github.com/natverse/nat/compare/v1.10.3...v1.10.4

# nat 1.10.3

Some new features, speed-ups and bug fixes including from @dokato and @jonmarty. Thanks to @PostPreAndCleft and @artxz for bug reports.

* xyzmatrix can now get and set character columns containing coordinates in forms like "(1, 6, 3)" or "1 6 3" (#449)
* also a bug fix for the same (#451)
* Improve reading of Simple Neurite Tracer fills (#448)
* Fix bug `pan3d()` not found (#447)
* Added support for multi material binary Amira surface files while fixing
  "Bad triangle numbers" error in `read.hxsurf()` (#445)
* `reroot` function added with support for neuronlist (#463, @dokato).
* Add `xform()` and `xyzmatrix<-()` methods for `mesh3d` objects
* Don't clean `mesh3d` objects read from ply files by default
* `summary.neuron` now prints number of subtrees (#462, @dokato)
* fixes in `xyzmatrix.list()` (#484)
* new `c.hxsurf()` method (#482, @dokato
* Teach `write.neuron()` and `write.neurons()` to put metadata in SWC header 
* switch to GitHub actions
* speed-ups for `simplify_neuron()` in part through new
  [natcpp](https://cran.r-project.org/package=natcpp) package (#472,#473,#474)
* Fix Strahler order fails on a simplified neuron with no branches (#464 @dokato)
* add function for rerooting neuron `reroot.neuron()` and `reroot.neuronlist()` (#463, @dokato)
* New `neuronlistz()` fast access to neuronlists stored as zip files (#456)
* print method for `neuronlistz()` (#468, #466 @dokato)
* pointsinside: defer check for Rvcg until committed to use it (#455,454)
* Zip command line character limit error fix (#452 @jonmarty)

https://github.com/natverse/nat/issues?q=closed%3A2020-09-11..2021-09-20+

# nat 1.10.2

This release brings support for reading, plotting and writing 3D surface meshes
of neurons into line with the skeleton support. It also includes an essential
fix requested by CRAN.

https://github.com/natverse/nat/issues?q=closed%3A2020-02-23..2020-09-11+

* Fix test failure on R-devel (R 4.1) for `c.neuronlist` (#444)
* Add `print.neuron()`, `print.neuronlist()`,`print.dotprops()` methods #443
* Better support for reading / plotting neuron meshes (#441, #442)
* Bug in `xyzmatrix()` for 0 row data.frames (#433)
* Updated parameter for toggling grid lines in plotly (#432)
* Add support for Amira hxsurf binary format to `read.hxsurf()` (#431)
* improve error message for `cmtk.version()` (#430)
* Remaining items to move from elmr (#428)
* Turn off grid lines when using plotly engine (#427)
* Move features to nat from other packages (#423)
* Add support for thin plate spine transformations via `tpsreg()` (#289)

# nat 1.10.1

We are bumping a whole version point because of a large number of new functions.
Note that 1.10.0 was never formally released.

* Add functions from elmr package including 
  pruning: `prune_twigs()`, `simplify_neuron()`, 
  stitching neuron fragments: `stitch_neuron()`, `stitch_neurons()`, `stitch_neurons_mst()`, 
  thin plate splines registration: `tpsreg()`
  (#423 by @SridharJagannathan)
* Add large collection of morphometry functions (#404 by @alexanderbates)
* Add collection of pruning functions (#403 by @alexanderbates)
* remove `overlap()` which duplicates `overlap_score()` (#426)
* Revamp `pointsinside()` (#353)
* Use progress package for auto progress bars (#275)
* Implement 3d-wireframe plot in plotly (#421 by @SridharJagannathan)
* Teach dotprops.character to handle multiple input files (#420)
* Add `xyzmatrix.shapelist3d()` method (#419)
* `xyzmatrix.mesh3d()` should have colnames XYZ (#418)
* Teach `write.neuron()` to set label column of soma when writing SWCs (#415)
* Setting fig size missing in vignette plotly.Rmd (#417 by @mmc46)
* Fix as.seglist.igraph is slow #425
* Neuron: fix use of "ties" parameter in `approx()` call bug (#416)
* Fix scroll to zoom does not work using `nopen3d()` (#413)

# nat 1.9.2

* switch to https://github.com/natverse/nat
* Support for plotly graphics engine (#409, #410)
* Teach dotprops.character to handle multiple input files (#420)
* Fix bug in nmapply with progress (#412)
* Fix warning in resample.neuron when there are identical points (#405)
* Fix fig size missing in vignette plotly.Rmd (#417)
* Add `xyzmatrix()`, `xyzmatrix<-()` and`nvertices()` methods for `shapelist3d`
  thereby enabling these rgl objects containing multiple meshes to be 
  transformed. Currently most relevant when fetching neuroglancer meshes (#419).
* `xyzmatrix.mesh3d()` now has colnames XYZ for consistency (#418)

# nat 1.9.1

* Fix very slow loading of neurons in `neuronlistfh()` objects. 
  Apparently due to a change in behaviour in R 3.6.0 (#402).
  Thanks to Shanice Bailey and Lisa Marin for the bug report.

# nat 1.9.0

We are bumping a whole version point because of some changes in the default
behaviour of nat. We do not expect to submit this first version in the 1.9.x
series to CRAN without further testing, but it is our current recommended
version for general use.

Significant changes:

* read.neurons will name neurons in the neuronlist that it returns by their
  filename after removing the file extension. So the neuron read from file
  "n1.swc" will now be named "n1" not "n1.swc". (#383)

* write.swc normalises SWC files to maximise external compatibility.
  The canonical SWC form gives each point a numbered identifier increasing from
  1 to the number of points. However many SWC files in the wild do not respect
  such an ordering. The new behaviour will result in cases where SWC files will
  be written out with a different point ordering from earlier versions of nat;
  nevertheless we prefer this behaviour since it is a source of confusion for
  new users since many external programs cannot read SWC files where this is not
  the case. The original behaviour can still be requested (#358).

* The use of operators e.g. adding or multiplying neurons by a constant is now
  provided by the Ops approach (see ?groupGeneric in main R help). This is a
  fairly large change under the hood, but should not have any user visible
  effects other than the fact that it is now possible to use the same
  functionality for hxsurf and mesh3d objects.

New vignettes:

* Working with Individual Neurons as Graph Structures
* NeuroGeometry: Analysing 3D Morphology of Neurons

Additional changes:

* Support for CMTK deformation fields 
* Always pass on ... for reglists in `xform()` and friends. This ensures that
`nat.templatebrains::xform_brain` can fall back to an affine registration when a
non-rigid registration fails (#365)
* Fix thinko in re-ordering vertex ids for SWC files (#366)
* Add `makeboundingbox()` for constructing a boundingbox explicitly rather than from
  an object. This allows `boundingbox()` to work for matrices of 3D points (#222)
* Export `nrrd.datafiles()` + nrrd doc fixes (#316)
* Teach `materials()` to return col for amiramesh/im3d (#284)
* Fix `nview3d()` "oblique_right"
* Pass ... on to spheres3d to plot somata with alpha transparency (#370)
* `as.data.frame.neuronlist()` makes autonames when none present (#371)
* Check that `ngraph()` edgelist references valid vertices to avoid segfaults in 
  igraph library (#363)
* Fix bug in constructing `im3d()` with image bounds (#205)
* Allow dotprops objects to be plotted in 2D (#112)
* Speed up `spine()` by only considering endpoints (#372)
* Export `graph.nodes()` and improve docs for root points (#373)
* Speed up `pointsinside()` by checking if inside bounding box of surface mesh 
  (progress on #353)
* Teach `read.neurons()` to read remote zip file (#381)
* Fix `invert_reglist()` for mix of swapped/unswapped regs
* Fix bug in `write.im3d()`/`write.nrrd()` for raw detached nrrd (#384)
* Export `is.im3d()`

# nat 1.8.12

* Speed up (2-3x) summary.neuron(list) by faster calculation of total cable
  length. Note that this may cause some changes in the reported cable length
  because all cable is now included (as it should always have been). (#361)
* fix bug: dotprops do not get resampled labels bug (#360)
* Add nview3d function to set anatomical viewpoints (#359)

# nat 1.8.11

Fixes
* pointsinside fails for points >1e5 from mesh (#354)
* as.neuron should work for an igraph made from an ngraph (#356)
  this is helpful if you e.g. delete nodes/edges from an ngraph object
* breaking change with testthat v2 #355

# nat 1.8.10

* pointsinside now works with alpha shape (ashape3d) objects directly (#350)
* On windows add cygwin binary directory to path if not present 
  (fixes errors with missing cygwin DLLs)
* workaround for some functions (eg those using CMTK registration folders) that
  were affected by file.exists("/some/real/registration.list/") returning FALSE
  on Windows
* Fix cmtk.reformatx under Cygwin - output NRRD file path (but not other paths)
  must be munged for reformatx - not sure why this isn't handled internally.
* Fix as.mesh3d.ashape3d messing up the order of triangle vertices 
  which was giving problems with pointsinside (#349)
* Fix pointsinside to return TRUE for vertices defining mesh (with distance=0)
  (#348)
* Switch pointsinside to use Rvcg::vcgClostKD, thereby fixing some cases
  where points were incorrectly reported to be inside a surface. (#352)

# nat 1.8.9

* fix reading of neurons when origin is not an endpoint (#342)
  nb this bug was introduced in 1.8.8
* fix edge case for is.neuron when applied to tibbles (#338)
* add PlotSubTrees option for plot.neuron (#339)
  thanks to Zhihao Zheng

# nat 1.8.8

* add xyzmatrix<-.neuronlist so that we can replace the vertices of a set of 
  objects in one go (#328)
* add as.mesh3d.ashape3d thereby adding support for 3D alpha surfaces generated 
  by the alphashape3d package (#337)
* add as.hxsurf to convert surfaces (#332)
* add nvertices as generic method to find the number of vertices in an object 
  (#329)
* add plot3d.cmtkreg to visualise domain of CMTK registration (#333)
* speed up resample function (and seglist2swc) (#336)
* add an option to override default progress bar behaviour for nlapply (and many
  other functions that use it) (#322)
* make plot3d more forgiving of colour specification (#331)
* fix overwrite=T option for landmarks files (#319)
* fix prune_edges for paths with opposing edge directions (#320)
* make read.amiramesh cope with different NA values (#324)
* fix crashing bug in read.neuron.hxlineset for malformed files (#325)
* only warn if xform.neuronlist fails to transform soma positions (#326)

# nat 1.8.7

This release includes some bug fixes and significantly improved online package
documentation visible at https://natverse.github.io/nat/ including two vignettes.

* Teach xform and friends to transform soma positions (#206)
* Copy attributes (including templatebrains) of neuronlists when subsetting (#310)
* Fix error in read.amiramesh for RLE encoded files. (#317)
  (reported by K. Hornik)
* replace nat::trim with base::trimws (#313)

# nat 1.8.6

* Add xform.data.frame method (#309)
* Add invert_reglist function (#308)
* Fix bug in imslice storing position of new singleton dim (#306)
* Fix namespace issues for smooth_neuron

# nat 1.8.5

The main feature of this release is improved support for nat+CMTK on Windows.

* teach voxdims.character to get voxel dimensions straight from image file on 
  disk enhancement (#303)
* teach coord2ind to accept nat.templatebrains objects for imdims (#302)
* add simple smooth_neuron function (#300)
* fix bug reading amira surfaces when Color precedes Id (#305)
* cmtk.reformatx needs to use system2 (#301)
* Don't use shell features on Windows (#295)

# nat 1.8.4

* fix  build failures with testthat >=0.12 (#293)
* ensure that nat still works with igraph <1.0 (#298)
* fix read.hxsurf failure when a region is not listed in the Parameters section (#291)
* Protect find.neuron from trying to filter with a neuronlist (#299)
* Fix simplification of reglist containing one cmtkreg (#297)
* Don't lose swap attribute when >1 cmtk registrations in reglist (#296)
* Documentation improvements for neurons and file formats

# nat 1.8.3

* nat now handles compound registrations via reglist objects (#286).
  These can contain homogeneous affine, CMTK registrations (in disk or in memory)
  and R functions (which can be used to wrap arbitrary registration types not 
  directly supported by nat). NB xformimage.reglist will currently only work for 
  CMTK compatible registrations.
* add mask(.im3d) function to zero out parts of an image (#285)
  Looks after im3d attributes and material name to integer pixel level mapping.
* add read.ngraph.swc which can be used to read even malformed SWC files (such
  as those containing cycles) (#282). 
  This is exposed by giving read.neuron an argument class, which can be set to 
  'ngraph' instead of 'neuron'.
  Inspired by https://github.com/BigNeuron/BigNeuron-Wiki/wiki/BigNeuron-Imperial-College-London-Hackathon-Discussion-Notes
* add prune_edges to delete by specifying neuron edges rather than vertices (#280)
* Give give spine an invert option (#279) using prune_edges
* Teach spine to return point ids (#278)
* fix bug in pointsinside for distant points (#290)
* fix subsetting neuronlists with a single column data.frame drops column name
  (#276)
* Fix invert option of subset/prune_vertices returns an error with igraph::dfs 
  points (#288)
* Fix fileformats(, rval='info') to return a well-formatted data.frame
* The idiom neuronlist[,] will never drop columns (since it is a useful shortcut
  for as.data.frame(neuronlist)) (#277)

# nat 1.8.2

* roll back rgl NULL mode changes from 1.8.0 in favour of less invasive approach
  of marking the majority of rgl based plotting examples as donttest (#274)

# nat 1.8.1

* fix non-canonical URLs to CRAN packages (thanks to Uwe Ligges)

# nat 1.8.0

This is tagged as a major release because of a change in behaviour that (by 
default) suppresses interactive rgl windows when running in batch 
(non-interactive) mode. See ?nat "rgl Package" section for details.

* nat will only activate an rgl display in interactive mode, otherwise 
  defaulting to rgl's NULL mode display. This should help run nat in batch mode
  situations including knitr runs, the CRAN build server etc. (#272)
* Add summary.neuron(list) functions to calculate tree statistics such as number
 of nodes and cable length for neurons/neuronlists (#269)
* Automate progress bar in nlapply - this will always show in interactive mode
  when there are more than 10 neurons being processed. (#271)
* Fix bug in c.neuronlist when combining more than 2 neuronlists (#270)
* Fix bug in read.neuronlistfh on Windows (#268)
* Fix bug when constructing an ngraph with a single node (#267)
* Fix bug in `[<-.neuronlist` when assigning a whole data.frame using (#256)
* Fix bug in bounding box with input = "bounds" (#273)
* Documentation improvements (especiall package overview - see ?nat)

# nat 1.7.2

* new function subset.neuron can keep/remove individual vertices from a neuron 
  based on internal variables such as position, width, vertex number etc while
  still producing a valid neuron with an appropriate graph structure (#261)
* this depends on a new low level function prune_vertices that can remove 
  specified vertices
* this has also enabled the prune.neuron method to be defined for the first time
  enabling pruning by spatial proximity ()
* teach mirror to work with image data (#265) and enable better handling of the 
  mirrorAxisSize by allowing this to be supplied as a bounding box (#254)
* teach as.im3d.matrix to use an im3d object to specify the desired space (#263)
  This enables for example a set or tracings to be turned into volume data that
  exactly matches a specific template brain.
* fix bug: seglengths fails for neurons with multiple trees bug (#257)
* fix bug: xformpoint.cmtkreg fails for in memory registration lists (#259)

# nat 1.7.1

* new functions strahler_order and prune_strahler to calculate the Strahler 
  order of points in a neuron and to remove lower order (terminal) branches.
* fix is.cmtkreg for non-existent files (and better handling of status messages)
* export segmentgraph function for end users
* enable segmentgraph to return segment ids

# nat 1.7.0

* implement extraction/replacement of the data.frame attached to a neuronlist
  using the [ operator (#217)
* fix xform of image data (#143, #199)
* implement handling of inverted (swapped i.e. reference to sample) 
  registrations in xform(image) (#199)
* normalise handling of registration sequences and inversion in 
  xform/image/points (all 3 expect reg to be in sample to ref order and for any
  individual registration to be marked with swap=T if it is ref->sample)
* read.nrrd no longer returns an im3d (use read.im3d) (#238)
* teach write.nrrd to handle more diverse inputs (including #242)
* add reading of detached nrrds (#236, #237)
* add ability to write detached nrrds
* add ability to make a detached nrrd from a compatible amira file on disk (#243)
* new function is.im3d
* fix cmtk.targetvolume for templatebrain objects (#241)
* fix cmtk.targetvolume for --target-grid specification (#240)
* fix names for [.neuronlistfh with null df (#250)
* plot.neuron can cope with some NA coordinates
* teach plot.neuron(list) to cope with NA points (#247)
* boundingbox has an na.rm argument (#246)
* teach droplevels.neuronlist to use extra arg (#249)
* teach plot.neuron to plot soma (#245)
* fix cmtk.call for numeric args (#235)
* set read.hxsurf RegionChoice="both" by default
* document return values of cmtk.statistics in more detail (#252)
* support for igraph 1.0 (#248)

# nat 1.6.6

* Fixes description capitalisation/quoting for CRAN
* Adds ability to write dotprops neuron objects to SWC format (#233)
* fixes bug in xyzmatrix.neuron for single point neurons (#234)
* likewise for xyzmatrix.default for 1-row >3-col matrices

# nat 1.6.5

* add xyzmatrix get/set methods for class mesh3d along with xform.shape3d. This
  should enable rgl::mesh3d objects (used in a number of other packages) to be
  transformed using CMTK registrations.
* add tail.neuronlist
* add as.data.frame.neuronlist for convenient generation of a data.frame from a
  neuronlist
* add help topics for generics subset and plot3d so that e.g. ?subset will give 
  the option to see our documentation as well as base docs.
* subset.hxsurf drops unused vertices by default
* add sample surface object of mushroom body
* teach cmtk.statistics to handle imagetype  'label' (#221)
* plot.neuronlist sets axes based on boundingbox of all plotted neurons, not just
  the first neuron (#223)
* plot.neuron(,AxisDirections) argument is re-implemented (#224)
* don't warn when read.neuronlistfh changes the directory location on disk (#157)
* remove superimposed plots in mirror example (hope this will fix build on 
  MacOS X Snow Leopard)
* teach read.neurons to OmitFailures when reading in a neuronlistfh object.
* fix handling of default colours in plot.neuronlist and plot3d.neuronlist (#230)
* fix plot3d.neuronlist returns one element per neuron
* fix bug in xformpoints.cmtkreg when some points could not be transformed - 
  now give NAs as expected (#227)
* fix bug when xform(na.action='drop') dropped dimensions when only 1 valid 
  point resulted (#228)
* teach xyz.matrix<- to work for bare 3 column matrices (i.e. without colnames)
* fix bug in xformimage where input image was not being passed on when 
  transforming with a matrix.
* read.hxsurf can accept RegionChoices='both' (see ?read.hxsurf)
* fix: unary -.neuronlist behaves
* docs: numerous minor doc/example improvements
* dev: improvements in test coverage (up to 84%) catching numerous small bugs in
  the process.

# nat 1.6.4

* Edits to package description for CRAN

# nat 1.6.3

* fix invalid url in README.md noticed by r-devel/BDR

# nat 1.6.2

* add as.im3d.matrix which allows conversion of a Nx3 matrix of coordinates into
  a volume representation
* add ability to use a neuron's StartPoint in the spine function that finds the 
  longest path (aka backbone or spine) of a neuron
* fix xformimage's handling of in-memory registrations
* fix bug in read.hxsurf docs
* dev: add coveralls code coverage support
* dev: fix use of require in package code (a new note from r-devel)
* dev: fix a cmtk-dependent test that was not guarded against cmtk's absence

# nat 1.6.1

* add ability to transform images (specified as path to a file)
* register image file formats to use with read/write.im3d
* register amira types including linesets, surfaces and landmarks
* add ability to read neurons from remote url
* add support for applying multiple registrations in xform.neuronlist, as well
  as vectorisation (where each registration is applied to a different object)

# nat 1.6.0

* Add support for reading NeuroML and Fiji Simple Neurite Tracer neurons
* recognise SWC neurons by content (rather than just file extension)
* can write a set of neurons to a zip archive (?write.neurons)
* getformatreader returns name of identified format
* Give find.soma and find.neuron invert arguments
* internal function normalise_swc sets sensible defaults for column values (and 
  these can be overridden from some user functions).
* fix resample.neuron so that it keeps all subtrees and handles width and labels
* fix seglist2swc when receiving flat seglist
* ngraph attaches vertex data to each point and now includes diameter
* fix is.nrrd for raw input
* fix node colours in plot3d.neuron when WithAllPoints=TRUE

# nat 1.5.14

* add nlscan function to review a set of neurons, optionally selecting a subset
* add find.soma function to select neurons whose cell bodies fall within an rgl
  selection box (thanks to Ben Sutcliffe).
* add setdiff, intersect and union generics with implementations for neuronlist
  objects (note this is done in more or less identical fashion to generics
  defined in the dplyr package).
* give write.neurons a format argument to make it more obvious that this is an 
  option rather than pointing people to the write.neuron docs (suggestion from 
  Ben Sutcliffe)
* teach as.neuronlist to handle inputs with empty (rather than NULL names)
* simplify calculation of inertia in dotprops
* minor doc fixes and clarifications

# nat 1.5.13

* mat2dof output gets descriptive rownames
* ndigest.neuronlistfh only uses 2 key fields (keyfilemap and df)
* fix bug in locating cmtk when only the cmtk wrapper script is in the path
  (spotted on neurodebian with /usr/bin/cmtk)
* fix bug in compound registrations when CMTK <3.2.2
* fix rgl import errors on r-devel
* switch from RANN to nabor package for finding nearest neighbours
* dev: update help to roxygen2 4.0.2

# nat 1.5.12

* fix bug in updating neuronlistfh objects from remote url
* nlapply now has options for progress bars and parallelisation by using the 
  plyr package under the hood.
* c.neuronlist can now join lists with attached data.frames with different 
  columns (missing values are filled with NAs). This also applies when some
  neuronlists have no attached data.frame at all.
* read.neurons can read neuronlist files from disk
* cmtk.statistics now has a Verbose argument
* warn if resampling a neuron with multiple subtrees that only the main subtree
  will be kept.

# nat 1.5.11

* add cmtk.statistics function
* fix infinite recursion in cmtk.targetvolume

# nat 1.5.10

* fix: write.neurons subdir argument can accept a column of the attached
  data.frame (unquoted)
* fix: write.neuron(s) can add an extension to an output filename that does not
  have one
* write.neurons now has an explicit files argument
* cmtk.targetvolume (and therefore cmtk.reformatx) can accept e.g. 
  templatebrains (as contained in nat.flybrains package) to specify target 
  image for registration.

# nat 1.5.9

* fix transformation of points using compound CMTK registrations
  (this was broken for CMTK<3.2.2, so a workaround is required).
* Add cmtk.version function.
* xyzmatrix can cope with lower case x,y,z for column names
* doc: fixes in xform

* dev: fix test for winbuilder

# nat 1.5.8

* Add boundingbox.list method to find boundingbox for neurons, surfaces etc
* nmapply and nlapply get OmitFailures arguments
* nlapply also gets a subset argument
* both arguments are also added to xform/mirror.neuronlist
* dotprops.neuronlist also gets the OmitFailures argument.
* fix subset.neuronlist handling of numeric indices
* minor bug/doc fixes

# nat 1.5.7

* teach plot3d.neuronlist and friends to plot soma locations.
* add npop3d function to remove last plotted neurons 
  (identical to flycircuit::pop3dfc, except that it works with any neurons
  plotted by plot3d.neuronlist, rather than only those plotted by plot3dfc)

# nat 1.5.6

* give mirror/xform.neuronlist subset arguments
* add plot3d.boundingbox
* give plot(3d).neuronlist a SUBSTITUTE argument that can be used to control
  non-standard evaluation. This is useful for people who wish to incorporate
  these functions inside other user-defined functions. See ?subset.neuronlist
  for details.
* fix: write.neurons into subdirs when df=NULL

# nat 1.5.5

* fix bug in reading swc files with many decimal places specific to R 3.1.0
* fix bug in header when writing swc files [ajdm]
* read.im3d now adds file as an attribute to output
* read.im3d can now add amiramesh files without the standard file extension
* doc: minor improvements to read.im3d docs and examples

# nat 1.5.4

* remove internal is.gzip function and depend on nat.utils >=0.4.2

# nat 1.5.3

* new function pointsinside to determine if points (e.g. neuron) are inside a
  closed surface
* resample.neuron implemented (no support for interpolating widths or multiple
  subtrees)
* dotprops.neuron can resample to a consistent segment length
* consistent return values for as.seglist and seglengths when all subtrees are
  returned
* seglengths can return lengths for each edge as well as the sum for each
  neurite segment (the default)
* suppress remote tests if flybrain unreachable
* zenodo DOI for package

# nat 1.5.2

* add seglengths function for neurons
* add segmentgraph function to produced a simplified graph representation of
  neurons with one edge per segment
* add potential_synapses.dotprops method
* add as.seglist.neuron method 
* teach plot3d.hxsurf and subset.hxsurf to accept regexes
* always drop unused vertices in as.mesh3d.hxsurf
* fix bug in write.neurons when subdir not specified
* fix colouring of vectors by plot3d.dotprops
* dev: switch to roxygen2 v4

# nat 1.5.1

* add potential_synapses (from nat.as)
* surfaces: add subset.hxsurf and as.mesh3d
* Teach read.im3d to read Vaa3d raw format
* add plot.neuronlist (for 2d plotting)
* add c.neuronlist function to combine neuronlists
* add db argument to plot3d.character
* make WithNodes=FALSE the default for plot3d.neuronlist
* make asp=1 the default for image.im3d
* write.cmtkreg warns if versions specified by cmtkreg attribute and argument
  differ (to avoid writing old registrations as if they were new or vice versa)
* fix: prune.neuronlist method signature (and therefore dispatch)

# nat 1.4.10

* add function to prune objects by removing points close to or far from another
  object
* add nmapply function for neuronlists
* add xyzmatrix method for neuronlists
* add materials function with methods for im3d and hxsurf objects
* enh: read.im3d can read materials from amiramesh files
* enh: download neuronlistfh objects to session temp directory if no localdir
  is specified
* fix: find CMTK in its new default location on macosx
* fix: plot3d should skip redrawing if par3d() skipRedraw is set
* fix: failure to export ngraph constructor function
* fix: set NeuronName when no InputFile argument is passed to neuron
  constructor
* fix: im3d axis labels
* doc: numerous small documentation improvements

# nat 1.4.9

* add spine() function to find longest path (aka backbone) of a neuron
* give ngraph (and therefore as.ngraph methods) a weights argument so that 
  resultant ngraph objects have edge weights defined by segment lengths.
* add as.im3d generic and as.im3d.im3d method
* fix bug in setting graph attributes in ngraph

# nat 1.4.8

* bugfix: ensure ind2coords.im3d is exported

# nat 1.4.7

* add ability to make a dotprops object directly from an image file on disk or
  an im3d object in memory.
* port ind2coord, coord2ind and sub2ind functions from AnalysisSuite
* fix clampmax function so that it removes Infinite values
* fix reading of gzip encoded amiramesh files
* fix mirror so that dotprops are recalculated for a simple flip
* make plot(3d).neuron colour settings more consistent
* simplify axis handling in plot.neuron
* dev: build nat.flybrains (as well as nat) after flycircuit build

# nat 1.4.6

* fix bug in handling vertexData argument in as.neuron.ngraph
* fix test error on Solaris (thanks to Brian Ripley)
* dev: fix remaining warnings in tests

# nat 1.4.5

* add plot.neuron for 2d plots (ajdm, from code in AnalysisSuite)
* add nopen3d() which opens an rgl viewer that allows panning. See ?nopen3d for
  details of how to use this.
* teach read.neurons to report files that have problems as they are read
* fix temp file left behind by xform.cmtkreg
* fix dangerous looking warning when amira binary files have empty sections
* fix ability of read.neurons to set InputFileName and therefore retain
  status information for the file along with its MD5 hash
* fix bug exporting xform.default (so that xform(matrix()) works)
* docs: improve coverage of BoundingBox vs bounds
* tests: simple tests for plot(3d).neuron
* tests: clear up temp files and suppress most warnings
* dev: restart travis build of flycircuit package when nat is pushed to github

# nat 1.4.4

* fix handling of logical expressions including NAs by subset.neuronlist
* add soma argument to plot3d.neuron
* fix: boundingbox.im3d() returns NULL when bounding box is not defined
* give remotesync.neuronlistfh an indices argument to allow download/update
  from the web of a selected population of neurons
* add ndigest.neuron method
* fix: export ndigest.dotprops method
* give image.im3d a useRaster option, which defaults to TRUE when possible.

# nat 1.4.3

* new ndigest (normalised digest) function to compute hash values. Methods for
  neuronlistfh and dotprops objects. Can be used e.g. to compute a hash value
  for a standard dps object to determine if a data.
* new threshold function with a method for im3d objects. Can be used to make
  masks.
* more flexible im3d constructor can use an existing im3d object to supply
  spatial attributes.

# nat 1.4.2

* fix bug in read.neuronlistfh resulting in error if a remote file was
  downloaded and then read back in (using cached copy) before any neurons were
  downloaded.

# nat 1.4.1

* fix bug in write.nrrd - was failing to write essential space dimensions field.
* make gzip the default encoding for nrrd images - this is pretty much always
  what one wants.

# nat 1.4.0

* add cmtk.reformatx command for reformatting images using CMTK registrations
* new function remotesync to synchronise a neuronlistfh object with its remote
  source.
* add write.neuronlistfh and improve consistency/docs of path handling in 
  read.neuronlistfh
* option to write missing (or no) neurons when using as.neuronlistfh.neuronlist
  this makes it _much_ faster to re-export these objects or prepare neuronlistfh
  for subsets of an existing group of neurons
* fix hashmap functionality of neuronlistfh (speeds up access to single neurons)

# nat 1.3.2

* fix bug in find.neuron for neuronlistfh objects
* fix bug in neuronlistfh constructor (wasn't adding hashmap) and 
  [[.neuronlistfh (was not searching properly for objects when using hashmap).

# nat 1.3.1

* add find.neuron for interactive selection of neurons in 3d.

# nat 1.3

* implementation of new im3d image manipulations functions including image.im3d,
  projection, unmask, imslice, imscalebar, imexpand.grid, flip, origin,
  boundingbox<-, xyzpos, ijkpos
* see ?im3d, ?boundingbox, ?image3d.im3d, ?imscalebar and ?flip for details
* fix reading of gzipped nrrds on windows
* dev: fix nocran tests

# nat 1.2

* Basic implementation of reading/writing NRRD format image data.
  See read.nrrd and write.nrrd for details.
* Basic implementation of reading/writing Amira format image data.
  See read.amiramesh and write.amiramesh for details.
* Basic implementation of a class for 3d images, im3d
* see read.im3d and write.im3d for I/O
* and voxdims and boundingbox methods for physical dimensions
* switch to more flexible neuronlistfh structure in which objects on disk are
  named by the md5 hash of their contents.
* workaround for connection leak in filehashRDS objects that stopped downloads
  of more than 124 new objects by a neuronlistfh (ajdm).
  (see https://github.com/rdpeng/filehash/pull/3 for details)
* dev: approach allowing some tests (e.g. those using the web) to be ignored on
  CRAN (ajdm)
* dev: travis continuous integration support (ajdm)

# nat 1.1

* Implemented reading and writing of groups of neurons from/to neuronlists.
  See read.neurons and write.neurons.
* Implemented writing of single neurons in swc format, Amira's HxLineSet and 
  SkeletonGraph formats, and R's rds format.
* Can now read Amira HxLineSet format neurons (see read.neuron)
* Give read.neuronlistfh an update argument (default:FALSE) so that it does not
  re-download remote data.
* dev: speed and flexibility improvements in fileformats ioregistry.
  see ?fileformats for details of new functions.

# nat 1.0.1

* fix bug in cmtk.bindir() on Solaris (thanks to Brian Ripley)

# nat 1.0

* implement reading of amiramesh data
* including amira SkeletonGraph format tracings
* switch to filehashRDS format for repositories of remote neurons
* retire stashR backend for neuronlistfh remote repositories (too slow)

# nat 0.9

* support for automatic download of neurons from stashR remote repositories on
  the web. ?neuronlistfh for details and an example. 
* Relies on stashR package
* dev: small doc fixes to make package CRAN compatible.

# nat 0.8

* important new feature: neuronlistfh objects backed by an on disk filehash
  allow single neurons to be loaded from disk on demand
* this allows 1) fast startup suitable for knitr documents and 2) working with
  more neurons than fit in main memory
* of course this is slower than working with an in-memory list but for plotting
  etc this should not be problem
* furthermore neuronlistfh objects can be subsetted/converted to give in memory
  neuronlist objects.

# nat 0.7

* implement read.neuron and read.neurons
* so far only works for rda, rds and swc files
* reading of additional file types will be implemented by registering functions =
  to test, read and write for the format using a package registry controlled by
  nat::neuronformats.
* developer: seglists in neurons now have class 'seglist'

# nat 0.6

* major new functionality - bidirectional interconversion of graph and regular
  neuron representation
* see as.neuron.* methods and as.ngraph.* methods
* plan is to use graph representation as the common intermediate with reading 
  different neuron file formats
* depends on igraph package

# nat 0.5.2

* add subset.neuronlist
* add methods for working with dataframe attached to neuronlist 
  (see e.g. ?with.neuronlist)
* TODO harmonise plot3d.neuronlist subset expressions with subset.neuronlist

# nat 0.5.1

* Switch license to GPL-3
* xform (transformation) of hxsurf objects
* fix xform of neuronlist objects
* switch from CMTK gregxform to streamxform (gregxform is deprecated; streamxform can concatenate registrations)
* teach mirror function to accept numeric axis specification
* Fix bug in checking extra directories in cmtk.bindir
* Add dotprops.neuronlist and include all dotprops methods in documentation
* Fix long-standing bug in parsing cmtk warping registrations (only relevant for
  reading these into R lists - no effect on transformations).
* fix bug in class of cmtkreg objects
* Remove call to .Internal(La_rs) by calling eigen directly
* doc: some notes on CMTK installation

# nat 0.5

* fix broken handling of cmtk affine transformations
* implement mirroring of neurons and other objects
* teach xformpoints (and therefore xform) to handle "~/" in paths
* bugfix in visibility of S3method xformpoints.character
* (developer) Switch to roxygen2 v3.0 for documentation

# nat 0.4

* read.hxsurf and plot3d.hxsurf for reading/plotting Amira surfaces
* better handling of location of cmtk command line tools see ?cmtk for details
* add neuronlist and associated methods (including plot)
* plot3d for neurons and dotprops

# nat 0.3

* basic and self-contained implementation of neuron class
* basic and self-contained implementation of dotprops
* new xform generic + methods for clean transformation of objects including
  neurons and dotprops objects
* this includes improved interaction between dotprops and xform so that xform
  can use the previously value of k when recalculating a dotprops object post
  transformation
* sample data (40 PNs from Jefferis, Potter et al. '07 and 20 KCs from Chiang 
  et al. 2011)
* 1 use of .Internal and missing nlapply remain to be fixed.

# nat 0.2.1

* fix install error due to missing neuron(*).R files that contain docs only so far

# nat 0.2

* Fairly complete implementation of CMTK registration I/O and geometry
* All functionality is self-contained (i.e. independent of nat.as/AnalysisSuite)
* passes check()
* can be augmented by nat.as (0.6)
