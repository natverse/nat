check_reticulate <- function() {
  if (!requireNamespace('reticulate'))
    stop("Please install suggested reticulate package!")
}

check_cloudvolume_reticulate <- memoise::memoise(function() {
  check_reticulate()
  tryCatch(
    cv <- reticulate::import("cloudvolume"),
    error = function(e) {
      stop(
        call. = F,
        "Please install python cloudvolume module as described at:\n",
        "https://github.com/seung-lab/cloud-volume#setup\n",
        "This should normally work:\n",
        "pip3 install cloud-volume\n\n",
        "If you have already installed cloudvolume but it is not found\n",
        "then do:\nusethis::edit_r_environ()\n to point to the right python\n",
        'e.g. RETICULATE_PYTHON="/usr/local/bin/python3"'
      )
    }
  )
  cv
})

# convert neuron to precomputed format
neuron2precomputed<-function(x, file,...){
  #Step 1: Check if cloud volume exists..
  cv <- check_cloudvolume_reticulate()
  #Step 2: Instantiate the skeleton class in cloud volume..
  skel <- cv$Skeleton
  #Step 3: convert neuron to swc format..
  swcfilename <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".swc")
  write.neuron(x, file = swcfilename, Force = T, format = 'swc',...)
  swcfiledata <- readr::read_file(file=swcfilename)
  
  #Step 4: convert swc to precomputed format..
  skeldata = skel$from_swc(swcfiledata) # decode an SWC file
  skeldata_precomputed = skeldata$to_precomputed()
  
}
