setup({
  ff=dir(test_path('testdata'), pattern = '\\.swc\\.txt$',
         recursive = T,  full.names = T)
  ffnew=sub("\\.txt$","",ff)
  file.copy(ff, ffnew)
})
