pdf_path =  test_path('Rplots.pdf')

# check for write permission
if(isTRUE(file.access(pdf_path, mode = 2)==0))
  file.remove(pdf_path)

ffnew=dir(test_path('testdata'), pattern = '\\.swc$', recursive = T,
          full.names = T)
if(isTRUE(all(file.access(ffnew, mode = 2)==0))) {
  file.remove(ffnew)
}
