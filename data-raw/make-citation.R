if(!require('rbibutils'))
  install.packages('rbibutils')

natverse.bib <- rbibutils::readBib('data-raw/natverse.bib')
citfile='inst/CITATION'
file.copy('data-raw/citheader.R', citfile, overwrite = T)
tf=tempfile()
rbibutils::writeBibentry(natverse.bib, file = tf)
file.append(citfile, tf)
unlink(tf)
