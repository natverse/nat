# patch over an issue with trailing separators on Windows
file.exists <- function(...) {
  x=unlist(pairlist(...), use.names = FALSE)
  if (.Platform$OS == "windows") {
    x=sub("[/\\]$","", x)
  }
  base::file.exists(x)
}

# hidden
nat_progress <- function (x, max = 100, message = NULL) {
  percent <- x / max * 100
  cat(sprintf('\r|%-50s| ~%d%% %s',
              paste(rep('+', percent / 2), collapse = ''),
              floor(percent), message))
  if (x == max)
    cat('\n')
}
