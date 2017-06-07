# patch over an issue with trailing separators on Windows
file.exists <- function(...) {
  x=unlist(pairlist(...), use.names = FALSE)
  if (.Platform$OS == "windows") {
    x=sub("[/\\]$","", x)
  }
  base::file.exists(x)
}
