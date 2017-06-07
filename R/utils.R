# patch over an issue with trailing separators on Windows
file.exists <- function(x) {
  if (.Platform$OS == "windows" && grepl("[/\\]$", x)) {
    x=dirname(x)
  }
  base::file.exists(x)
}
