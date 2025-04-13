#' @importFrom readr read_table
#' @importFrom utils download.file
#' @importFrom utils unzip
#'
.onLoad <- function(libname, pkgname){
  options(timeout = 500)

  url <- "https://www.dropbox.com/scl/fi/v6qv87s47w2ptfgbvcs60/glove.6B.50d.txt?rlkey=l1p7h8a84pawesol01ooje9a9&st=57lcgmhi&dl=1"
  temp_file <- tempfile(fileext = ".txt")
  download.file(url, temp_file, mode = "wb")

  glove_6B_50d <- readr::read_table(temp_file, col_names = FALSE)
  glove_vectors <- glove_6B_50d[,-1]
  rownames(glove_vectors) <- glove_6B_50d$X1
  glove_vectors <- as.matrix(glove_vectors)

  assign("glove_vectors", glove_vectors, envir = .GlobalEnv)
  unlink(temp_file)
}


