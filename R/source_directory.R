#' Source all files in a directory
#' @param directory (character) the directory in which all files will be source
#' @export
source_directory <- function(directory="S:/ChenaSalchaSR2020/Rfunctions"){
  for (i in 1:length(list.files(directory))){
    source(paste(directory, "/", list.files(directory), sep="")[i])
  }
}
