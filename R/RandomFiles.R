

#' Randomly sample files in a directory
#'
#' RandomFiles will randomly sample files from a directory and optionally its internal folders
#'
#' RandomFiles was designed to randomly sample systematic photos of a sampled area for analysis.  It is generalized to use any file extension
#' a user inputs.  Files are copied and a lookup table is generated to crosswalk original and new names.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/cfrost3/AKaerial}
#'
#' @param path The path to the folder containing files to be sampled (or folders of files).
#' @param percent_number The percent (if less than 1.0) or raw number (if greater than 1.0) of files to sample.
#' @param pattern The file extension of the requested sample.
#' @param folders Should nested folders also be searched?  TRUE/FALSE
#' @param results The folder path for the copied (sampled) output
#'
#' @return copies randomly selected files to results directory and writes a lookup key \code{selected_files.csv}
#'
#' @examples
#'  RandomFiles(path = "C:/DATA/PHOTOS", percent_number = 0.10, pattern = "jpg$|JPG$", folders = TRUE, results = "C:/RESULTS")
#'
#' @export
random_files <- function(path, percent_number, pattern = "jpg$|JPG$", folders=TRUE, results="Q:/Waterfowl/STEI_Survey/Data/RandomFiles"){
  ####################################################################
  # path = path to folder with files to select
  #
  # percent_number = percentage or number of files to select. If value is
  #   between 0 and 1 percentage of files is assumed, if value greater than 1,
  #   number of files is assumed
  #
  # pattern = file extension to select. By default it selects jpg files. For
  #   other type of files replace jpg and JPG by the desired extension
  ####################################################################

  # Get file list with full path and file names
  files <- list.files(path, full.names = TRUE, pattern = pattern, include.dirs = TRUE, recursive=folders)
  file_names <- list.files(path, pattern = pattern, recursive=folders)

  # Select the desired % or number of file by simple random sampling
  randomize <- sample(seq(files))
  file_sample <- files[randomize]
  name_sample <- file_names[randomize]
  if(percent_number <= 1){
    size <- floor(percent_number * length(files))
  }else{
    size <- percent_number
  }
  file_sample <- file_sample[(1:size)]
  name_sample <- name_sample[(1:size)]

  # Create folder to output
  results_folder <- results
  dir.create(results_folder, recursive=TRUE)

  # Write csv with file names

  changes=data.frame(Original=file_sample, Copy=paste0(results_folder, "/", basename(file_sample)), New=paste0(results_folder, "/", seq(1:length(name_sample)),".jpg"))

  write.csv(changes, file = paste0(results_folder, "/selected_files.csv"),
              row.names = FALSE, quote=FALSE)

  # Copy files
  for(i in seq(file_sample)){

    file.copy(file_sample[i], results)

  file.rename(from = as.character(changes$Copy[i]), to = as.character(changes$New[i]))
  }



}
