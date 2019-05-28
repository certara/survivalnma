prep_model_file <- function(model, type) {
  # grab the included WinBUGS model, write a temporary file
  file_loc <- tempfile()
  file.copy(
    system.file("bugs_models",
                model_filenames[[type]][[model]],
                package = "survivalnma"),
    file_loc)
  # ".temp_survnma_model")

  file_loc
}