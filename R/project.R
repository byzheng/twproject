
.get_meta_file <- function() {
    # Define the base directory using the `here` package.
    base_folder <- here::here()

    # Define the metadata storage folder.
    meta_folder <- file.path(base_folder, ".tw")

    # Create the metadata folder if it does not exist.
    if (!dir.exists(meta_folder)) {
        dir.create(meta_folder)
    }

    # Check again to ensure the folder was successfully created.
    if (!dir.exists(meta_folder)) {
        stop("Target folder does not exist: ", meta_folder)
    }

    # Define the path for the metadata file.
    meta_file <- file.path(meta_folder, "tw_meta.Rds")

    return(meta_file)
}


#' @title Cache Metadata
#' @description This function retrieves metadata for a given project
#' and saves it as an RDS file for caching.
#' @param project A character string representing the project name.
#' @param as_tibble A logical indicating whether to convert into table. Default is TRUE.
#' @return None. The function saves the metadata to a file.
#' @export
cache_meta <- function(project, as_tibble = TRUE) {
    # Ensure that `project` is a single character string.
    stopifnot(length(project) == 1)
    stopifnot(is.character(project))

    # Retrieve metadata using a function `get_tw_meta()`.
    meta <- get_meta(project, as_tibble = as_tibble, standard_name = TRUE)

    # Get the metadata file path.
    meta_file <- .get_meta_file()

    # Save the metadata to an RDS file.
    saveRDS(meta, meta_file)
}

#' @title Load Metadata
#' @description This function loads metadata for a given project from cache.
#' If the metadata file is missing or `update = TRUE`, it refreshes the cache.
#' @param project A character string representing the project name.
#' @param update A logical flag; if TRUE, refreshes the metadata cache.
#' @param as_tibble A logical indicating whether to convert into table. Default is TRUE.
#' @return A list or object containing the project metadata.
#' @export
load_meta <- function(project, as_tibble = TRUE, update = FALSE) {
    # Ensure that `project` is a single character string.
    stopifnot(length(project) == 1)
    stopifnot(is.character(project))

    # Get the metadata file path.
    meta_file <- .get_meta_file()

    # If the metadata file does not exist or `update = TRUE`, refresh the cache.
    if (!file.exists(meta_file) || update) {
        cache_meta(project, as_tibble = as_tibble)
    }

    # Load metadata from the cached RDS file.
    meta <- readRDS(meta_file)

    return(meta)
}
