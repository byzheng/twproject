# function related with tiddlywiki.com

#' Save Metadata for a project from TiddlyWiki
#'
#' @details
#'
#' This function retrieves metadata from a TiddlyWiki instance based on a specified project
#' and applies a filtering process to extract relevant tiddlers. It can optionally map tiddlers
#' to a standard name based on an alias (`aka` field).
#'
#' @param project A character string specifying the project name. Must be a single value.
#' @param as_tibble A logical indicating whether to convert into table. Default is TRUE.
#' @param standard_name A logical indicating whether to assign a standard name based on `aka` values. Default is TRUE.
#'
#' @return A list containing filtered metadata for the project.
#' @export
get_meta <- function(project, as_tibble = TRUE, standard_name = TRUE) {
    # Ensure input validity
    stopifnot(length(project) == 1)
    stopifnot(is.character(project))
    stopifnot(length(as_tibble) == 1)
    stopifnot(is.logical(as_tibble))
    stopifnot(length(standard_name) == 1)
    stopifnot(is.logical(standard_name))

    # Define filter string to retrieve project-specific tiddlers
    filter <- sprintf("[tag[Project Filter]tag[%s]]", project)
    filter_tiddler <- rtiddlywiki::get_tiddlers(filter = filter)
    stopifnot(length(filter_tiddler) == 1) # Ensure exactly one tiddler is retrieved
    filter_tiddler <- filter_tiddler[[1]]

    # Extract relevant filter fields
    names_filter <- names(filter_tiddler)
    names_filter <- names_filter[grepl("^filter-", names_filter)]
    if (length(names_filter) == 0) {
        stop("No filter is defined for project ", project)
    }

    # Initialize result list
    res <- list()

    # Loop through each filter field
    for (i in seq(along = names_filter)) {
        filter_i <- filter_tiddler[[names_filter[i]]]
        tiddlers_i <- rtiddlywiki::get_tiddlers(filter_i)

        # Convert retrieved tiddlers into a tibble format
        # values_i <- tibble::tibble(repo = tiddlers_i) |>
        #     tidyr::unnest_wider("repo")
        values_i <- tiddlers_i
        # Remove the revision column if present
        #values_i$revision <- NULL

        # # Ensure `standard_name` is not an existing field
        # if ("standard_name" %in% names(values_i)) {
        #     stop("standard_name is not allowed in the fields")
        # }

        # Add an empty `standard_name` column
        #values_i$standard_name <- NA

        # If standard_name mapping is not required, store results and continue
        if (!standard_name) {
            res[[names_filter[i]]] <- values_i
            next
        }

        # Assign standard names based on `aka` field
        for (j in seq(along = values_i)) {
            filter_j <- sprintf("[tag[%s]aka[%s]]", values_i[[j]]$group, values_i[[j]]$id)
            tiddler_j <- rtiddlywiki::get_tiddlers(filter_j)

            # Ensure a unique standard name is found
            if (length(tiddler_j) > 1) {
                stop("Multiple standard names are found")
            }

            # Assign standard name if available
            if (length(tiddler_j) == 1) {
                values_i[[j]]$standard_name <- tiddler_j[[1]]$title
                values_i[[j]]$preferred_name <- values_i[[j]]$standard_name
            } else {
                values_i[[j]]$preferred_name <- values_i[[j]]$id
            }
        }

        # Store processed values
        res[[names_filter[i]]] <- values_i
    }
    if (as_tibble) {
        for (i in seq(along = res)) {
            # Convert retrieved tiddlers into a tibble format
            res[[i]] <- tibble::tibble(repo = res[[i]]) |>
                tidyr::unnest_wider("repo")

        }
    }
    return(res)
}
