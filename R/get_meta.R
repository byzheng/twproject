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
#' @param apsim_name A logical indicating whether to retrieve names for APSIM. The id is used if missing
#'
#' @return A list containing filtered metadata for the project.
#' @export
get_meta <- function(project, as_tibble = TRUE, standard_name = TRUE,
                     apsim_name = TRUE) {
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


    # for all apsim tiddlers
    if (standard_name && apsim_name) {
        filter_apsim_j <- sprintf("[tag[APSIM NG]has[crop]!is[system]]")
        apsim_tiddlers <- rtiddlywiki::get_tiddlers(filter_apsim_j)
    }

    # Initialize result list
    res <- list()
    i <- 1
    # Loop through each filter field
    for (i in seq(along = names_filter)) {
        filter_i <- filter_tiddler[[names_filter[i]]]
        tiddlers_i <- rtiddlywiki::get_tiddlers(filter_i)

        values_i <- tiddlers_i
        if (!standard_name) {
            res[[names_filter[i]]] <- values_i
            next
        }
        # Get standard name
        groups <- purrr::map_chr(values_i, function(x) x$group) |> unique()

        # Get all groups from meta data
        j <- 1
        for (j in seq(along = groups)) {
            # Obtain all tiddlers for this group
            filter_j <- sprintf("[tag[%s]]", groups[j])
            tiddler_j <- rtiddlywiki::get_tiddlers(filter_j)
            names_j <- list()
            # Split aka field and loop through group tiddlers
            m <- 1
            for (m in seq(along = tiddler_j)) {
                names_m <- c(tiddler_j[[m]]$title,
                    rtiddlywiki::split_field(tiddler_j[[m]]$aka)) |>
                    unique() |>
                    tolower()
                # match id with aka field
                k <- 1
                for (k in seq(along = values_i)) {
                    # go to next if not find
                    if (!(tolower(values_i[[k]]$id) %in% names_m)) {
                        next
                    }

                    # Assign values if find
                    values_i[[k]]$standard_name <- tiddler_j[[m]]$title
                    values_i[[k]]$preferred_name <- values_i[[k]]$standard_name
                    # Break if no apsim name
                    if (!apsim_name) {
                        break
                    }
                    # Get apsim name

                    n <- 1
                    for (n in seq(along = apsim_tiddlers)) {
                        if (!(tolower(apsim_tiddlers[[n]]$cultivar) %in% names_m)) {
                            next
                        }
                        values_i[[k]]$apsim_name <- apsim_tiddlers[[n]]$cultivar
                        break
                    }
                    break

                }

            }

        }

        # update preferred_name and apsim_name

        for (k in seq(along = values_i)) {
            if ((is.null(values_i[[k]]$preferred_name))) {
                values_i[[k]]$preferred_name <- values_i[[k]]$id
            }

            if ((is.null(values_i[[k]]$apsim_name))) {
                values_i[[k]]$apsim_name <- values_i[[k]]$id
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


.get_apsim_name <- function(crop, names) {
    i <- 1
    for (i in seq(along = names)) {
        filter <- sprintf("[tag[APSIM NG]tag[%s]!is[system]] :filter[get[cultivar]match:caseinsensitive[%s]]", crop, tolower(names[i]))
        tiddlers <- rtiddlywiki::get_tiddlers(filter)
        if (length(tiddlers) > 0) {
            return (tiddlers[[1]]$cultivar)
        }
    }
    return(NULL)
}
