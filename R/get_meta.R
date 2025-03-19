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
    filter_tiddlers <- rtiddlywiki::get_tiddlers(filter = filter)

    if (length(filter_tiddlers) == 0) {
        stop("Cannot find any filters for project: ", project)
    }



    # for all apsim tiddlers
    if (standard_name && apsim_name) {
        filter_apsim_j <- sprintf("[tag[APSIM NG]has[cultivar]!is[system]]")
        apsim_tiddlers <- rtiddlywiki::get_tiddlers(filter_apsim_j)
    }

    res <- list()

    i <- 1
    for (i in seq(along = filter_tiddlers)) {
        filter_tiddler <- filter_tiddlers[[i]]

        # Extract relevant filter fields
        names_filter <- names(filter_tiddler)
        if (is.null(filter_tiddler$filter)) {
            warning("Field filter is not defined for ", filter_tiddler$title)
            next
        }



        filter_i <- filter_tiddler$filter
        tiddlers_i <- rtiddlywiki::get_tiddlers(filter_i)

        values_i <- tiddlers_i
        # update extra values: category and subcategory
        j <- 1
        for (j in seq(along = values_i)) {
            if (!is.null(filter_tiddler$group)) {
                values_i[[j]]$group <- filter_tiddler$group
            }
            if (!is.null(filter_tiddler$crop)) {
                values_i[[j]]$crop <- filter_tiddler$crop
            }
        }
        if (!standard_name) {
            res[[filter_tiddler$title]] <- values_i
            next
        }
        # Get standard name
        #groups <- purrr::map_chr(values_i, function(x) x$group) |> unique()
        # We should assume each filter has the same group
        group <- unique(filter_tiddler$group)

        # Get all groups from meta data
        # j <- 1
        # for (j in seq(along = groups)) {
            # Obtain all tiddlers for this group
        filter_j <- sprintf("[tag[%s]]", group)
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

                # for variety group
                if (group == "Variety") {
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
                        values_i[[k]]$in_apsim <- TRUE
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
            if (group == "Variety") {
                if ((is.null(values_i[[k]]$apsim_name))) {
                    values_i[[k]]$apsim_name <- values_i[[k]]$id
                    values_i[[k]]$in_apsim <- FALSE
                }
            }
        }
        # Get coordinate of Place
        if (group == "Place") {
            place_all <- purrr::map_chr(values_i, function(x) x$place) |>
                paste(collapse = " ") |>
                rtiddlywiki::split_field()
            filter_place <- paste0("[[", place_all, "]]") |>
                paste(collapse = " ")
            tiddler_place <- rtiddlywiki::get_tiddlers(filter_place)
            tiddler_place <- tiddler_place |>
                purrr::map_df(function(x) {
                    tibble::tibble(
                        place = x$title,
                        point = x$point
                    )
                }) |>
                tidyr::separate(col = "point", into = c("latitude", "longitude"), sep = ", +") |>
                dplyr::mutate(latitude = as.numeric(.data$latitude),
                              longitude = as.numeric(.data$longitude))
            k <- 1
            for (k in seq(along = values_i)) {
                place_name_k <- values_i[[k]]$place |>
                    rtiddlywiki::split_field()
                if (length(place_name_k) != 1) {
                    stop("Only require a single place for ", values_i[[k]]$title)
                }
                pos <- tiddler_place$place %in% place_name_k
                if (sum(pos) != 1) {
                    next
                }

                values_i[[k]]$latitude <- tiddler_place$latitude[pos]
                values_i[[k]]$longitude <- tiddler_place$longitude[pos]
            }

        }


        # Store processed values
        res[[filter_tiddler$title]] <- values_i
    }
    if (as_tibble) {
        system_fields <- c("revision", "bag", "creator", "modifier",
                                 "modified", "created", "type", "text", "title")
        for (i in seq(along = res)) {
            # Convert retrieved tiddlers into a tibble format
            res_i <- tibble::tibble(repo = res[[i]]) |>
                tidyr::unnest_wider("repo")
            res_i <- res_i[,!(names(res_i) %in% system_fields)]
            res[[i]] <- res_i
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
