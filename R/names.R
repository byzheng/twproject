
#' Get standard names
#'
#' @param names A character vector for names
#' @param group Group of standard names
#' @param is_apsim_name whether to return names only if group is "Variety".
#'
#' @returns A vector for standard name if is_apsim_name is FALSE, but a data.frame of
#' standard_name and apsim_name if is_apsim_name is TRUE.
#' @export
standard_name <- function(names, group, is_apsim_name = FALSE) {

    stopifnot(is.character(group))
    stopifnot(length(group) == 1)
    stopifnot(is.vector(names))
    stopifnot(length(names) > 0)
    stopifnot(is.logical(is_apsim_name))
    stopifnot(length(is_apsim_name) == 1)

    filter <- sprintf("[tag[%s]]", group)
    tiddlers <- rtiddlywiki::get_tiddlers(filter)


    if (group == "Variety" && is_apsim_name) {
        filter_apsim <- sprintf("[tag[APSIM NG]has[cultivar]!is[system]]")
        apsim_tiddlers <- rtiddlywiki::get_tiddlers(filter_apsim)
    }
    # Split aka field and loop through group tiddlers
    standard_name <- rep(NA, length(names))
    apsim_names <- rep(NA, length(names))

    k <- 1
    for (k in seq(along = names)) {

        m <- 1
        for (m in seq(along = tiddlers)) {
            if (is.null(tiddlers[[m]]$name_aka99999999999999999)) {
                tiddlers[[m]]$name_aka99999999999999999 <- c(tiddlers[[m]]$title,
                                                             rtiddlywiki::split_field(tiddlers[[m]]$aka)) |>
                    unique() |>
                    tolower()
            }

            # go to next if not find
            if (!(tolower(names[k]) %in% tiddlers[[m]]$name_aka99999999999999999)) {
                next
            }

            # Assign values if find
            standard_name[k] <- tiddlers[[m]]$title
            if (group == "Variety" && is_apsim_name) {
                # Get apsim name
                n <- 1
                for (n in seq(along = apsim_tiddlers)) {
                    if (tolower(apsim_tiddlers[[n]]$cultivar) %in%  tiddlers[[m]]$name_aka99999999999999999) {
                        apsim_names[k] <- apsim_tiddlers[[n]]$cultivar
                        break
                    }
                }
            }
            break
        }

    }
    if (group == "Variety" && is_apsim_name) {
        return(data.frame(standard_name = standard_name,
                    apsim_name = apsim_names))
    }
    return (standard_name)
}
