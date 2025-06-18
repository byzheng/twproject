test_that("tiddler", {
    skip_if(!is_test_tw())

    # Test errors
    expect_error(get_meta(c("project1", "project2")))
    expect_error(get_meta("project"))


    # Test warning
    #Inject test data
    rtiddlywiki::put_tiddler("project2/filter/variety/wheat", text="", tags = c("Project Filter", "project2", "test"),
                             fields = list(group = "Variety",
                                           crop = "wheat"))

    expect_warning(get_meta("project2"))


    # Test meta data
    rtiddlywiki::put_tiddler("project1", text="", tags = c("Project", "test"))
    rtiddlywiki::put_tiddler("project1/filter/variety", text="", tags = c("Project Filter", "project1", "test"),
                             fields = list(`filter` = "[tag[Project Meta]field:group[Variety]field:project[project1]]",
                                           group = "Variety",
                                           crop = "wheat"))


    for (i in c(0, seq_len(10))) {
        id <- sprintf("variety%s", i)
        rtiddlywiki::put_tiddler(sprintf("project1/%s", id), text="", tags = c("Project Meta", "test"),
                                 fields = list(group = "Variety",
                                               project = "project1",
                                               id = id))
    }
    id <- sprintf("variety%s", 11)
    rtiddlywiki::put_tiddler(sprintf("project1/%s", id), text="", tags = c("Project Meta", "test"),
                             fields = list(group = "Variety",
                                           project = "project1",
                                           id = id))
    for (i in seq_len(10)) {
        id <- sprintf("Variety %s", i)
        id_aka <- c(sprintf("variety%s", i), sprintf("apsim_variety%s", i))
        rtiddlywiki::put_tiddler(id, text="", tags = c("Variety", "test"),
                                 fields = list(aka = id_aka, crop = "Wheat"))
    }

    # Add apsim
    rtiddlywiki::put_tiddler("data/apsimng/parameter/Wheat/variety1", text="", tags = c("APSIM NG", "Wheat", "test"),
                             fields = list(cultivar = "apsim_Variety1", crop = "Wheat"))

    rtiddlywiki::put_tiddler("data/apsimng/parameter/Wheat/variety2", text="", tags = c("APSIM NG", "Wheat", "test"),
                             fields = list(cultivar = "apsim_Variety2", crop = "Wheat"))


    # Generate Place
    latitude <- -34
    longitude <- 147
    for (i in seq_len(10)) {
        id <- sprintf("Place %s", i)
        id_aka <- c(sprintf("place%s", i))
        id_weather <- c(sprintf("ppd/%s", i))
        rtiddlywiki::put_tiddler(id, text="", tags = c("Place", "test"),
                                 fields = list(aka = id_aka,
                                               `weather-station` = id_weather,
                                               point = paste(latitude + i, longitude + i, sep = ", ")))
    }
    for (i in c(0, seq_len(10))) {
        id <- sprintf("place%s", i)
        pid <- sprintf("[[Place %s]]", i)
        rtiddlywiki::put_tiddler(sprintf("project1/%s", id),
                                 text="",
                                 tags = c("Project Meta", "test"),
                                 fields = list(group = "Place",
                                               project = "project1",
                                               id = id,
                                               place = pid))
    }

    rtiddlywiki::put_tiddler("project1/filter/place", text="", tags = c("Project Filter", "project1", "test"),
                             fields = list(`filter` = "[tag[Project Meta]field:group[Place]field:project[project1]]",
                                           group = "Place"))

    # standard_name

    sname <- standard_name(paste0("variety", seq(1, 11)), "Variety")
    expect_equal(c(paste0("Variety ", seq(1, 10)), NA), sname)

    # APSIM name
    apsim_names <- standard_name(paste0("variety", seq(1, 11)), "Variety", is_apsim_name = TRUE)
    expect_equal(class(apsim_names), "data.frame")
    expect_equal(c(paste0("apsim_Variety", seq(1, 2)), rep(NA, 9)), apsim_names$apsim_name)

    # Test meta

    meta <- get_meta("project1", as_tibble = FALSE)

    expect_equal(length(meta), 2)
    meta_variety <- meta[["project1/filter/variety"]]
    expect_equal(length(meta_variety), 12)


    meta <- get_meta("project1")
    expect_equal(length(meta), 2)

    # Variety
    meta_variety <- meta[["project1/filter/variety"]]
    expect_equal(nrow(meta_variety), 12)
    expect_equal(meta_variety$id[2], "variety1")
    expect_equal(meta_variety$group[2], "Variety")
    expect_equal(meta_variety$standard_name[2], "Variety 1")
    expect_equal(meta_variety$preferred_name[2], "Variety 1")
    expect_equal(meta_variety$preferred_name[4], "variety11")
    expect_equal(meta_variety$preferred_name[5], "Variety 2")
    expect_equal(meta_variety$preferred_name[6], "Variety 3")

    expect_equal(meta_variety$apsim_name[2], "apsim_Variety1")
    expect_equal(meta_variety$apsim_name[5], "apsim_Variety2")
    expect_equal(meta_variety$apsim_name[3], "variety10")
    expect_equal(meta_variety$in_apsim[2], TRUE)
    expect_equal(meta_variety$in_apsim[5], TRUE)
    expect_equal(meta_variety$in_apsim[3], FALSE)


    # Place
    meta_place <- meta$`project1/filter/place`
    expect_equal(tibble::has_name(meta_place, "apsim_name"), FALSE)

    expect_equal(meta_place$id[2], "place1")
    expect_equal(meta_place$group[2], "Place")
    expect_equal(meta_place$standard_name[2], "Place 1")
    expect_equal(meta_place$preferred_name[2], "Place 1")
    expect_equal(meta_place$preferred_name[4], "Place 2")
    expect_equal(meta_place$latitude[1], NA_integer_)
    expect_equal(meta_place$longitude[1], NA_integer_)
    expect_equal(meta_place$latitude[2], -33)
    expect_equal(meta_place$longitude[2], 148)
    expect_equal(meta_place$weather_station[1], NA_character_)
    expect_equal(meta_place$weather_station[2], "ppd/1")


    # Clean tiddlers
    tiddlers <- rtiddlywiki::get_tiddlers("[tag[test]]")
    for (i in seq(along = tiddlers)) {
        rtiddlywiki::delete_tiddler(tiddlers[[i]]$title)
    }
})


