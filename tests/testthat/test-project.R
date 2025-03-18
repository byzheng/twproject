test_that("tiddler", {
    skip_if(!is_test_tw())

    # Test errors
    expect_error(get_meta(c("project1", "project2")))
    expect_error(get_meta("project"))


    # Test warning
    rtiddlywiki::put_tiddler("project2/filter/variety/wheat", text="", tags = c("Project Filter", "project2", "test"),
                             fields = list(category = "variety",
                                           subcategory = "wheat"))

    expect_warning(get_meta("project2"))


    # Test meta data
    rtiddlywiki::put_tiddler("project1", text="", tags = c("Project", "test"))
    rtiddlywiki::put_tiddler("project1/filter", text="", tags = c("Project Filter", "project1", "test"),
                             fields = list(`filter` = "[tag[Project Meta]field:group[Variety]field:project[project1]]",
                                           category = "variety",
                                           subcategory = "wheat"))


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




    meta <- get_meta("project1")
    expect_equal(length(meta), 1)
    expect_equal(nrow(meta[[1]]), 12)
    expect_equal(meta[[1]]$id[2], "variety1")
    expect_equal(meta[[1]]$group[2], "Variety")
    expect_equal(meta[[1]]$standard_name[2], "Variety 1")
    expect_equal(meta[[1]]$preferred_name[2], "Variety 1")
    expect_equal(meta[[1]]$preferred_name[4], "variety11")
    expect_equal(meta[[1]]$preferred_name[5], "Variety 2")
    expect_equal(meta[[1]]$preferred_name[6], "Variety 3")

    expect_equal(meta[[1]]$apsim_name[2], "apsim_Variety1")
    expect_equal(meta[[1]]$apsim_name[5], "apsim_Variety2")
    expect_equal(meta[[1]]$apsim_name[3], "variety10")


    meta <- get_meta("project1", as_tibble = FALSE)
    expect_equal(length(meta), 1)
    expect_equal(length(meta[[1]]), 12)

    # Clean tiddlers
    tiddlers <- rtiddlywiki::get_tiddlers("[tag[test]]")
    for (i in seq(along = tiddlers)) {
        rtiddlywiki::delete_tiddler(tiddlers[[i]]$title)
    }
})


