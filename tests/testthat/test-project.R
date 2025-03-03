test_that("tiddler", {
    skip_if(!is_test_tw())
    # Inject data to retrieve
    rtiddlywiki::put_tiddler("project1", text="", tags = c("Project", "test"))
    rtiddlywiki::put_tiddler("project1/filter", text="", tags = c("Project Filter", "project1", "test"),
                             fields = list(`filter-variety` = "[tag[Project Link]field:group[Variety]field:project[project1]]"))
    for (i in seq_len(10)) {
        id <- sprintf("variety%s", i)
        rtiddlywiki::put_tiddler(sprintf("project1/%s", id), text="", tags = c("Project Link", "test"),
                                 fields = list(group = "Variety",
                                               project = "project1",
                                               id = id))
    }
    for (i in seq_len(10)) {
        id <- sprintf("Variety %s", i)
        id_aka <- sprintf("variety%s", i)
        rtiddlywiki::put_tiddler(id, text="", tags = c("Variety", "test"),
                                 fields = list(aka = id_aka))
    }

    expect_error(get_meta(c("project1", "project2")))
    expect_error(get_meta("project"))

    meta <- get_meta("project1")
    expect_equal(length(meta), 1)
    expect_equal(nrow(meta[[1]]), 10)
    expect_equal(meta[[1]]$id[1], "variety1")
    expect_equal(meta[[1]]$group[1], "Variety")
    expect_equal(meta[[1]]$standard_name[1], "Variety 1")
    expect_equal(meta[[1]]$project_name[1], "Variety 1")

    meta <- get_meta("project1", as_tibble = FALSE)
    expect_equal(length(meta), 1)
    expect_equal(length(meta[[1]]), 10)

    # Clean tiddlers
    tiddlers <- rtiddlywiki::get_tiddlers("[tag[test]]")
    for (i in seq(along = tiddlers)) {
        rtiddlywiki::delete_tiddler(tiddlers[[i]]$title)
    }
})


