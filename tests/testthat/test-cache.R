skip_on_cran()

# ---------------------------------------------------------------------------
# qualtrics_cache_ttl() and qualtrics_cache_dir() read from options
# ---------------------------------------------------------------------------

test_that("qualtrics_cache_ttl() returns 86400 by default", {
  withr::with_options(list(qualtRics.cache_ttl = NULL), {
    expect_equal(qualtrics_cache_ttl(), 86400L)
  })
})

test_that("qualtrics_cache_ttl() reads qualtRics.cache_ttl option", {
  withr::with_options(list(qualtRics.cache_ttl = 3600), {
    expect_equal(qualtrics_cache_ttl(), 3600)
  })
})

test_that("qualtrics_cache_dir() returns R_user_dir path by default", {
  withr::with_options(list(qualtRics.cache_dir = NULL), {
    expect_equal(
      qualtrics_cache_dir(),
      tools::R_user_dir("qualtRics", which = "cache")
    )
  })
})

test_that("qualtrics_cache_dir() reads qualtRics.cache_dir option", {
  withr::with_options(list(qualtRics.cache_dir = "/tmp/my-cache"), {
    expect_equal(qualtrics_cache_dir(), "/tmp/my-cache")
  })
})


# ---------------------------------------------------------------------------
# qualtrics_configure_cache()
# ---------------------------------------------------------------------------

test_that("qualtrics_configure_cache() sets both options", {
  dir <- withr::local_tempdir()
  withr::with_options(list(qualtRics.cache_ttl = NULL, qualtRics.cache_dir = NULL), {
    qualtrics_configure_cache(ttl = 1800, dir = dir)
    expect_equal(getOption("qualtRics.cache_ttl"), 1800)
    expect_equal(getOption("qualtRics.cache_dir"), dir)
  })
  .qualtrics_cache_env$api_request       <- NULL
  .qualtrics_cache_env$file_download     <- NULL
  .qualtrics_cache_env$fetch_and_process <- NULL
})

test_that("qualtrics_configure_cache() returns ttl and dir invisibly", {
  dir <- withr::local_tempdir()
  withr::with_options(list(qualtRics.cache_ttl = NULL, qualtRics.cache_dir = NULL), {
    out <- qualtrics_configure_cache(ttl = 7200, dir = dir)
    expect_equal(out$ttl, 7200)
    expect_equal(out$dir, dir)
  })
  .qualtrics_cache_env$api_request       <- NULL
  .qualtrics_cache_env$file_download     <- NULL
  .qualtrics_cache_env$fetch_and_process <- NULL
})

test_that("qualtrics_configure_cache() rejects negative ttl", {
  expect_error(
    qualtrics_configure_cache(ttl = -1),
    "`ttl` must be >= 0"
  )
})

test_that("qualtrics_configure_cache() rejects non-numeric ttl", {
  expect_error(
    qualtrics_configure_cache(ttl = "daily"),
    "must be a single numeric value"
  )
})

test_that("qualtrics_configure_cache() rejects NA ttl", {
  expect_error(
    qualtrics_configure_cache(ttl = NA_real_),
    "must be a single numeric value"
  )
})

test_that("qualtrics_configure_cache() rejects non-string dir", {
  expect_error(
    qualtrics_configure_cache(dir = 42),
    "single string"
  )
})


# ---------------------------------------------------------------------------
# .build_cache() — TTL = 0 disables caching (points to originals)
# ---------------------------------------------------------------------------

test_that(".build_cache() with ttl=0 assigns unwrapped originals", {
  withr::with_options(list(qualtRics.cache_ttl = 0), {
    .qualtrics_cache_env$api_request    <- NULL
    .qualtrics_cache_env$file_download  <- NULL
    .qualtrics_cache_env$fetch_and_process <- NULL
    .build_cache()

    # When ttl=0, the assigned functions should be identical to their originals,
    # NOT memoised wrappers
    expect_false(memoise::is.memoised(.qualtrics_cache_env$api_request))
    expect_false(memoise::is.memoised(.qualtrics_cache_env$file_download))
    expect_false(memoise::is.memoised(.qualtrics_cache_env$fetch_and_process))
  })
  # Null out so we don't leak a reference to a non-existent function
  .qualtrics_cache_env$api_request       <- NULL
  .qualtrics_cache_env$file_download     <- NULL
  .qualtrics_cache_env$fetch_and_process <- NULL
})

test_that(".build_cache() with ttl>0 creates memoised wrappers", {
  dir <- withr::local_tempdir()
  withr::with_options(list(qualtRics.cache_ttl = 3600, qualtRics.cache_dir = dir), {
    .qualtrics_cache_env$api_request    <- NULL
    .qualtrics_cache_env$file_download  <- NULL
    .qualtrics_cache_env$fetch_and_process <- NULL
    .build_cache()

    expect_true(memoise::is.memoised(.qualtrics_cache_env$api_request))
    expect_true(memoise::is.memoised(.qualtrics_cache_env$file_download))
    expect_true(memoise::is.memoised(.qualtrics_cache_env$fetch_and_process))
  })
  # Null out so destroyed tempdir cache doesn't leak into subsequent tests
  .qualtrics_cache_env$api_request       <- NULL
  .qualtrics_cache_env$file_download     <- NULL
  .qualtrics_cache_env$fetch_and_process <- NULL
})


# ---------------------------------------------------------------------------
# cached_api_request() — POST always bypasses cache
# ---------------------------------------------------------------------------

test_that("cached_api_request() calls original directly for POST", {
  dir <- withr::local_tempdir()
  withr::with_options(list(qualtRics.cache_ttl = 3600, qualtRics.cache_dir = dir), {
    .qualtrics_cache_env$api_request    <- NULL
    .build_cache()

    called <- 0L
    local_mocked_bindings(
      qualtrics_api_request = function(...) { called <<- called + 1L; list() }
    )
    # Two POST calls — neither should be served from cache
    cached_api_request("POST", url = "https://example.com")
    cached_api_request("POST", url = "https://example.com")
    expect_equal(called, 2L)
  })
  # Null out so destroyed tempdir cache doesn't leak into subsequent tests
  .qualtrics_cache_env$api_request       <- NULL
  .qualtrics_cache_env$file_download     <- NULL
  .qualtrics_cache_env$fetch_and_process <- NULL
})


# ---------------------------------------------------------------------------
# qualtrics_clear_cache()
# ---------------------------------------------------------------------------

test_that("qualtrics_clear_cache() emits informational message for missing dir", {
  withr::with_options(list(qualtRics.cache_dir = withr::local_tempdir()), {
    missing_dir <- file.path(qualtrics_cache_dir(), "nonexistent")
    expect_message(
      qualtrics_clear_cache(dir = missing_dir),
      "does not exist"
    )
  })
})

test_that("qualtrics_clear_cache() clears an existing cache directory", {
  dir <- withr::local_tempdir()
  # Populate the cache with something
  c <- cachem::cache_disk(dir = dir)
  c$set("key1", "value1")
  expect_equal(length(c$keys()), 1L)

  qualtrics_clear_cache(dir = dir)

  # Re-open to verify it's empty
  c2 <- cachem::cache_disk(dir = dir)
  expect_equal(length(c2$keys()), 0L)
})

test_that("qualtrics_clear_cache() emits success message for existing dir", {
  dir <- withr::local_tempdir()
  cachem::cache_disk(dir = dir)$set("k", "v")

  expect_message(
    qualtrics_clear_cache(dir = dir),
    "cache cleared"
  )
})

test_that("qualtrics_clear_cache() returns the directory path invisibly", {
  dir <- withr::local_tempdir()
  out <- qualtrics_clear_cache(dir = dir)
  expect_equal(out, dir)
})

test_that("qualtrics_clear_cache() rejects non-string dir", {
  expect_error(qualtrics_clear_cache(dir = 123), "single string")
})
