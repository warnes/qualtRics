skip_on_cran()

# Expected column sets per type combination ------------------------------------

COLS_ALL <- c(
  "survey_id", "survey_name",
  "dist_invited", "dist_in_progress", "dist_completed",
  "meta_completed", "meta_response_rate",
  "export_completed", "export_in_progress", "export_response_rate"
)
COLS_DIST <- c(
  "survey_id", "survey_name",
  "dist_invited", "dist_in_progress", "dist_completed"
)
COLS_META <- c(
  "survey_id", "survey_name",
  "meta_completed", "meta_response_rate"
)
COLS_RESP <- c(
  "survey_id", "survey_name",
  "export_completed", "export_in_progress", "export_response_rate"
)
COLS_DIST_META <- c(
  "survey_id", "survey_name",
  "dist_invited", "dist_in_progress", "dist_completed",
  "meta_completed", "meta_response_rate"
)
COLS_DIST_RESP <- c(
  "survey_id", "survey_name",
  "dist_invited", "dist_in_progress", "dist_completed",
  "export_completed", "export_in_progress", "export_response_rate"
)

# Fake API results used by mocks -----------------------------------------------

fake_distributions <- tibble::tibble(
  stats_sent     = 100L,
  stats_started  = 60L,
  stats_finished = 50L
)

fake_metadata <- list(
  metadata       = list(name = "Fake Survey Name"),
  responsecounts = list(auditable = 55L)
)

fake_survey_complete   <- tibble::tibble(ResponseId = paste0("R_", 1:50))
fake_survey_inprogress <- tibble::tibble(ResponseId = paste0("R_ip_", 1:10))

# local_mock_deps(): mocks the three direct dependencies of
# fetch_response_counts so tests are fully offline (no network, no cache).
local_mock_deps <- function(.env = parent.frame()) {
  local_mocked_bindings(
    check_credentials   = function()            invisible(NULL),
    fetch_distributions = function(sid, ...)    fake_distributions,
    metadata            = function(sid, ...)    fake_metadata,
    fetch_survey        = function(sid, responses = "complete", ...) {
      if (responses == "complete")    fake_survey_complete
      else                            fake_survey_inprogress
    },
    .env = .env
  )
}

# Input validation ------------------------------------------------------------

test_that("fetch_response_counts() errors when surveyIDs is not character", {
  local_mocked_bindings(check_credentials = function() invisible(NULL))
  expect_error(fetch_response_counts(123L), "character")
})

test_that("fetch_response_counts() errors when survey_names wrong length", {
  local_mocked_bindings(check_credentials = function() invisible(NULL))
  expect_error(
    fetch_response_counts(c("SV_abc", "SV_def"), survey_names = "Only One Name"),
    "same length"
  )
})

test_that("fetch_response_counts() errors on invalid type value", {
  local_mocked_bindings(check_credentials = function() invisible(NULL))
  expect_error(fetch_response_counts("SV_abc", type = "invalid"), "should be one of")
})

# Default (type = "all") -------------------------------------------------------

test_that("fetch_response_counts() returns correct structure for a single survey", {
  local_mock_deps()

  x <- fetch_response_counts("SV_abcdef123456789")

  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x), 1L)
  expect_named(x, COLS_ALL)
  expect_type(x$survey_id,            "character")
  expect_type(x$survey_name,          "character")
  expect_type(x$dist_invited,         "integer")
  expect_type(x$dist_in_progress,     "integer")
  expect_type(x$dist_completed,       "integer")
  expect_type(x$meta_completed,       "integer")
  expect_type(x$meta_response_rate,   "double")
  expect_type(x$export_completed,     "integer")
  expect_type(x$export_in_progress,   "integer")
  expect_type(x$export_response_rate, "double")
  # values from fake_distributions: sent=100, started=60, finished=50
  expect_equal(x$dist_invited,     100L)
  expect_equal(x$dist_in_progress,  10L)   # 60 - 50
  expect_equal(x$dist_completed,    50L)
  # values from fake_metadata: auditable=55
  expect_equal(x$meta_completed,    55L)
  expect_equal(x$meta_response_rate, 55 / 100)
  # values from fake_survey_complete/inprogress: 50 / 10
  expect_equal(x$export_completed,   50L)
  expect_equal(x$export_in_progress, 10L)
  expect_equal(x$export_response_rate, 50 / 100)
})

test_that("fetch_response_counts() returns one row per survey for multiple surveys", {
  local_mock_deps()

  ids <- c("SV_aaa111", "SV_bbb222")
  x <- fetch_response_counts(ids)

  expect_equal(nrow(x), 2L)
  expect_equal(x$survey_id, ids)
})

# survey_names handling --------------------------------------------------------

test_that("fetch_response_counts() uses names() of surveyIDs as survey_names by default", {
  local_mock_deps()

  # names on the surveyIDs vector become survey labels (no extra API call)
  ids <- c("My Survey" = "SV_abcdef123456789")
  x <- fetch_response_counts(ids)

  expect_equal(x$survey_name, "My Survey")
})

test_that("fetch_response_counts() accepts explicit parallel survey_names vector", {
  local_mock_deps()

  # survey_names is a plain parallel vector — same length and order as surveyIDs
  x <- fetch_response_counts(
    c("SV_aaa111", "SV_bbb222"),
    survey_names = c("Household Survey 2025", "Business Survey 2025")
  )

  expect_equal(x$survey_name, c("Household Survey 2025", "Business Survey 2025"))
})

test_that("fetch_response_counts() fetches survey name from API when survey_names = NULL", {
  local_mock_deps()

  x <- fetch_response_counts("SV_abcdef123456789", survey_names = NULL)

  expect_equal(x$survey_name, "Fake Survey Name")
})

# type parameter ---------------------------------------------------------------

test_that("fetch_response_counts() type='distributions' returns only dist_* columns", {
  local_mock_deps()

  x <- fetch_response_counts("SV_abcdef123456789", type = "distributions")

  expect_named(x, COLS_DIST)
  expect_equal(x$dist_invited, 100L)
})

test_that("fetch_response_counts() type='metadata' returns only meta_* columns", {
  local_mock_deps()

  x <- fetch_response_counts("SV_abcdef123456789", type = "metadata")

  expect_named(x, COLS_META)
  expect_equal(x$meta_completed, 55L)
  # dist data is fetched internally to support rate calculation even when
  # type = "metadata" only, so meta_response_rate is computable
  expect_equal(x$meta_response_rate, 55 / 100)
})

test_that("fetch_response_counts() type='responses' returns only export_* columns", {
  local_mock_deps()

  x <- fetch_response_counts("SV_abcdef123456789", type = "responses")

  expect_named(x, COLS_RESP)
  expect_equal(x$export_completed, 50L)
  # dist data is fetched internally to support rate calculation even when
  # type = "responses" only, so export_response_rate is computable
  expect_equal(x$export_response_rate, 50 / 100)
})

test_that("fetch_response_counts() type=c('distributions','metadata') returns combined columns", {
  local_mock_deps()

  x <- fetch_response_counts("SV_abcdef123456789",
                              type = c("distributions", "metadata"))

  expect_named(x, COLS_DIST_META)
  # response_rate computable because dist_invited is present
  expect_equal(x$meta_response_rate, 55 / 100)
})

test_that("fetch_response_counts() type=c('distributions','responses') returns combined columns", {
  local_mock_deps()

  x <- fetch_response_counts("SV_abcdef123456789",
                              type = c("distributions", "responses"))

  expect_named(x, COLS_DIST_RESP)
  expect_equal(x$export_response_rate, 50 / 100)
})

test_that("fetch_response_counts() type='all' is equivalent to the default", {
  local_mock_deps()

  x_default <- fetch_response_counts("SV_abcdef123456789")
  x_all     <- fetch_response_counts("SV_abcdef123456789", type = "all")

  expect_equal(x_default, x_all)
})
