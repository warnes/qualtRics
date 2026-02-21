skip_on_cran()

# Survey IDs used for live-API tests (SNG Community Assessment surveys)
SID1 <- "SV_bIYzD8GGlzdTNiK"  # consumer survey  (~485 cols, ~400 rows)
SID2 <- "SV_0iHoeLpRyUJyXrg"  # business survey

# Helper: skip tests that require live API credentials
skip_if_no_real_api <- function() {
  skip_if(
    Sys.getenv("QUALTRICS_BASE_URL") == "iad1.qualtrics.com",
    "Skipping live-API test: only test credentials available"
  )
}

test_that("fetch_survey() returns a data frame with expected structure", {
  skip_if_no_real_api()
  vcr::turned_off({
    x <- fetch_survey(SID1, responses = "complete", limit = 5)
  })

  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_gte(ncol(x), 17L)

  # Standard metadata columns present
  expect_true(all(c(
    "StartDate", "EndDate", "Status", "IPAddress", "Progress",
    "Duration (in seconds)", "Finished", "RecordedDate", "ResponseId",
    "DistributionChannel", "UserLanguage"
  ) %in% names(x)))

  # datetime columns stored as POSIXct (typeof "double")
  expect_s3_class(x$StartDate, c("POSIXct", "POSIXt"))
  expect_s3_class(x$EndDate,   c("POSIXct", "POSIXt"))

  # plain character metadata
  expect_type(x$Status,     "character")
  expect_type(x$ResponseId, "character")

  # ordered factor (MC/Likert question, stored as integer)
  expect_s3_class(x$Q1, c("ordered", "factor"))
  expect_s3_class(x$Q7, c("ordered", "factor"))

  # logical (boolean metadata column)
  expect_type(x$Finished, "logical")
})

test_that("fetch_survey() respects limit and include_* params", {
  skip_if_no_real_api()
  vcr::turned_off({
    x <- fetch_survey(
      SID1,
      responses         = "complete",
      limit             = 3,
      include_metadata  = c("StartDate", "EndDate", "ResponseId"),
      include_questions = c("QID205"),  # QID205 = Q1 (first real question)
      breakout_sets     = FALSE
    )
  })

  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_lte(nrow(x), 3L)
  expect_true(all(c("StartDate", "EndDate", "ResponseId") %in% names(x)))
  expect_false("Status" %in% names(x))
  expect_type(x$ResponseId, "character")
})

test_that("fetch_survey() with include_* = NA omits metadata and question cols", {
  skip_if_no_real_api()
  vcr::turned_off({
    x <- fetch_survey(
      SID1,
      responses         = "complete",
      limit             = 3,
      include_questions = NA,
      include_metadata  = NA,
      breakout_sets     = FALSE
    )
  })

  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_false("StartDate" %in% names(x))
  expect_false("Q1"        %in% names(x))
})

test_that("fetch_survey() returns in-progress responses when requested", {
  skip_if_no_real_api()
  vcr::turned_off({
    x <- fetch_survey(SID1, responses = "in_progress")
  })
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
})

test_that("fetch_survey() returns all responses when requested", {
  skip_if_no_real_api()
  vcr::turned_off({
    x <- fetch_survey(SID1, responses = "all")
  })
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
})

test_that("Limit cannot be less than one", {
  expect_error(
    qualtRics::fetch_survey("1234", limit = 0),
    "1 or greater"
  )
})

test_that("Handle convert and label conditions", {
  expect_error(
    fetch_survey("1234", label = FALSE),
    "Error in arguments `convert` & `label`:"
  )
})

test_that("unanswer_recode is integer-ish", {
  # Call fetch_survey
  expect_error(
    fetch_survey("1234", unanswer_recode = "hello"),
    "must be a single integer"
  )
})

test_that("formerly-deprecated args are now rejected", {
  expect_error(
    fetch_survey("1234", force_request = TRUE),
    "unused argument"
  )
  expect_error(
    fetch_survey("1234", save_dir = "~/Desktop"),
    "unused argument"
  )
})

test_that("error if bad temporary directory", {
  expect_error(
    fetch_survey("1234", tmp_dir = "/unrealistictempdirectory/"),
    "not an existing directory"
  )
})

test_that("responses argument rejects invalid values", {
  expect_error(
    fetch_survey("1234", responses = "partial"),
    "should be one of"
  )
})

test_that("fetch_survey(responses='all') returns completed when in-progress is empty", {
  call_count <- 0L
  local_mocked_bindings(
    cached_fetch_and_process = function(surveyID, body, verbose, tmp_dir,
                                        import_id, time_zone, col_types, quiet,
                                        add_column_map, add_var_labels,
                                        strip_html, convert, label) {
      call_count <<- call_count + 1L
      # First call = completed (2 rows); second call = in-progress (0 rows)
      if (call_count == 1L) tibble::tibble(ResponseId = c("R_1", "R_2"), StartDate = Sys.time())
      else                  tibble::tibble(ResponseId = character(), StartDate = character())
    }
  )
  x <- fetch_survey(SID1, responses = "all")
  expect_equal(nrow(x), 2L)
  expect_equal(x$ResponseId, c("R_1", "R_2"))
})

test_that("fetch_survey(responses='all') returns in-progress when completed is empty", {
  call_count <- 0L
  local_mocked_bindings(
    cached_fetch_and_process = function(surveyID, body, verbose, tmp_dir,
                                        import_id, time_zone, col_types, quiet,
                                        add_column_map, add_var_labels,
                                        strip_html, convert, label) {
      call_count <<- call_count + 1L
      # First call = completed (0 rows); second call = in-progress (1 row)
      if (call_count == 1L) tibble::tibble(ResponseId = character(), StartDate = character())
      else                  tibble::tibble(ResponseId = "R_ip1", StartDate = Sys.time())
    }
  )
  x <- fetch_survey(SID1, responses = "all")
  expect_equal(nrow(x), 1L)
  expect_equal(x$ResponseId, "R_ip1")
})

test_that("fetch_survey(responses='all') returns empty frame when both are empty", {
  local_mocked_bindings(
    cached_fetch_and_process = function(surveyID, body, verbose, tmp_dir,
                                        import_id, time_zone, col_types, quiet,
                                        add_column_map, add_var_labels,
                                        strip_html, convert, label) {
      tibble::tibble(ResponseId = character())
    }
  )
  x <- fetch_survey(SID1, responses = "all")
  expect_equal(nrow(x), 0L)
  expect_s3_class(x, "data.frame")
})

test_that("fetch_survey(responses='all') row-binds when both have rows", {
  call_count <- 0L
  local_mocked_bindings(
    cached_fetch_and_process = function(surveyID, body, verbose, tmp_dir,
                                        import_id, time_zone, col_types, quiet,
                                        add_column_map, add_var_labels,
                                        strip_html, convert, label) {
      call_count <<- call_count + 1L
      # First call = completed; second call = in-progress
      if (call_count == 1L) tibble::tibble(ResponseId = c("R_1", "R_2"))
      else                  tibble::tibble(ResponseId = "R_ip1")
    }
  )
  x <- fetch_survey(SID1, responses = "all")
  expect_equal(nrow(x), 3L)
  expect_equal(x$ResponseId, c("R_1", "R_2", "R_ip1"))
})

test_that("fetch_survey() passes quiet param to process_raw_survey()", {
  # Verify the quiet argument is forwarded — mock the expensive network calls
  # so this test is fully offline.
  quiet_received <- NA

  local_mocked_bindings(
    cached_fetch_and_process = function(surveyID, body, verbose, tmp_dir,
                                        import_id, time_zone, col_types, quiet,
                                        add_column_map, add_var_labels,
                                        strip_html, convert, label) {
      quiet_received <<- quiet
      tibble::tibble(ResponseId = "R_test")
    }
  )

  fetch_survey(SID1, responses = "complete", quiet = FALSE)
  expect_false(quiet_received)

  fetch_survey(SID1, responses = "complete", quiet = TRUE)
  expect_true(quiet_received)
})
