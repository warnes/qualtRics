# cache.R — on-disk caching for qualtRics API responses
#
# All Qualtrics API calls are potentially slow (network + large data).  This
# module wraps the two internal primitives that make actual HTTP calls:
#
#   - qualtrics_api_request()  — used by all lightweight GET endpoints
#   - export_responses_filedownload()  — used by fetch_survey(); downloads and
#     reads the response-export CSV zip
#
# Caching is implemented with memoise + cachem::cache_disk().  The on-disk
# cache persists across R sessions.  Each cached value expires after `ttl`
# seconds (default 86400 = 24 h).  Setting ttl = 0 disables caching entirely.
#
# User-facing controls:
#   qualtrics_configure_cache(ttl, dir)  — set options and rebuild cache
#   qualtrics_clear_cache()              — delete all cached values


# ---------------------------------------------------------------------------
# Internal cache state (package-level environment)
# ---------------------------------------------------------------------------

.qualtrics_cache_env <- new.env(parent = emptyenv())
.qualtrics_cache_env$api_request    <- NULL   # memoised qualtrics_api_request
.qualtrics_cache_env$file_download  <- NULL   # memoised export_responses_filedownload
.qualtrics_cache_env$fetch_and_process <- NULL  # memoised .fetch_and_process_impl


# ---------------------------------------------------------------------------
# Helpers: read current TTL / cache dir from options
# ---------------------------------------------------------------------------

#' @keywords internal
qualtrics_cache_ttl <- function() {
  getOption("qualtRics.cache_ttl", default = 86400L)
}

#' @keywords internal
qualtrics_cache_dir <- function() {
  getOption(
    "qualtRics.cache_dir",
    default = tools::R_user_dir("qualtRics", which = "cache")
  )
}


# ---------------------------------------------------------------------------
# Build / rebuild the memoised functions
# ---------------------------------------------------------------------------

#' Build (or rebuild) memoised API wrappers from current options
#'
#' Called automatically on first use and after [qualtrics_configure_cache()].
#' @keywords internal
.build_cache <- function() {
  ttl <- qualtrics_cache_ttl()
  dir <- qualtrics_cache_dir()

  if (ttl == 0) {
    # Caching disabled: point to the originals
    .qualtrics_cache_env$api_request      <- qualtrics_api_request
    .qualtrics_cache_env$file_download    <- export_responses_filedownload
    .qualtrics_cache_env$fetch_and_process <- .fetch_and_process_impl
    return(invisible(NULL))
  }

  # Create on-disk cache object with TTL
  cache <- cachem::cache_disk(dir = dir, max_age = ttl)

  .qualtrics_cache_env$api_request <-
    memoise::memoise(qualtrics_api_request, cache = cache)

  .qualtrics_cache_env$file_download <-
    memoise::memoise(export_responses_filedownload, cache = cache)

  .qualtrics_cache_env$fetch_and_process <-
    memoise::memoise(.fetch_and_process_impl, cache = cache)

  invisible(cache)
}

#' Ensure memoised wrappers are initialised (lazy init on first use)
#' @keywords internal
.ensure_cache <- function() {
  if (is.null(.qualtrics_cache_env$api_request)) {
    .build_cache()
  }
}


# ---------------------------------------------------------------------------
# Internal call-through helpers (used by all API functions)
# ---------------------------------------------------------------------------

#' Cached wrapper around qualtrics_api_request()
#'
#' Routes through the memoised version when caching is enabled, otherwise calls
#' the original directly.  Only GET requests are cached; POST requests bypass
#' the cache because they have server-side side-effects.
#'
#' @param verb,url,query,body,as,... Passed to [qualtrics_api_request()].
#' Never cache POST requests; bypass with [qualtRics::qualtrics_configure_cache()].
#' @keywords internal
cached_api_request <- function(verb = c("GET", "POST"),
                               url = url,
                               query = NULL,
                               body = NULL,
                               as = c("parsed", "raw"),
                               ...) {
  verb <- rlang::arg_match(verb)
  .ensure_cache()

  # Never cache POST requests (they trigger export jobs on the server)
  if (verb == "POST" || qualtrics_cache_ttl() == 0) {
    return(qualtrics_api_request(verb = verb, url = url, query = query,
                                 body = body, as = as, ...))
  }

  .qualtrics_cache_env$api_request(verb = verb, url = url, query = query,
                                   body = body, as = as, ...)
}

#' Cached wrapper around export_responses_filedownload()
#'
#' @param surveyID,fileID,tmp_dir Passed to [export_responses_filedownload()].
#' @keywords internal
cached_file_download <- function(surveyID, fileID, tmp_dir) {
  .ensure_cache()
  .qualtrics_cache_env$file_download(surveyID = surveyID,
                                     fileID   = fileID,
                                     tmp_dir  = tmp_dir)
}


#' Underlying implementation for cached_fetch_and_process()
#'
#' Accepts all explicit parameters so the memoised wrapper can key on them.
#' @keywords internal
.fetch_and_process_impl <- function(surveyID, body, verbose, tmp_dir,
                                    import_id, time_zone, col_types, quiet,
                                    add_column_map, add_var_labels, strip_html,
                                    convert, label) {
  rawdata <- export_responses_request(
    surveyID = surveyID,
    body     = body,
    verbose  = verbose,
    tmp_dir  = tmp_dir
  )
  d <- process_raw_survey(
    rawdata        = rawdata,
    import_id      = import_id,
    time_zone      = time_zone,
    col_types      = col_types,
    quiet          = quiet,
    add_column_map = add_column_map,
    add_var_labels = add_var_labels,
    strip_html     = strip_html
  )
  if (convert && label) d <- infer_data_types(d, surveyID, col_types = col_types)
  d
}

#' Cached wrapper around .fetch_and_process_impl()
#'
#' Caches the fully-processed data frame (factors, labels, column map) keyed on
#' all parameters that affect the result.  This is the most expensive step in
#' [fetch_survey()], so caching it gives the biggest speedup.
#' @keywords internal
cached_fetch_and_process <- function(surveyID, body, verbose, tmp_dir,
                                     import_id, time_zone, col_types, quiet,
                                     add_column_map, add_var_labels, strip_html,
                                     convert, label) {
  .ensure_cache()
  .qualtrics_cache_env$fetch_and_process(
    surveyID = surveyID, body = body, verbose = verbose, tmp_dir = tmp_dir,
    import_id = import_id, time_zone = time_zone, col_types = col_types,
    quiet = quiet, add_column_map = add_column_map,
    add_var_labels = add_var_labels, strip_html = strip_html,
    convert = convert, label = label
  )
}


# ---------------------------------------------------------------------------
# User-facing functions
# ---------------------------------------------------------------------------

#' Configure the qualtRics on-disk response cache
#'
#' Sets the time-to-live and/or directory for the cache used by all qualtRics
#' API functions, then rebuilds the internal memoised wrappers to pick up the
#' new settings.
#'
#' Cache settings are stored as R options (`qualtRics.cache_ttl` and
#' `qualtRics.cache_dir`) and therefore apply for the lifetime of the current R
#' session.  To make them persistent, add the corresponding
#' `options(qualtRics.cache_ttl = ...)` call to your `.Rprofile`.
#'
#' @param ttl Numeric. Cache time-to-live in **seconds**.  Cached values older
#'   than `ttl` are ignored and re-fetched from the API.  Set to `0` to disable
#'   caching entirely.  Defaults to the current value of the
#'   `qualtRics.cache_ttl` option, or `86400` (24 hours) if unset.
#' @param dir String. Path to the directory used for on-disk cache storage.
#'   Defaults to the current value of the `qualtRics.cache_dir` option, or a
#'   platform-appropriate user cache directory (`tools::R_user_dir("qualtRics",
#'   "cache")`) if unset.
#'
#' @return Invisibly, the updated `ttl` and `dir` as a named list.
#' @export
#'
#' @examples
#' \dontrun{
#' # Cache responses for 1 hour
#' qualtrics_configure_cache(ttl = 3600)
#'
#' # Use a project-specific cache directory, valid for 12 hours
#' qualtrics_configure_cache(ttl = 43200, dir = "~/.cache/my_project/qualtRics")
#'
#' # Disable caching for this session
#' qualtrics_configure_cache(ttl = 0)
#' }
qualtrics_configure_cache <- function(
    ttl = qualtrics_cache_ttl(),
    dir = qualtrics_cache_dir()
) {
  checkarg_isnumeric(ttl)
  if (ttl < 0) rlang::abort("`ttl` must be >= 0.")
  checkarg_isstring(dir)

  options(qualtRics.cache_ttl = ttl,
          qualtRics.cache_dir = dir)

  # Force rebuild of memoised wrappers with new settings
  .qualtrics_cache_env$api_request   <- NULL
  .qualtrics_cache_env$file_download <- NULL
  .build_cache()

  invisible(list(ttl = ttl, dir = dir))
}


#' Clear the qualtRics on-disk response cache
#'
#' Deletes all cached API responses.  The next call to any qualtRics function
#' will fetch fresh data from the Qualtrics API.
#'
#' @param dir String. Cache directory to clear.  Defaults to the currently
#'   configured cache directory (see [qualtRics::qualtrics_configure_cache()]).
#'
#' @return Invisibly, the path to the cache directory that was cleared.
#' @export
#'
#' @examples
#' \dontrun{
#' qualtrics_clear_cache()
#' }
qualtrics_clear_cache <- function(dir = qualtrics_cache_dir()) {
  checkarg_isstring(dir)
  if (dir.exists(dir)) {
    cache <- cachem::cache_disk(dir = dir)
    cache$reset()
    rlang::inform(c("v" = glue::glue("qualtRics cache cleared: {dir}")))
  } else {
    rlang::inform(c("i" = glue::glue("Cache directory does not exist (nothing to clear): {dir}")))
  }
  invisible(dir)
}
