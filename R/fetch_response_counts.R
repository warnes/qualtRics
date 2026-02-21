#' Fetch response participation counts for one or more surveys
#'
#' Retrieves a summary of survey participation from three Qualtrics API
#' sources: the Distributions API (invited/started/finished per tracked email
#' send), the Metadata API (authoritative submitted-response count across all
#' channels), and the Response Export API (authoritative in-progress and
#' completed counts across all channels).
#'
#' @param surveyIDs Character vector of one or more unique survey IDs, as
#'   returned in the `id` column by [all_surveys()]. Elements may optionally be
#'   *named* with human-readable survey names, e.g.
#'   `c("Household Survey" = "SV_abc123")`, in which case those names are used
#'   as `survey_name` values without an extra API call.
#' @param survey_names Character vector of human-readable display names,
#'   parallel to `surveyIDs` (same length, same order).
#'   Defaults to `names(surveyIDs)`, so you can place labels directly on
#'   the `surveyIDs` vector: `c("My Survey" = "SV_abc123")`.
#'   Pass `NULL` explicitly to force name resolution via the Qualtrics
#'   Metadata API ([metadata()]) for every survey.
#' @param type Character vector. Which count sources to include in the result.
#'   One or more of:
#'   \describe{
#'     \item{`"distributions"`}{Distributions API columns (`dist_*`).}
#'     \item{`"metadata"`}{Metadata API columns (`meta_*`) and the derived
#'       `meta_response_rate`.}
#'     \item{`"responses"`}{Response Export API columns (`export_*`) and the
#'       derived `export_response_rate`.}
#'     \item{`"all"`}{All of the above (default).}
#'   }
#'   Multiple values may be combined, e.g. `type = c("metadata", "responses")`.
#'
#' @return A tibble with one row per survey and a `survey_id` / `survey_name`
#'   identifier pair, followed by the requested source groups:
#'
#'   **Distributions API** (`type` includes `"distributions"`; tracked email
#'   sends only — excludes anonymous/direct-link access):
#'   \describe{
#'     \item{dist_invited}{Contacts sent a distribution link (`stats_sent`).}
#'     \item{dist_in_progress}{Started but not submitted via a tracked link
#'       (`stats_started - stats_finished`). Lower-bound estimate.}
#'     \item{dist_completed}{Submitted via a tracked link (`stats_finished`).
#'       Undercounts responses arriving via anonymous or direct links.}
#'   }
#'
#'   **Metadata API** (`type` includes `"metadata"`; all channels):
#'   \describe{
#'     \item{meta_completed}{All submitted responses regardless of channel
#'       (`responsecounts$auditable`). **Authoritative completed count.**}
#'     \item{meta_response_rate}{`meta_completed / dist_invited`. Requires
#'       `"distributions"` to also be included; `NA` otherwise or when
#'       `dist_invited == 0`.}
#'   }
#'
#'   **Response Export API** (`type` includes `"responses"`; all channels):
#'   \describe{
#'     \item{export_completed}{Submitted responses across all channels, from
#'       `fetch_survey(responses = "complete")`. **Authoritative completed
#'       count** (cross-check for `meta_completed`).}
#'     \item{export_in_progress}{Responses currently in-progress across all
#'       channels, from `fetch_survey(responses = "in_progress")`.
#'       **Authoritative in-progress count.**}
#'     \item{export_response_rate}{`export_completed / dist_invited`. Requires
#'       `"distributions"` to also be included; `NA` otherwise or when
#'       `dist_invited == 0`.}
#'   }
#'
#' @template retry-advice
#' @export
#'
#' @importFrom purrr map_chr map_int
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#'
#' @examples
#' \dontrun{
#' # Register your Qualtrics credentials if you haven't already
#' qualtrics_api_credentials(
#'   api_key = "<YOUR-API-KEY>",
#'   base_url = "<YOUR-BASE-URL>"
#' )
#'
#' surveys <- all_surveys()
#'
#' # Single survey — name resolved via Metadata API
#' fetch_response_counts(surveys$id[1])
#'
#' # Multiple surveys — names resolved via Metadata API
#' fetch_response_counts(surveys$id[1:3])
#'
#' # Named survey ID vector — names used directly, no extra API call
#' fetch_response_counts(
#'   c(
#'     "Household eCheckup 2025" = "SV_abc123",
#'     "Business eCheckup 2025"  = "SV_def456"
#'   )
#' )
#'
#' # Explicit survey_names vector — parallel to surveyIDs
#' fetch_response_counts(
#'   surveyIDs    = c("SV_abc123", "SV_def456"),
#'   survey_names = c("Household eCheckup 2025", "Business eCheckup 2025")
#' )
#'
#' # Force name resolution via the API even when surveyIDs has names
#' fetch_response_counts(surveys$id[1:3], survey_names = NULL)
#'
#' # Only metadata and response-export counts (skip Distributions API)
#' fetch_response_counts(surveys$id[1:3], type = c("metadata", "responses"))
#' }
fetch_response_counts <- function(
    surveyIDs,
    survey_names = names(surveyIDs),
    type = c("all", "distributions", "metadata", "responses")
) {

  type <- match.arg(type, several.ok = TRUE)
  if ("all" %in% type) type <- c("distributions", "metadata", "responses")

  check_credentials()
  checkarg_ischaracter(surveyIDs)

  if (!is.null(survey_names)) {
    checkarg_ischaracter(survey_names)
    if (length(survey_names) != length(surveyIDs)) {
      rlang::abort(
        c(
          "`survey_names` must be the same length as `surveyIDs`.",
          "i" = glue::glue(
            "Got {length(surveyIDs)} survey IDs but {length(survey_names)} names."
          )
        )
      )
    }
    # Ensure survey_names is an unnamed vector (names on surveyIDs are labels,
    # not keys into this vector).
    survey_names <- unname(survey_names)
  }

  # Index variable used inside purrr::map to look up the name by position.
  survey_index <- seq_along(surveyIDs)

  purrr::map(
    survey_index,
    function(i) {
      sid <- surveyIDs[[i]]

      out <- tibble::tibble(survey_id = sid, survey_name = NA_character_)

      # --- Source 1: Distributions API ---------------------------------------
      # Tracks responses from emailed distribution links only.
      # Does NOT capture anonymous/direct-link access.
      if ("distributions" %in% type || "metadata" %in% type || "responses" %in% type) {
        # dist_invited is needed for response_rate regardless of whether
        # "distributions" is requested, so always compute it when any rate is needed
        need_dist <- "distributions" %in% type ||
          (("metadata" %in% type || "responses" %in% type))
      }

      dist_invited <- NA_integer_

      if ("distributions" %in% type ||
          "metadata"      %in% type ||
          "responses"     %in% type) {
        dists <- fetch_distributions(sid)

        if (nrow(dists) == 0) {
          dist_invited     <- 0L
          dist_in_progress <- 0L
          dist_completed   <- 0L
        } else {
          dist_invited     <- sum(dists$stats_sent,     na.rm = TRUE)
          dist_started     <- sum(dists$stats_started,  na.rm = TRUE)
          dist_completed   <- sum(dists$stats_finished, na.rm = TRUE)
          dist_in_progress <- dist_started - dist_completed
        }

        if ("distributions" %in% type) {
          out$dist_invited     <- dist_invited
          out$dist_in_progress <- dist_in_progress
          out$dist_completed   <- dist_completed
        }
      }

      # --- Source 2: Metadata API --------------------------------------------
      # Authoritative submitted-response count across ALL channels.
      # Also used to resolve the survey display name when not supplied.
      need_metadata <- "metadata" %in% type || is.null(survey_names)

      if (need_metadata) {
        md <- metadata(sid, get = c("metadata", "responsecounts"))

        out$survey_name <- if (!is.null(survey_names)) {
          survey_names[[i]]
        } else {
          md$metadata$name
        }

        if ("metadata" %in% type) {
          meta_completed <- md$responsecounts$auditable
          out$meta_completed <- meta_completed
          out$meta_response_rate <- dplyr::if_else(
            !is.na(dist_invited) & dist_invited > 0L,
            meta_completed / dist_invited,
            NA_real_
          )
        }
      } else {
        out$survey_name <- survey_names[[i]]
      }

      # --- Source 3: Response Export API ------------------------------------
      # Authoritative completed and in-progress counts across ALL channels.
      if ("responses" %in% type) {
        completed_data <- fetch_survey(sid, responses = "complete",
                                       quiet = TRUE, verbose = FALSE)
        export_completed <- nrow(completed_data)

        in_progress_data <- fetch_survey(sid, responses = "in_progress",
                                         quiet = TRUE, verbose = FALSE)
        export_in_progress <- nrow(in_progress_data)

        out$export_completed   <- export_completed
        out$export_in_progress <- export_in_progress
        out$export_response_rate <- dplyr::if_else(
          !is.na(dist_invited) & dist_invited > 0L,
          export_completed / dist_invited,
          NA_real_
        )
      }

      out
    }
  ) |>
    dplyr::bind_rows()
}
