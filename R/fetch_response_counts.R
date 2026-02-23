#' Fetch response participation counts for one or more surveys
#'
#' Retrieves a summary of survey participation from up to three Qualtrics API
#' sources depending on the `type` argument: the Distributions API
#' (invited/started/finished per tracked email send), the Metadata API
#' (authoritative submitted-response count across all channels), and the
#' Response Export API (authoritative in-progress count across all channels).
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
#'     \item{`"metadata"`}{Metadata API columns (`meta_*`).}
#'     \item{`"in_progress"`}{Response Export API column (`export_in_progress`).
#'       Adds one extra API call per survey but provides an authoritative
#'       all-channels in-progress count.}
#'     \item{`"all"`}{All of the above (default).}
#'   }
#'   Multiple values may be combined, e.g. `type = c("distributions", "metadata")`.
#'
#' @return A tibble with one row per survey and a `survey_id` / `survey_name`
#'   identifier pair, followed by the requested source groups:
#'
#'   **Distributions API** (`type` includes `"distributions"`; tracked email
#'   sends only — excludes anonymous/direct-link access):
#'   \describe{
#'     \item{dist_invited}{Unique contacts sent an initial invite
#'       (`stats_sent` summed over `requestType == "Invite"` distributions
#'       only). Reminder and ThankYou distributions target the same contacts
#'       and are excluded to avoid overcounting.}
#'     \item{dist_in_progress}{Started but not submitted via a tracked link
#'       (`stats_started - stats_finished`). Lower-bound estimate.}
#'     \item{dist_completed}{Submitted via a tracked link (`stats_finished`).
#'       Undercounts responses arriving via anonymous or direct links.}
#'     \item{dist_response_rate_complete}{`dist_completed / dist_invited`.
#'       `NA` when `dist_invited == 0`.}
#'     \item{dist_response_rate_total}{`(dist_completed + dist_in_progress) / dist_invited`.
#'       `NA` when `dist_invited == 0`.}
#'   }
#'
#'   **Metadata API** (`type` includes `"metadata"`; all channels):
#'   \describe{
#'     \item{meta_completed}{All submitted responses regardless of channel
#'       (`responsecounts$auditable`). **Authoritative completed count.**}
#'     \item{meta_response_rate}{`meta_completed / dist_invited`. The
#'       Distributions API is fetched internally even when `type = "metadata"`
#'       to provide the denominator. `NA` when `dist_invited == 0`.}
#'   }
#'
#'   **Response Export API** (`type` includes `"in_progress"`; all channels):
#'   \describe{
#'     \item{export_in_progress}{Responses currently in-progress across all
#'       channels. **Authoritative all-channels in-progress count.**}
#'   }
#'
#' @template retry-advice
#' @export
#'
#' @importFrom purrr map_chr map_int
#' @importFrom tibble tibble
#' @importFrom dplyr if_else bind_rows
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
#' # Only metadata counts (Distributions API fetched internally for the rate)
#' fetch_response_counts(surveys$id[1:3], type = "metadata")
#'
#' # Skip the slow in-progress export call
#' fetch_response_counts(surveys$id[1:3], type = c("distributions", "metadata"))
#' }
fetch_response_counts <- function(
    surveyIDs,
    survey_names = names(surveyIDs),
    type = c("all", "distributions", "metadata", "in_progress")
) {

  type <- match.arg(type, several.ok = TRUE)
  if ("all" %in% type) type <- c("distributions", "metadata", "in_progress")

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

      # --- Distributions API ------------------------------------------------
      # Always fetched when metadata is requested (provides dist_invited for
      # the meta_response_rate denominator). Also the sole data source when
      # type = "distributions".
      # Tracks responses from emailed distribution links; does NOT capture
      # anonymous / direct-link access.
      need_dists <- "distributions" %in% type || "metadata" %in% type

      if (need_dists) {
        dists <- fetch_distributions(sid)

        if (nrow(dists) == 0) {
          dist_invited     <- 0L
          dist_in_progress <- 0L
          dist_completed   <- 0L
        } else {
          # Only count unique invitees from the initial 'Invite' distributions.
          # Reminder and ThankYou distributions target overlapping subsets of
          # the same contacts, so summing their stats_sent would overcount
          # the number of unique people invited.
          invite_dists <- if (
            "requestType" %in% names(dists) &&
            any(dists$requestType == "Invite", na.rm = TRUE)
          ) {
            dists[!is.na(dists$requestType) & dists$requestType == "Invite", ]
          } else {
            dists  # fall back to all rows if requestType is absent or never "Invite"
          }

          dist_invited     <- sum(invite_dists$stats_sent, na.rm = TRUE)
          dist_started     <- sum(dists$stats_started,  na.rm = TRUE)
          dist_completed   <- sum(dists$stats_finished, na.rm = TRUE)
          dist_in_progress <- dist_started - dist_completed
        }
      }

      if ("distributions" %in% type) {
        out$dist_invited     <- dist_invited
        out$dist_in_progress <- dist_in_progress
        out$dist_completed   <- dist_completed
        out$dist_response_rate_complete <- dplyr::if_else(
          dist_invited > 0L,
          dist_completed / dist_invited,
          NA_real_
        )
        out$dist_response_rate_total <- dplyr::if_else(
          dist_invited > 0L,
          (dist_completed + dist_in_progress) / dist_invited,
          NA_real_
        )
      }

      # --- Metadata API -----------------------------------------------------
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
          out$meta_completed     <- md$responsecounts$auditable
          out$meta_response_rate <- dplyr::if_else(
            dist_invited > 0L,
            as.double(md$responsecounts$auditable) / dist_invited,
            NA_real_
          )
        }
      } else {
        out$survey_name <- survey_names[[i]]
      }

      # --- Response Export API (in-progress count) --------------------------
      # Authoritative in-progress count across ALL channels (including
      # anonymous / direct-link access). One extra API call per survey.
      if ("in_progress" %in% type) {
        in_progress_data   <- fetch_survey(sid, responses = "in_progress",
                                           quiet = TRUE, verbose = FALSE)
        out$export_in_progress <- nrow(in_progress_data)
      }

      out
    }
  ) |>
    dplyr::bind_rows()
}
