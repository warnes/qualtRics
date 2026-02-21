vcr::vcr_configure(dir = "../fixtures")

# Load real API credentials from .Renviron in the package root if it exists.
# This allows live-API tests to run locally when real credentials are available.
renviron <- file.path(rprojroot::find_package_root_file(), ".Renviron")
if (file.exists(renviron)) readRenviron(renviron)

# If credentials were not supplied by .Renviron, fall back to test sentinels so
# that cassette-based tests still work.  The sentinel base URL (iad1.qualtrics.com)
# matches the URIs recorded in the vcr fixtures, and causes skip_if_no_real_api()
# to skip any test that requires a live API call.
if (Sys.getenv("QUALTRICS_API_KEY")  == "") Sys.setenv(QUALTRICS_API_KEY  = "test-key")
if (Sys.getenv("QUALTRICS_BASE_URL") == "") Sys.setenv(QUALTRICS_BASE_URL = "iad1.qualtrics.com")
