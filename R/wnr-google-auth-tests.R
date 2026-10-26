source("R/wnr-google-auth.R")

local({
  directory <- tempfile("wnr-auth-test-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  path <- file.path(directory, "credentials.json")
  writeLines("local test credentials", path)
  missing <- file.path(directory, "missing.json")
  account <- list(
    type = "service_account",
    client_email = "test@example.invalid",
    token_uri = "https://oauth2.googleapis.com/token",
    private_key = "-----BEGIN PRIVATE KEY-----\nTEST\n-----END PRIVATE KEY-----\n"
  )
  credentials <- jsonlite::toJSON(account, auto_unbox = TRUE)
  calls <- character()
  secret_path <- NULL
  mock_auth <- function(path) {
    calls <<- c(calls, path)
    if (!identical(path, file.path(directory, "credentials.json"))) {
      secret_path <<- path
      stopifnot(
        file.exists(path),
        identical(jsonlite::fromJSON(path), account)
      )
    }
  }
  expect_error <- function(expression, pattern) {
    error <- tryCatch(force(expression), error = identity)
    stopifnot(inherits(error, "error"), grepl(pattern, conditionMessage(error)))
  }

  stopifnot(auth_wnr_sheets(path, "invalid JSON", auth = mock_auth))
  stopifnot(identical(calls, path))

  stopifnot(auth_wnr_sheets(missing, credentials, "", mock_auth))
  stopifnot(!file.exists(secret_path))

  warning_message <- NULL
  fallback_auth <- function(path) {
    if (identical(path, file.path(directory, "credentials.json"))) {
      stop("bad local credentials")
    }
    mock_auth(path)
  }
  withCallingHandlers(
    auth_wnr_sheets(path, credentials, "", fallback_auth),
    warning = function(warning) {
      warning_message <<- conditionMessage(warning)
      invokeRestart("muffleWarning")
    }
  )
  stopifnot(
    grepl("trying GOOGLE_CREDENTIALS", warning_message),
    !file.exists(secret_path)
  )

  escaped_key <- gsub("\n", "\\n", account$private_key, fixed = TRUE)
  stopifnot(auth_wnr_sheets(missing, credentials, escaped_key, mock_auth))
  windows_key <- gsub("\n", "\r\n", account$private_key, fixed = TRUE)
  stopifnot(auth_wnr_sheets(missing, credentials, windows_key, mock_auth))
  no_key <- account
  no_key$private_key <- NULL
  no_key_json <- jsonlite::toJSON(no_key, auto_unbox = TRUE)
  stopifnot(auth_wnr_sheets(missing, no_key_json, escaped_key, mock_auth))

  expect_error(auth_wnr_sheets(missing, "", "", mock_auth), "No usable")
  expect_error(auth_wnr_sheets(missing, "{bad", "", mock_auth), "valid service-account JSON")
  expect_error(auth_wnr_sheets(missing, "[]", "", mock_auth), "service-account object")
  expect_error(auth_wnr_sheets(missing, no_key_json, "", mock_auth), "requires client_email")
  expect_error(
    auth_wnr_sheets(missing, credentials, "", function(path) {
      secret_path <<- path
      stop("invalid key")
    }),
    "Google authentication using GOOGLE_CREDENTIALS failed"
  )
  stopifnot(!file.exists(secret_path))
})

cat("Google authentication tests passed.\n")
