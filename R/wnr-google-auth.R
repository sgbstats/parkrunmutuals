auth_wnr_sheets <- function(
  path = "credentials.json",
  credentials = Sys.getenv("GOOGLE_CREDENTIALS"),
  private_key = Sys.getenv("GOOGLE_PRIVATE_KEY"),
  auth = googlesheets4::gs4_auth
) {
  if (file.exists(path)) {
    authenticated <- tryCatch(
      {
        auth(path = path)
        TRUE
      },
      error = function(error) {
        warning(
          "Google authentication with ", path,
          " failed; trying GOOGLE_CREDENTIALS instead.",
          call. = FALSE
        )
        FALSE
      }
    )
    if (authenticated) {
      return(invisible(TRUE))
    }
  }

  if (!nzchar(credentials)) {
    stop(
      "No usable Google credentials. Provide a valid ", path,
      " or set GOOGLE_CREDENTIALS to the service-account JSON.",
      call. = FALSE
    )
  }
  account <- tryCatch(
    jsonlite::fromJSON(credentials, simplifyVector = FALSE),
    error = function(error) {
      stop(
        "GOOGLE_CREDENTIALS must contain valid service-account JSON.",
        call. = FALSE
      )
    }
  )
  if (!is.list(account) || !identical(account$type, "service_account")) {
    stop("GOOGLE_CREDENTIALS must contain a service-account object.", call. = FALSE)
  }
  if (nzchar(private_key)) {
    account$private_key <- private_key
  }
  required <- c("client_email", "token_uri", "private_key")
  if (!all(vapply(account[required], function(value) {
    is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
  }, logical(1)))) {
    stop(
      "GOOGLE_CREDENTIALS requires client_email, token_uri and private_key; ",
      "GOOGLE_PRIVATE_KEY may supply the private_key separately.",
      call. = FALSE
    )
  }
  account$private_key <- gsub("\\n", "\n", account$private_key, fixed = TRUE)
  account$private_key <- gsub("\r\n?", "\n", account$private_key)
  secret_path <- tempfile("wnr-google-", fileext = ".json")
  on.exit(unlink(secret_path), add = TRUE)
  jsonlite::write_json(account, secret_path, auto_unbox = TRUE)
  Sys.chmod(secret_path, mode = "0600")
  tryCatch(
    auth(path = secret_path),
    error = function(error) {
      stop(
        "Google authentication using GOOGLE_CREDENTIALS failed. ",
        "Check that the service-account key is valid and matches client_email. ",
        "Original error: ", conditionMessage(error),
        call. = FALSE
      )
    }
  )
  invisible(TRUE)
}
