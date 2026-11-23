source("tests/test-supabase-storage.R")
source("tests/test-supabase-cache.R")
source("tests/test-supabase-integration.R")
source("tests/test-supabase-consumers.R")
source("R/wnr-google-auth-tests.R")
files <- c(
  list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE),
  list.files("parkrunmutuals", pattern = "\\.R$", full.names = TRUE)
)
invisible(lapply(files, parse))
cat("All storage and consumer tests passed; all R scripts parsed\n")
