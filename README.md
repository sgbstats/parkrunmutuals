# parkrunmutuals

## Supabase data storage

Runtime data is stored in the existing Supabase **Storage** bucket, not committed
to Git. The Shiny app continues to read `parkrunmutuals/data.RDa`; the publisher,
league, benchmarks, home-and-away study and SWB analyses now read their intermediate
datasets from the same bucket.

Configure these environment variables in your local `.Renviron` and on the
server running the app:

- `SUPABASE_URL`: HTTPS project URL (defaults to the existing project).
- `SUPABASE_API_KEY`: server-side legacy service-role JWT for workers, or a JWT
  with the required bucket policies.
- `PARKRUNMUTUALS_SUPABASE_BUCKET`: bucket name (defaults to `prmutuals`).

Create the bucket if it does not exist. Keep it private. Workers need SELECT,
INSERT and UPDATE access to objects; the app only needs SELECT access. Set the
bucket's file-size limit high enough for the existing R data bundles. Keep keys
in secret configuration, never in Git or client-side code.

For GitHub Actions, set the `SUPABASE_API_KEY` repository secret and optionally
the `SUPABASE_URL` and `PARKRUNMUTUALS_SUPABASE_BUCKET` repository variables.
The league workflow writes caches to Supabase and no longer commits or pushes
data. R workers require `httr` and `jsonlite`, alongside their existing packages.

### Migrate existing data first

Pause data-producing jobs while migrating. From the repository root, run:

```powershell
Rscript R\migrate-supabase-cache.R
```

This uploads tracked CSV/RDS/RDa datasets using their existing `data/...` paths.
Unused top-level and app-local bundles are preserved under `data/legacy/...`.
It skips uploads already present in Supabase and downloads every object to check
its checksum against the local file. An upload, download or checksum failure
stops migration without untracking any data. Rerunning resumes safely.

Once verification succeeds, untrack the migrated files:

```powershell
Rscript R\migrate-supabase-cache.R --untrack
```

This verifies again before staging removals from Git, retaining your local
copies. Review and commit those removals. `.gitignore` prevents local migration
copies and new runtime datasets from being added again. Nothing automatically
deletes historical Git data: old clones and full clones still contain it.
Use shallow clones to avoid downloading that history; permanently shrinking
history requires a separately coordinated history rewrite.

### Fetching and resuming

```powershell
Rscript R\prmutuals.R
Rscript R\wnr-parkrun-league.R
```

Result CSVs retain their historical event/number keys. A paginated remote listing
determines which results are missing, even on a machine with no local cache.
Each successful result is uploaded before the next request; stored results are
not fetched again. A missing remote result that already exists as a local legacy
CSV is imported byte-for-byte rather than scraped again. Results are inserted
without overwriting existing objects.
An overlapping insert fails explicitly; rerunning sees the stored result and
skips it.

Runner histories are cached by runner ID and date so reruns on the same day
do not refetch them, while a new day discovers new results. The aggregate runner
histories remain at `data/all_parkruns.RDa` for league and benchmark consumers.
Other studies keep their existing checkpoints and share their own raw-result
caches between dependent scripts. Mutable checkpoints and derived exports are
updated in place.

The existing tracked `error_log.txt` is retained as a legacy event-level skip
list. Failed result requests and invalid results are also recorded by result in
Supabase under `data/result-errors/`, including the URL, error and timestamp.
Later runs skip those failures and report omissions as warnings, while continuing
to fetch other results and publishing the available data. If no results are
available, the existing bundle is left unchanged. Storage failures stop the
run; failures to save the error log are never treated as successfully recorded.

To explicitly retry recorded failures, run from the repository root:

```powershell
Rscript -e "source('R/prmutuals.R'); publish_prmutuals(retry_failed = TRUE)"
```

Previous failure records are retained even after a successful retry. Existing
successful results are never refetched. Every call to the parkrun website,
including failed requests, has a 23-second cooling-off period.

To inspect a recorded failure, use its result filename from the warning:

```powershell
Rscript -e "source('R/supabase-cache.R'); print(cache_read_rds('data/result-errors/Bad1.csv.rds'))"
```

### Tests

```powershell
Rscript tests\run-tests.R
```

Tests use an isolated in-memory Supabase HTTP fixture and injected parkrun
responses; they do not call parkrun or require real credentials. They cover
storage authentication headers, serialization, pagination, errors, deduplication,
resumption, publication/app bundle compatibility, league caching, study and SWB
checkpoints, and verified migration/untracking. Real bucket permissions and
deployment configuration must also be checked by running the verified migration
with your credentials before removing tracked data.
