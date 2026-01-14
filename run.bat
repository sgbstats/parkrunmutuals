@echo off
rem Set the path to Rscript
set RSCRIPT="C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
rem %RSCRIPT% --no-init-file -e "install.packages('curl', type='binary', repos='https://cran.rstudio.com/')"

cd "C:/R/git/parkrunmutuals"
rem Set the path to your R script
set RSCRIPT_FILE="R\get_all_runs.R"
set RSCRIPT_FILE2="R\get_all_results.R"
set RSCRIPT_FILE3="R\all_results.R"
set RSCRIPT_FILE4="R/wnr_parkrun_league.R"
%RSCRIPT% --no-init-file %RSCRIPT_FILE%
%RSCRIPT% --no-init-file %RSCRIPT_FILE2%
%RSCRIPT% --no-init-file %RSCRIPT_FILE3%
%RSCRIPT% --no-init-file %RSCRIPT_FILE4%


pause
