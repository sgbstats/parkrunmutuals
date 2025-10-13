@echo off
rem Set the path to Rscript
set RSCRIPT="C:\Program Files\R\R-4.4.1\bin\Rscript.exe"
rem %RSCRIPT% --no-init-file -e "install.packages('curl', type='binary', repos='https://cran.rstudio.com/')"

rem Set the path to your R script
set RSCRIPT_FILE="C:\R\git\parkrunmutuals\R\get_all_runs.R"
set RSCRIPT_FILE2="C:\R\git\parkrunmutuals\R\get_all_results.R"
set RSCRIPT_FILE3="C:\R\git\parkrunmutuals\R\all_results.R"
%RSCRIPT% --no-init-file %RSCRIPT_FILE%
%RSCRIPT% --no-init-file %RSCRIPT_FILE2%
%RSCRIPT% --no-init-file %RSCRIPT_FILE3%


pause
