# Stock index now

Tracking exchange data from https://stooq.com using Github action.

## Workflows: Scrape latest global index

* Scrape daily: `5 1,5,9,13,17,21 * * *`
* Create csv file monthly: `index/{{ index }}.csv`

## Workflow: Report <WIP>


## Target index
* KOSPI - South Korea
* KOSDQ - South Korea
* S&P 500 - U.S.
* NASDAQ - U.S.
* Dow Jones - U.S.
* Nikkei 225 - Japan
* Shanghai - China
* DAX - Germany


docker run --rm -it \
    -v $(pwd):/home/runner/work \
    -v ${HOME}/data/renv_cache:/mnt/shared/renv/cache \
    -w /home/runner/work \
    -e R_LIBS_="~/.local/share/rlibs" \
    -e RENV_PATHS_ROOT="~/.local/share/renv" \
    -e RENV_PATHS_CACHE="/mnt/shared/renv/cache" \
    rocker/r-ver:4.0.3 \
    bash

adduser runner
usermod -aG sudo runner
chown -R runner:runner /home/runner
apt update
apt install -y libcurl4-openssl-dev libssl-dev libxml2-dev zlib1g-dev

su -l runner
cd work

export RENV_PATHS_ROOT=/home/runner/.local/share/renv
export RENV_PATHS_CACHE=/mnt/shared/renv/cache


apt-get update && apt-get install -y libcurl4-openssl-dev libssl-dev libxml2-dev zlib1g-dev

mkdir -p /home/runner/.local/share/renv


R

.libPaths("/home/runner/.local/share/renv")

if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::restore()

renv::snapshot()

.libPaths( c(Sys.getenv("RENV_PATHS_ROOT"), .libPaths()))

library(renv)

pkgs = c("httr", "rvest", "data.table", "stringr", "xts", "PerformanceAnalytics", "magrittr", "tidyr", "readr", "lubridate", "timetk", "dplyr", "arrow")
new.pkgs = pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(new.pkgs)) renv::install(new.pkgs)

arrow::arrow_available()
arrow::install_arrow()
