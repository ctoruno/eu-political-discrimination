## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Main Entry Point
## Author(s):         Carlos Toruno   (ctoruno@worldjusticeproject.org)
## Creation date:     May 7th, 2025
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(optparse)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1. Command Line Options ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Define command line options
option_list <- list(
  make_option(
    c("--v1"),
    action  = "store_true",
    default = FALSE,
    help    = "Execute routines related to V1 of the the paper and all its estimations"
  ),
  make_option(
    c("--v2"),
    action  = "store_true",
    default = FALSE,
    help    = "Execute routines related to V2 of the the paper and all its estimations"
  ),
  make_option(
    c("--all"),
    action  = "store_true",
    default = FALSE,
    help    = "Execute ALL scripts and its estimations"
  ),
  make_option(
    c("--noverbose"),
    action  = "store_true",
    default = FALSE,
    help    = "Display verbose"
  )
)

# Parse command line options
opt_parser <- OptionParser(
  option_list     = option_list,
  add_help_option = TRUE,
  description     = "R project for the EU Political Discrimination Paper",
  epilogue        = "Example: Rscript main.R --all"
)
opt <- parse_args(opt_parser)

# Helper function to print verbose messages
verbose_message <- function(message) {
  if (opt$noverbose) {
    cat(paste0("[INFO] ", message, "\n"))
  }
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Entry Point Exec ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

main <- function(){
  
  renv::activate()
  
  source("src/config.R")
  source("src/data_loading.R")
  
  if (opt$v1 | opt$all){
    verbose_message("Executing Discrimination script and all its estimations...")
    source("src/discrimination.R")
    source("src/polarization.R")
    source("src/trust.R")
  }
  if (opt$p | opt$all){
    verbose_message("Executing Polarization script and all its estimations...")
    source("src/extended.R")
  }
  
}

if(!interactive()){
  main()
  quit(save = "no", status = 0)
}