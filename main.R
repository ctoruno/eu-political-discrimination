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
    c("--d"),
    action  = "store_true",
    default = FALSE,
    help    = "Execute Discrimination script and all its estimations"
  ),
  make_option(
    c("--p"),
    action  = "store_true",
    default = FALSE,
    help    = "Execute Polarization script and all its estimations"
  ),
  make_option(
    c("--t"),
    action  = "store_true",
    default = FALSE,
    help    = "Execute Trust script and all its estimations"
  ),
  make_option(
    c("--all"),
    action  = "store_true",
    default = FALSE,
    help    = "Execute ALL scripts and its estimations"
  ),
  make_option(
    c("--verbose"),
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
  if (opt$verbose) {
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
  
  if (opt$d | opt$all){
    verbose_message("Executing Discrimination script and all its estimations...")
    source("src/discrimination.R")
  }
  if (opt$p | opt$all){
    verbose_message("Executing Polarization script and all its estimations...")
    source("src/polarization.R")
  }
  if (opt$t | opt$all){
    verbose_message("Executing Trust script and all its estimations")
    source("src/trust.R")
  }
  
}

if(!interactive()){
  main()
  quit(save = "no", status = 0)
}