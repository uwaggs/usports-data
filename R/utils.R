
# check if GitHub CLI is available

gh_cli_available <- function(){
  gh_test <- try(system2("gh", stderr = TRUE, stdout = FALSE), silent = TRUE)

  if ( inherits(gh_test, "try-error") ){
    cli::cli_abort("The Github Command Line Interface is not available on your machine! \\
                   Please visit {.url https://github.com/cli/cli#installation} \\
                   for install instructions.")
  }

  invisible(TRUE)
}

# upload data to releases

upload_to_release <- function(files,
                                  tag,
                                  repo = "uwaggs/usports-data",
                                  overwrite = TRUE){
  # see https://cli.github.com/manual/gh_release_upload

  # validate file paths
  file_available <- file.exists(files)

  # if files are missing, warn the user and update the files vector to keep
  # valid file paths only. If there are no valid file paths, exit the function.
  if ( !all(file_available) ){
    cli::cli_alert_warning("The following file{?s} {?is/are} missing: {.path {files[!file_available]}}")

    if (all(file_available == FALSE)){
      cli::cli_alert_warning("There's nothing left to upload. Exiting!")
      return(invisible(FALSE))
    }
  }
  # keep valid file paths
  files <- files[file_available]

  # Make sure the gh cli is available
  gh_cli_available()

  # create command for the shell
  cli_command <- paste(
    "gh release upload",
    tag,
    paste(files, collapse = " "),
    "-R", repo,
    if(isTRUE(overwrite)) "--clobber" else ""
  )
  # Start shell command
  cli::cli_progress_step("Start upload of {cli::no(length(files))} file{?s} to \\
                         {.url {paste0('https://github.com/', repo, '/releases')}} \\
                         @ {.field {tag}}")

  # This command will error if any error occurs.
  system(cli_command, intern = TRUE)

  cli::cli_progress_done()

  invisible(TRUE)
}

# helper function to add unique identifying info to dataframes

add_info <- function(df, link) {

  # Extract game ID from the link
  start_string <- "boxscores/"
  end_string <- ".xml"
  pattern <- paste0("(?<=", start_string, ").*?(?=", end_string, ")")

  game_id <- stringr::str_extract(link, pattern)

  # Extract season from the link
  season <- stringr::str_extract(link, "\\d{4}-\\d{2}")
  sport <- stringr::str_extract(link, "(?<=sports/)[a-z]+(?=/)")

  # Add game_id and season to the dataframe
  df$game_id <- game_id
  df$season <- season
  df$date <- format(game_id, "%Y%m%d")
  df$sport <- sport
  df$link <- link

  return(df)
}
