#' Deploy Shiny application
#'
#' @param gh_organization Github organization. Defaults to "OHI-Science".
#' @param gh_repo Github repository
#' @param gh_branch_app Github branch that contains the app. Defaults to "master" and
#' does not have to already exist in the repo.
#' @param app_url URL of the application
#' @param app_server the secure copy (scp) server location given by username@server:/dir/
#' @param run_app run the Shiny app locally
#' @param open_url open the web browser to the app_url
#' @param dir_local top-level directory to use for populating git repo folder
#'   and branch subfolders within, defaults to tmpdir()
#' @param del_out whether to delete output directory when done, defaults to TRUE
#' @param dir_server directory on the app_server
#' @param gh_data_commit commit of gh_branch_data for freezing the app,
#'   ie do not automatically update
#'
#' @return Returns URL of Shiny app if successfully deployed, otherwise errors
#'   out. Requires git credentials to push to Github repository,
#' and SSH keys for secure copying to server.
#' Suggestions to update Shiny app:
#' \enumerate{
#'  \item Ensure permissions are set for you and shiny on fitz:
#'        \verb{ssh jstewart@fitz
#'              sudo chown -R jstewart /srv/shiny-server/ohi-global
#'              sudo chmod -R 775 .
#'              sudo chgrp -R shiny .}
#'  \item Copy your repo to dir_local locally. For example, if input
#'        argument \code{dir_local = '~/Desktop/ohirepos_tmp'},
#'        copy \code{'~/github/ohi-global'}
#'        to \code{dir_local = '~/Desktop/ohirepos_tmp/ohi-global/draft'}.
#' }
#'  Please also visit
#' \link[=https://github.com/OHI-Science/ohirepos/blob/master/inst/app/README.md]{app/README.md}
#'   for more details.
#'
#' @examples
#' \dontrun{
#' deploy_app('ohi-global', 'Global', c('eez2015','eez2012','eez2013','eez2014','eez2016'),
#'   projection = 'Mollweide')
#' deploy_app(       'bhi', 'Baltic', 'baltic2015')
#' }
#' @import tidyverse yaml devtools
#' @export

deploy_app <- function(gh_organization = 'OHI-Science',
                       gh_repo,
                       gh_shiny_dir    = NULL,
                       gh_branch_app   = 'master',
                       app_base_url    = 'http://ohi-science.nceas.ucsb.edu',
                       app_name_remote = NULL,
                       app_server, #      = 'jstewart@128.111.84.76',
                       install_pkgs    = FALSE,
                       dir_server      = '/srv/shiny-server',
                       dir_local       = tempdir()) {

  library(tidyverse)
  library(yaml)

  ##### create run_cmd() function for system() commands #####
  run_cmd <- function(cmd) {
    message('running command:\n  ', cmd)
    message('...elapsed time: ', system.time(system(cmd))[3] %>% round(3), ' sec')
  }


  ################################################.
  ##### construct local and remote filepaths #####
  ################################################.

  dir_branches <- file.path(dir_local, gh_repo)
  ### local repo location; this will get deleted at the end of the function
  ### if del_out == TRUE
  dir_repo_local <- file.path(dir_branches, gh_branch_app)
  ### local folder to copy the repo into
  dir_app_local  <- file.path(dir_repo_local, gh_shiny_dir)
  ### local folder where the app files reside
  gh_url <- sprintf('https://github.com/%s/%s.git', gh_organization, gh_repo)
  ### url of the repo from which app will be pulled

  if (is.null(app_name_remote) & !is.null(gh_shiny_dir)) {
    dir_app_remote <- gh_shiny_dir
    ### the folder on Fitz where the shiny app will reside; if no explicit
    ### name is given, will default to the repo name/shiny folder name
  } else if (is.null(gh_shiny_dir)) {
    dir_app_remote <- gh_repo
  } else {
    dir_app_remote <- app_name_remote
  }

  app_url <- file.path(app_base_url, dir_app_remote)


  ###################################################.
  ##### fetch app from GitHub location to local #####
  ###################################################.

  ### Create local repo to store app (won't overwrite existing though)
  dir.create(dir_repo_local, showWarnings = FALSE, recursive = TRUE)

  ### fetch existing, or clone new if app isn't in local repo copy
  if (!file.exists(dir_app_local)){
    # clone app branch, shallowly and quietly
    message('Local app directory does not exist; cloning into local directory')
    run_cmd(sprintf('git clone -q --depth 1 --branch %s %s %s', gh_branch_app, gh_url, dir_repo_local))
  } else {
    # git fetch & overwrite
    message('Local app directory exists; fetching updates into local directory')
    run_cmd(sprintf('cd %s; git fetch -q; git reset -q --hard origin/%s; git checkout -q %s; git pull -q',
                    dir_repo_local, gh_branch_app, gh_branch_app))
  }


  ###########################################################.
  ##### Check for missing packages, install if possible #####
  ###########################################################.

  ### Determine required packages from library() or require() in any R scripts
  script_files <- list.files(dir_app_local, pattern = '\\.R$|\\.r$', full.names = TRUE)
  pkgs_rqd <- lapply(script_files, FUN = function(x) {
    grep("library|require", readLines(x), value = TRUE) %>%
      stringr::str_extract('(?<=\\().*?(?=\\))')
  }) %>%
    unlist() %>%
    unique()


  ### If install_pkgs == TRUE and the user has superuser privileges, install
  ### missing packages; otherwise just report what they are.
  pkg_check_cmd <- sprintf("ssh %s Rscript -e 'installed.packages\\(\\)[,1]'", app_server)
  pkgs_installed <- system(pkg_check_cmd, intern = TRUE) %>%
    stringr::str_split('[\\" ]+') %>%
    unlist() %>%
    unique()

  pkgs_missing <- pkgs_rqd[!pkgs_rqd %in% pkgs_installed]

  if(length(pkgs_missing) == 0) {

    message('All packages required by the app seem to be installed...')

  } else {
    ### report missing packages
    message('The following required packages are not installed on the remote server:')
    message('  ', paste(pkgs_missing, collapse = ', '))

    if(install_pkgs == TRUE) {
      pkg_string <- paste0('\\"', pkgs_missing, '\\"') ### put quotation marks...

      for(pkg in pkg_string) { # pkg = pkg_string[1]
        message('Attempting to install ', pkg, ' on remote server.')
        install_pkg_cmd <- sprintf("ssh %s Rscript -e 'install.packages\\(%s\\)'", app_server, pkg)
        run_cmd(install_pkg_cmd)
      }
    }
  }


  #######################################.
  ##### write app.yml configuration #####
  #######################################.

  message('...writing app.yml')
  write_file(
    as.yaml(list(
      gh_organization = gh_organization,
      gh_repo         = gh_repo,
      gh_shiny_dir    = gh_shiny_dir,
      gh_branch_app   = gh_branch_app,
      app_url         = app_url,
      debug           = FALSE,
      pkgs_required   = paste(pkgs_rqd, collapse = ', '),
      last_updated    = Sys.Date())),
    file.path(dir_repo_local, 'app.yml'))


  #############################################.
  ##### copy app files from local to Fitz #####
  #############################################.

  cmds <- c(
    sprintf('ssh %s mkdir %s/%s', app_server, dir_server, dir_app_remote),
    # sprintf('cd %s; rsync -rq --exclude .git %s:%s/%s',
    #         dir_app_local, app_server, dir_server, dir_app_remote),
    sprintf('scp -r %s/* %s:%s/%s',
            dir_app_local, app_server, dir_server, dir_app_remote)
  )
  for (cmd in cmds){ ### cmd <- cmds[3]
    run_cmd(cmd)
  }

  ################################.
  ##### Wrap up and clean up #####
  ################################.

  ### Run app from local URL to test it
  message('Loading app into browser from ', app_url)
  utils::browseURL(app_url)

  ### Remove temp files
  message('Removing temp files from ', dir_branches)
  unlink(dir_branches, recursive = TRUE, force = TRUE)

}

deploy_app(gh_organization = 'OHI-Science',
           gh_repo         = 'IUCN-Aquamaps',
           gh_shiny_dir    = 'shiny_am_iucn',
           gh_branch_app   = 'master',
           app_base_url    = 'http://ohi-science.nceas.ucsb.edu',
           app_name_remote = 'plos_marine_rangemaps',
           app_server      = 'ohara@fitz.nceas.ucsb.edu',
           dir_server      = '/srv/shiny-server',
           dir_local       = tempdir(),
           install_pkgs    = TRUE)

#
## Julie test
# gh_organization = 'OHI-Science';
# gh_repo         = 'IUCN-Aquamaps';
# gh_shiny_dir    = 'shiny_am_iucn';
# gh_branch_app   = 'master';
# app_base_url    = 'http://ohi-science.nceas.ucsb.edu';
# app_name_remote = 'plos_marine_rangemaps';
# app_server      = 'jstewart@fitz.nceas.ucsb.edu';
# dir_server      = '/srv/shiny-server';
# dir_local       = tempdir();
# install_pkgs    = TRUE;


