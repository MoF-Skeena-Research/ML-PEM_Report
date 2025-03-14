get_saga <- function() {
  
  # Check OS
  if(Sys.info()[['sysname']] == "Windows") {
    
    sys_drive <- Sys.getenv()[["SystemDrive"]]
    
    # Check for an existing instance of saga_cmd in the usual locations
    message("Searching your system for a valid installation of SAGA GIS")
    
    # Don't search these folders to save time
    windows_folders <- paste(c("\\$Recycle.Bin", "Documents and Settings", "Recovery", 
                               "System Volume Information", "Intel", "OneDriveTemp", 
                               "PerfLogs", "ProgramData", "Users", "Windows", 
                               "Program Files", "Program Files (x86)"), 
                             collapse = "|")
    
    remaining_folders <- grep(windows_folders, 
                              list.dirs(sys_drive, full.names = FALSE, recursive = FALSE),
                              value = TRUE, invert = TRUE)
    
    saga_dirs <- dir(file.path(sys_drive, remaining_folders), 
                     pattern = "saga", ignore.case = TRUE, full.names = TRUE)
    
    saga_dir <- list.files(saga_dirs, pattern = "saga_cmd.exe", full.names = TRUE, recursive = TRUE)
    
    if(length(saga_dir) == 0) {
      message("saga_cmd.exe was not found on your system, proceeding to download")
      
      # If there is a hit, check the version
    } else if(length(saga_dir) == 1) {
      version <- readr::parse_number(system(paste(saga_dir, "-v"), intern = TRUE)[1])
      if(!version < 7.3) {
        message("SAGA GIS found in ", saga_dir)
        saga_cmd <- saga_dir
      } else message("Only an old version of SAGA was found. An upgrade will occur.")
      
    } else {
      message("Multiple instances of saga_cmd.exe found on your system")
      for(i in saga_dir) {
        version <- readr::parse_number(system(paste0('"', i, '" -v'), intern = TRUE)[1])
        if(!version < 7.3) {
          message("SAGA GIS found in ", i)
          saga_cmd <- i
          break
        }
      }
      if(!exists("saga_cmd")) message("Unfortunately, all detected versions of SAGA are outdated. An upgrade will be performed.")
    }
    
    # Check if directory exists from string
    if(!exists("saga_cmd")) {
      temp_dir <- file.path(tempdir(), "saga.zip")
      
      # Check Windows bit version
      if(Sys.info()[["machine"]] %in% c("x86-64", "x86_64", "x64")) {
        curl::curl_download(
          "https://sourceforge.net/projects/saga-gis/files/SAGA%20-%207/SAGA%20-%207.3.0/saga-7.3.0_x64.zip/download", 
          destfile = temp_dir)
        
        # or else download from this URL
      } else {
        curl::curl_download(
          "https://sourceforge.net/projects/saga-gis/files/SAGA%20-%207/SAGA%20-%207.3.0/saga-7.3.0_win32.zip/download", 
          destfile = temp_dir)
      }
      
      unzip(temp_dir, exdir = file.path(paste0(sys_drive, "/SAGA-GIS")))
      saga_cmd <- file.path(paste0(sys_drive, "/SAGA/saga-7.3.0_x64/saga_cmd.exe"))
    }
    
    ###########################################################################
    
    # If system is a linux machine running ubuntu: 
    # Following guide outlined here: https://sourceforge.net/p/saga-gis/wiki/Compiling%20SAGA%20on%20Linux/
    
  } else if(Sys.info()[["sysname"]] == "Linux" && grepl("Ubuntu", Sys.info()[["version"]])) {
    
    # Detect if SAGA exists and version is >= 7.3
    if(file.exists(Sys.which("saga_cmd"))) {
      version <- readr::parse_number(system(paste("saga_cmd -v"), intern = TRUE)[1])
      if(version >= 7.3) {
        saga_cmd <- Sys.which("saga_cmd")
        message("Valid SAGA installation detected, using ", saga_cmd)
      } else message("SAGA detected, but it is an old version. The newer long term release (7.3.0) will be downloaded")
    } else message("No SAGA version detected, beginning download")
    
    if(!exists("saga_cmd")) {
      # Set sudo permissions
      if(!as.logical(system("if sudo -n true 2>/dev/null; then echo 'TRUE'; else echo 'FALSE'; fi", intern = TRUE))) {
        user <- Sys.info()[["user"]]
        pwd <- askpass::askpass("Please enter your OS login password: ")
        system(paste0("echo ", pwd, " | sudo -S echo '", user, " ALL=(ALL) NOPASSWD: ALL' | sudo EDITOR='tee -a' visudo"))
      }
      
      # Need to install prerequisite linux packages
      system(paste(
        "sudo apt-get -y install libwxgtk3.0-dev libtiff5-dev libgdal-dev libproj-dev libexpat-dev wx-common libogdi3.2-dev unixodbc-dev", 
        "sudo apt-get -y install g++ make automake libtool git",
        sep = "\n"
      ))
      
      # Need to create directories and download repository
      # Note it looks like SAGA 7.4.0 is being downloaded but it is actually 7.3.0!
      system(paste(
        "sudo mkdir /home/devel",
        "cd /home/devel",
        "sudo git clone --single-branch -b release-7.4.0 git://git.code.sf.net/p/saga-gis/code saga-gis-code",
        sep = "\n"
      ))
      
      # Compile SAGA
      system(paste(
        "cd /home/devel/saga-gis-code/saga-gis", 
        if(exists("version")) "sudo make clean", "sudo make distclean",
        "sudo autoreconf -fi",
        "sudo ./configure",
        "sudo make", 
        if(exists("version")) "sudo make uninstall",
        "sudo make install",
        sep = "\n"
      ))
      
      saga_cmd <- Sys.which("saga_cmd")
    }
  }
  
  #############################################################################
  
  # For installing SAGA GIS on a MAC...
  # The most consistent and easiest way was to download the lts version from 
  # the osgeo4mac group here: https://github.com/OSGeo/homebrew-osgeo4mac
  # Basically, homebrew needs to be installed and then the repository is cloned
  # and the necessary components are compiled to create SAGA GIS. It would
  # be otherwise very difficult to compile a version on ones own without pitfalls
  
  else if(Sys.info()[["sysname"]] == "Darwin") {
    
    # Check if SAGA is already installed. Installation may not be
    # in PATH on Mac systems so check all folders
    message("Searching your system for a valid installation of SAGA GIS")
    
    saga_dir <- grep("saga_cmd$", system("find / -name saga_cmd 2>/dev/null", intern = TRUE), value = TRUE)
    if(length(saga_dir) == 0) {
      message("No valid installation of SAGA found, proceeding with new installation")
    
      } else if(length(saga_dir) == 1) {
      version <- readr::parse_number(system(paste(saga_dir, "-v"), intern = TRUE)[1])
      if(!version < 7.3) {
        message("Using", saga_dir)
        saga_cmd <- saga_dir
        
      } else {
        message("An old version of SAGA was detected, an upgrade will occur.")
      }
      
    } else if(length(saga_dir) > 1) {
      message("Multiple instances of saga_cmd found on your system")
      for(i in saga_dir) {
        version <- readr::parse_number(system(paste0('"', i, '" -v'), intern = TRUE)[1])
        if(!version < 7.3) {
          message("Using ", i)
          saga_cmd <- i
          break
        }
      }
      if(!exists("saga_cmd")) message("Unfortunately, all instances of saga_cmd are outdated. An upgrade will occur.")
    }
    
    if(!exists("saga_cmd")) {
      # Check for sudo permissions and grant them to the user if missing
      if(!as.logical(system("if sudo -n true 2>/dev/null; then echo 'TRUE'; else echo 'FALSE'; fi", intern = TRUE))) {
        user <- Sys.info()[["user"]]
        pwd <- askpass::askpass("Please enter your OS login password")
        system(paste0("echo ", pwd, " | sudo -S echo '", user, " ALL=(ALL) NOPASSWD: ALL' | sudo EDITOR='tee -a' visudo"))
      }
      
      # Check for brewer installation and install brewer if it's not there
      if(tryCatch(system("brew help"), warning = function(x) TRUE)) 
        system('/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"')
      
      # Install SAGA GIS from osgeo
      system(paste(
        "brew tap osgeo/osgeo4mac", 
        "brew install osgeo-saga-lts", 
        sep = "\n"))
      
      # Write result - Cellar is the homebrew folder
      saga_cmd <- file.path("/usr/local/Cellar/osgeo-saga-lts/7.3.0_1/bin/saga_cmd")
    }
  } else stop("Your operating system is not supported for this function")
  return(saga_cmd)
}
