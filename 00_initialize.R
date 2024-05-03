
initializeProject <- function(configPath = "~/r_code/config.yaml") {
  # Does two things, (1) installs required packages (2) sets up project config
  # Need to run at the beginning of every transition/revision
  
  neededPackages <- c("yaml", 
                      "data.table",
                      "magrittr",
                      "iterators",
                      "clipr",
                      "ggplot2",
                      "gridExtra",
                      "RODBC",
                      "readxl",
                      "writexl")
  packagesToInstall <- setdiff(neededPackages, intersect(installed.packages(), neededPackages))
  
  sapply(packagesToInstall, FUN = install.packages)
  
  lapply(neededPackages, FUN = require, character.only = TRUE)
  
  configOut <- read_yaml(configPath)
  
  configOut$filesDir <- file.path(configOut$baseDir, configOut$filesDir)
  configOut$pogsDir  <- file.path(configOut$baseDir, configOut$pogsDir)
  
  configOut$rawTieReportPath     <- file.path(configOut$filesDir, configOut$rawTieReportPath)
  configOut$cleanTieReportPath   <- file.path(configOut$filesDir, configOut$cleanTieReportPath)
  configOut$cleanIndexReportPath <- file.path(configOut$filesDir, configOut$cleanIndexReportPath)
  configOut$templatePogsDir      <- file.path(configOut$pogsDir, configOut$templatePogsDir)
  configOut$configsDir           <- file.path(configOut$filesDir, configOut$configsDir)
  configOut$configsTestDir       <- file.path(configOut$filesDir, configOut$configsTestDir)
  
  # Performance library paths
  configOut$performanceDsnFile <- file.path(configOut$productLibraryDsnDir,
                                         paste0(configOut$categoryTag, " performance library.dsn"))
  configOut$performanceLibraryDbPath <- configOut$performanceDsnFile %>%
    readLines %>%
    grep("DBQ=C", ., value = TRUE) %>%
    gsub("DBQ=", "", .)
  configOut$performanceLibraryPath <- configOut$performanceDsnFile %>%
    readLines %>%
    grep("DefaultDir=", ., value = TRUE) %>%
    gsub("DefaultDir=", "", .) %>%
    file.path(paste0(configOut$categoryTag, " performance library.psp"))
  
  # Product library paths
  configOut$productDsnFile <- file.path(configOut$productLibraryDsnDir,
                                     paste0(configOut$categoryTag, " product library.dsn"))
  configOut$productLibraryDbPath <- configOut$productDsnFile %>%
    readLines %>%
    grep("DBQ=C", ., value = TRUE) %>%
    gsub("DBQ=", "", .)
  configOut$productLibraryPath <- configOut$productDsnFile %>%
    readLines %>%
    grep("DefaultDir=", ., value = TRUE) %>%
    gsub("DefaultDir=", "", .) %>%
    file.path(paste0(configOut$categoryTag, " product library.psp"))
  
  
  # the pure data tables
  configOut$storePogDtPath   <- file.path(configOut$filesDir, "storePogDt.csv")
  
  setwd(configOut$functionDir)
  sapply(list.files(), FUN = source)
  setwd("~/r_code")
  
  return(
    configOut
  )
}


