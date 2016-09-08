SetupEnvironment <- function(workingDirectory, requiredPackages) 
{
  options(max.print = 1000)
  GetPackages(requiredPackages)
  RequirePackages(requiredPackages)
  SetWorkingDirectory(workingDirectory)
}

GetPackages <- function(PackageNames, repo = 'http://r.meteo.uni.wroc.pl/')
{
  packages.needed <- c(PackageNames)
  new.packages <- packages.needed[!(packages.needed %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages, repos=repo)
}

RequirePackages <- function(PackageNames)
{
  lapply(PackageNames, require, character.only = TRUE)
}

SetWorkingDirectory <- function(path)
{
  path = gsub("([\\])","/", path)
  if (!grepl(path, getwd()))
  {
    setwd(path)
  }
}
