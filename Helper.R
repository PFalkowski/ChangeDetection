SetupEnvironment <- function(workingDirectory, requiredPackages) 
{
  options(max.print = 1000)
  GetPackages(requiredPackages)
  RequirePackages(requiredPackages)
  SetWorkingDirectory(workingDirectory)
}

GetPackages <- function(PackageNames, repo = 'http://r.meteo.uni.wroc.pl/')
{
  if(any(!has)) install.packages(wants[!has])
  has   <- wants %in% rownames(installed.packages())
  if(any(!has)) install.packages(wants[!has], repos=repo)
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
