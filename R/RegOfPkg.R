if(!require("repmis"))
  install.packages("repmis")
library("repmis")

## Process Available Packages
## Uncomment the following three lines when creating new package list
# AP <- available.packages()
# AP <- AP[,-c(3,9:17)]
# save(AP, file="ApprovedPkgs.Rdata")

# Load AP, available packages
load(file="data/ApprovedPkgs.Rdata") 

PackagesName <- AP[,1]

ConnectedPackages <- function( PkgName , Relation)
{
  DataColumnNames <- colnames(AP)
  AllRelations <- DataColumnNames[3:7]
  AllRelations <- c("all",AllRelations)  
  
  CreateList <- NULL
  PkgIndex <- which(PackagesName == PkgName)
  if( is.element(Relation, AllRelations))
  {
    if (is.element(Relation , AllRelations[2:6]))
    {
      DepList <- unlist(strsplit(AP[PkgIndex,Relation], ","))
      DepList <- gsub(" ", "", DepList)
      DepList <- gsub('\n', '', DepList)
      DepList <- DepList[!grepl('R[(]>=[1-3].',DepList)]
      DepList <- gsub("[(]>.+[)]","",DepList)  
      CreateList <- c(PkgName,DepList)
      CreateList <- CreateList[!is.na(CreateList)]
    }
    else
    {
      DepList1 <- unlist(strsplit(AP[PkgIndex,"Depends"], ","))
      DepList1 <- gsub(" ", "", DepList1)
      DepList1 <- gsub('\n', '', DepList1)
      DepList1 <- DepList1[!grepl('R[(]>=[1-3].',DepList1)]
      DepList1 <- gsub("[(]>.+[)]","",DepList1)  
      
      DepList2 <- unlist(strsplit(AP[PkgIndex,"Imports"], ","))
      DepList2 <- gsub(" ", "", DepList2)
      DepList2 <- gsub('\n', '', DepList2)
      DepList2 <- DepList2[!grepl('R[(]>=[1-3].',DepList2)]
      DepList2 <- gsub("[(]>.+[)]","",DepList2)  
      
      DepList3 <- unlist(strsplit(AP[PkgIndex,"LinkingTo"], ","))
      DepList3 <- gsub(" ", "", DepList3)
      DepList3 <- gsub('\n', '', DepList3)
      DepList3 <- DepList3[!grepl('R[(]>=[1-3].',DepList3)]
      DepList3 <- gsub("[(]>.+[)]","",DepList3)  
      
      CreateList <- unique(c(PkgName,DepList1,DepList2,DepList3))
      CreateList <- CreateList[!is.na(CreateList)]
    }
  }
  else
  {
    cat("Wrong Relation!\n")
  }
  if( length(CreateList) == 0)
  {
    cat(PkgName," is wrong\n")
  }
  CreateList
}

CreateNeighbor <- function(PkgList, Relation)
{
  PackageNetwork <- NULL
  for(i in 1:length(PkgList))
  {
    PkgIndex <- which(PackagesName == PkgList[i])
    RelationList <- ConnectedPackages(PkgList[i], Relation)
    if(length(RelationList) >0 )
    {
      for (j in 1:length(RelationList))
      {
        if(is.element(RelationList[j], PackagesName))
        {
          RelPkgIndex <- which(PackagesName == RelationList[j])
          PackageNetwork <- c(PackageNetwork, c(PkgIndex, RelPkgIndex))
        }
      }
    }
  }
  names(PackagesName[unique(PackageNetwork)])
}

InstallApprovedPackages <- function(PkgList){
  AllPkgList <- CreateNeighbor(PkgList,"all")
  AllPkgVer <- AP[AllPkgList,"Version"]
  message("Start install the following packages:")
  for(pkgName in AllPkgList){
    message(pkgName)  
  }
  repmis::InstallOldPackages(AllPkgList, AllPkgVer)
  message("Move the following package directories to another machine.")
  for(pkgName in AllPkgList){
    message(pkgName)  
  }
}