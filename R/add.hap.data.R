#' @title Add individual haplotype
#'
#' @description This function adds haplotype (Baker designations) to the Mnov individuals database file.
#'
#' @param dat matrix or dataframe with two columns, one specifies SWFSC lab ID (no leading zeros), the other specifies haplotype
#' @param db_fname the name of the file to update; should be Npac.Mnov.individual.info
#' @export add.hap.data

add.hap.data <- function(dat, db_fname){

  data(known.haps)
  if(length(which(dat$HAP %in% known.haps$HAP)) < dim(dat)[1]) {
    print("Invalid input; make sure all haps are in known.haps.rda")
    return(NULL)
  }

  data("ind.info")

  inds.to.update <- filter(ind.info, LABID %in% dat$LABID) %>%
    left_join(dat,by="LABID") %>%
    mutate(match = HAP.x == HAP.y)

  if(length(which((inds.to.update$match==FALSE) > 0))) {
    print("mismatched haps")
    print(inds.to.update$LABID[which(inds.to.update$match==FALSE)])
    return(NULL)
  } else {
    ind.info$HAP[which(ind.info$LABID %in% inds.to.update$LABID)] <- inds.to.update$HAP.y
    save(ind.info, file="data/ind.info.rda")
    write.csv(ind.info, file="data-raw/Npac.Mnov.individual.info.csv", row.names = FALSE)
  }
}
