#' @title Add individual sex
#'
#' @description This function adds sex information to the Mnov individuals database file.
#'
#' @param dat matrix or dataframe with two columns, one specifies SWFSC lab ID (no leading zeros), the other specifies sex (M or F)
#' @export add.sex.data

add.sex.data <- function(dat){
  if(length(which(dat$SEX %in% c("M","F"))) < dim(dat)[1]) {
    print("Invalid input; only M or F are allowed, no blanks")
    return(NULL)
  }

  data(ind.info)

  inds.to.update <- filter(ind.info, LABID %in% dat$LABID) %>%
    left_join(dat,by="LABID") %>%
    mutate(match = SEX.x == SEX.y)

  if(length(which((inds.to.update$match==FALSE) > 0))) {
    print("mismatched sexes")
    print(inds.to.update$LABID[which(inds.to.update$match==FALSE)])
    return(NULL)
  } else {
    ind.info$SEX[which(ind.info$LABID %in% inds.to.update$LABID)] <- inds.to.update$SEX.y
    save(ind.info, file = "data/ind.info.rda")
    write.csv(ind.info, file="data-raw/Npac.Mnov.individual.info.csv", row.names = FALSE)
  }
}
