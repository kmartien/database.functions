#' @title Add individual haplotype
#'
#' @description This function adds information to an individuals database file.
#' It should work with any species database.
#'
#' @param new.dat matrix or dataframe with info to add to an ind.info database object.
#'   The first column should specifying the Animal.ID (with leading zeros), the
#'   others specifying information to add/update. Column names should match those
#'   of the ind.info object being updated. Standard columns are Location (not
#'   used for stratification, so generally as precise as possible), Sex (coded
#'   as M/F), CR.hap, mito.hap, and Comments. Some species databases have
#'   additional columns. Columns can be omitted from new.dat if there are no data
#'   to add/update for those columns.
#' @param db_fname the name of the file to update, typically {4-letter species identifier}.ind.info
#' @param known.CR.haps a list of known control region haplotypes for the species
#' @param known.mito.haps a list of known mitogenome haplotypes for the species
#' @export add.hap.data

add.hap.data <- function(new.dat, known.CR.haps = NULL, known.mito.haps = NULL, db_fname){

  # check input validity
  if("CR.hap" %in% names(new.dat) &
     !all(na.omit(new.dat$CR.hap) %in% known.CR.haps$HAP)
  ) stop("Invalid input; make sure all haps are in known.CR.haps.rda")

  if("mito.hap" %in% names(new.dat) &
     !all(na.omit(new.dat$mito.hap) %in% known.mito.haps$mito.hap)
  ) stop("Invalid input; make sure all haps are in known.mito.haps.rda")

  if("Sex" %in% names(new.dat)){
    new.dat$Sex <- toupper(new.dat$Sex)
    if(!all(na.omit(new.dat$Sex) %in% c("M", "F"))
    ) stop("Invalid input; sex should be coded as M or F")
  }
  load(paste0("../Pcra.database.data/data/", db_fname, ".rda"))
  num.ind <- nrow(ind.info)

  # Get the columns to be updated from new.dat
  columns_to_update <- colnames(new.dat)[colnames(new.dat) != "Animal.ID"]

  for (i in 1:nrow(new.dat)) {
    # if the Animal.ID doesn't already exist in ind.info, add it
    if(!new.dat$Animal.ID[i] %in% ind.info$Animal.ID) ind.info <- add_row(ind.info, Animal.ID = new.dat$Animal.ID[i])
    ind.info_row <- which(ind.info$Animal.ID == new.dat$Animal.ID[i])
    # Update corresponding row in ind.info
    for (col in columns_to_update) {
      # if the old value is NA, update it, else check for mismatched values (ignoring NA in new.dat)
      if(is.na(ind.info[ind.info_row, col])) {
        ind.info[ind.info_row, col] <- new.dat[i, col]
      } else if (!is.na(new.dat[i,col]) & ind.info[ind.info_row, col] != new.dat[i, col]) {
        stop(paste("The new value", col, "for individual", new.dat$Animal.ID[i], "mismatches"))
      }
    }
  }
  if(nrow(ind.info) > num.ind) print("You added new individuals; remember to also add them to id.key.rda")
  save(ind.info, file=paste0("data/", db_fname, ".rda"))
  return(ind.info)
}
