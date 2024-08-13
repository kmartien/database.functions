#' @title Add stratification scheme
#'
#' @description This function adds a new stratification scheme to Mnov.strata.rda
#'
#' @param new.strat matrix or dataframe with two columns, one specifies SWFSC lab ID (no leading zeros), stratum each sample belongs to
#' @export add.stratification.scheme

add.stratification.scheme <- function(new.strat){
  data("Mnov.strata")

  if (names(new.strat)[2] %in% names(Mnov.strata)) {
    print("This stratification scheme already exists")
  } else {
    Mnov.strata <- full_join(Mnov.strata, new.strat, by = "LABID")
    save(Mnov.strata, file = "data/Mnov.strata.rda")
    write.csv(Mnov.strata, file="data-raw/Mnov.strata.csv", row.names = FALSE)

  }
  return(NULL)
}

