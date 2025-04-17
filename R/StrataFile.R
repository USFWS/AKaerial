#' Create or append to a csv file with study area stratum information
#'
#' StrataFile will summarize study area stratum-level information into a csv file
#'
#' StrataFile will take a spatial layer with study area stratification polygons and
#' summarize the areas into a csv file.  The file can either be created entirely or appended
#' to for a single year addition.  The file is intended to be an input into the estimate-generating
#' functions in AKaerial.
#'
#' @author Charles Frost, \email{charles_frost@@fws.gov}
#' @references \url{https://github.com/USFWS/AKaerial}
#'
#' @param method "create" or "append"
#' @param append.to File path for appending
#'
#' @return Text file (.csv) is opened, appended to, and saved, or created and saved.
#'
#' @export
StrataFile <- function(
                        method = "create",
                        append.to = NULL){

if(method=="create"){

ykd.design = "C:/Users/cfrost/OneDrive - DOI/Waterfowl/YKD/data/source_data/YKD_DesignStrata.gpkg"
ykd = StrataSummarySF(strata.file=ykd.design, id="STRATNAME")
ykd$survey = "YKD"

acp.design = "C:/Users/cfrost/OneDrive - DOI/Waterfowl/ACP/data/source_data/ACP_DesignStrata.gpkg"
acp = StrataSummarySF(strata.file=acp.design, id="STRATNAME")
acp$survey = "ACP"

crd.design = "C:/Users/cfrost/OneDrive - DOI/Waterfowl/CRD/data/source_data/CRD_DesignStrata.gpkg"
crd = StrataSummarySF(strata.file=crd.design, id="STRATNAME")
crd$survey = "CRD"

}

MasterStrata = rbind(acp, ykd, crd)

MasterStrata$layer.area = units::drop_units(MasterStrata$layer.area)

write.csv(MasterStrata, "C:/Users/cfrost/OneDrive - DOI/Documents/Data Held for AKaerial/MasterStrata.csv", quote = FALSE, row.names = FALSE)

save(MasterStrata, file="C:/Users/cfrost/OneDrive - DOI/Documents/AKaerial/data/MasterStrata.rda")

}





