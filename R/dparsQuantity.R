## deparse Point$quantity
.dparsQuantity <- function(avp)

{
  pts <- names(avp) == "Point"
  allPST <- unlist(avp[pts])
  qt <- grep( ".quantity",names(allPST))
  qt <- as.numeric(allPST[qt])
  mqt <- mean(qt, na.rm = TRUE)
  uqt <- unique(qt)
  nbqt <- length(uqt)
  qti <- ifelse(length(nbqt)>1, "variableValue", "constantValue")
  list(mqt = mqt, qti = qti)
}
