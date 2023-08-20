#' SRK/T Formula for IOL Effective Lens Position
#' 
#' Calculate IOL effective lens position for emmetropia given axial length, 
#' corenal curvature, and IOL A-constant.
#' 
#' Note: A warning is provided if the combination of corneal curvature and axial length
#' produces an unexpected corneal height.
#' 
#' @param L axial length of the eye in millimeters (mm)
#' @param K corneal power (D)
#' @param ACD IOL anterior chamber depth constant (mm) 
#' @param A IOL A-constant (D) used to calculate equivalent ACD
#' @return Effective lens position of IOL (mm)
#' @seealso \code{\link{ELP}}
#' @family ELP
SRK.T.ELP <- function(L, K=337.5/R, R=337.5/K, ACD, A) {
  args <- list(L = L, K = K)
  if (missing(ACD) || ! is.finite(ACD)) {
    if (! missing(A) && all(is.finite(A))) {
      ACD <- 0.62467 * A - 68.747
      args$A <- A
      warning("Using SRK/T conversion from A constant to ACD: ", ACD, "mm")
    } else {
      stop("No ACD constant or A constant provided for SRK/T equation")
    }
  }
  # Corneal radius of curvature (mm)
  #R <- 337.5 / K
  # Corrected axial length (mm)
  LCOR <- L
  LCOR[L > 24.2] <- -3.446 + 1.715 * L[L > 24.2] - 0.0237 * L[L > 24.2] * L[L > 24.2]
  # Computed corneal width (mm)
Cw <- -5.41 + 0.58412 * L2 + 0.098 * K
# Corneal height (mm)
temp <- R * R - Cw * Cw / 4  

# Handle cases where temp is less than 0
negative_temp_indices <- which(temp < 0)
if (length(negative_temp_indices) > 0) {
  warning("Calculation of SRK/T corneal height poorly defined for this combination of corneal curvature and axial length in SRK/T")
  temp[negative_temp_indices] <- 0
  result[negative_temp_indices] <- NA
}

# Continue with calculations using temp
H <- R - sqrt(temp)
  # IOL specific offset
  Offset <- ACD - 3.336
  # Estimated ELP
  ELP <- H + Offset
  result <- ELP
  attr(result, 'parameters') <- args
  return(result)
}
ELP.functions$SRK.T <- SRK.T.ELP

#' SRK/T Formula for IOL Power
#' 
#' Calculate IOL power for emmetropia given axial length, 
#' corenal curvature, and effective lens position.
#' 
#' @param L axial length of the eye in millimeters (mm)
#' @param K corneal power (D)
#' @param ELP IOL effective lens position (mm)
#' @return Power of IOL (D)
#' @seealso \code{\link{Power}}
#' @family Power
SRK.T.Power <- function(L, K, ELP) {
  args <- list(L = L, K = K, ELP = ELP)
  na <- 1.336
  nc <- 1.333
  ncm1 <- 0.333
  R <- 337.5 / K
  RETHICK <- 0.65696 - 0.02029 * L
  LOPT <- L + RETHICK
  P <- 1000 * na * (na * R - ncm1 * LOPT) /
    ((LOPT - ELP) * (na * R - ncm1 * ELP))
  attr(P, 'parameters') <- args
  return(P)
}
Power.functions$SRK.T <- SRK.T.Power