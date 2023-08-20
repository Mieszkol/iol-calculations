#' Calculate IOL Power from Biometry Data and ELP
#' 
#' Using specified ocular biometry data including a measure of the eye's optical
#' length and corneal curvature, this function calculates one or more estimates 
#' of an intraocular lens's effective lens position (ELP). The ELP is the 
#' position in millimeters of the IOL's principle plane from the cornea's 
#' principle plane. The ELP used to be referred to as the anterior chamber depth
#' (ACD); however, ELP is a more accurate description.
#' 
#' @param L length of the eye in millimeters (mm)
#' @param K average corneal curvature of the eye in diopters (D)
#' @param A IOL A constant
#' @param ELP effective lens position in millimeters (mm)
#' @param Rx desired refractive outcome in diopters (D), defaults to emmetropia
#' @param V vertex of refractive outcome in millimeters (mm), defaults to 13 mm
#' @param which string vector specifying which IOL power formulas to use
#' @return Named numeric vector of optimal IOL powers (in diopters, D) for each 
#'   formula requested
#' @export
#' @seealso \code{\link{ELP}}
#' @family Power
#' @author Eric N. Brown \email{eric.n.brown@@gmail.com}
Power <- function(L, K, A, ELP = NULL, Rx = 0, V = 13, which = "SRK/T", ...) {
  which <- match.arg(which)
  P <- switch(which,
              "SRK/T" = {
                ELP <- if (is.null(ELP)) SRK.T.ELP(L, K, A = A, ...) else ELP
                SRK.T.Power(L, K, ELP)
              },
              "Holladay 1" = {
                ELP <- if (is.null(ELP)) Holladay.1.ELP(L, K, A = A, ...) else ELP
                Holladay.1.Power(L, K, ELP, Rx = Rx, V = V)
              },
              "Hoffer Q" = {
                ELP <- if (is.null(ELP)) Hoffer.Q.ELP(L, K, A = A, ...) else ELP
                Hoffer.Q.Power(L, K, ELP, Rx = Rx, V = V)
              },
              "SRK" = {
                SRK.Power(L, K, A, ELP)
              },
              "Haigis" = {
                # Haigis formula was not provided, so it is omitted here.
                stop("Haigis formula was not provided.")
              }
  )
  return(P)
}



#' @export
print.Power <- function(x, ...) {
  # Remove all attributes other than names
  n <- names(x)
  attributes(x) <- NULL
  names(x) <- n
  # Print the object
  print(unclass(x))
}