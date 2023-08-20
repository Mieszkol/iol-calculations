#' Calculate Effective Lens Position (ELP) from Biometry and IOL Data
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
#' @param R average corneal radius in millimeters (mm)
#' @param cornea_n effective corneal index of refraction
#' @param ACD ultrasound anterior chamber depth (mm)
#' @param A IOL A constant (D)
#' @param pACD IOL pACD constant (mm)
#' @param S IOL surgeon factor constant
#' @param a0 Haigis formula a0 lens constant (mm)
#' @param a1 Haigis formula a1 lens constant
#' @param a2 Haigis formula a2 lens constant
#' @param which string vector specifying which ELP formulas to use
#' @return Named numeric vector of effective lens position (in mm) for each ELP 
#'   formula requested.
#' @export
#' @seealso \code{\link{Power}}
#' @family ELP
#' @note If the some of the IOL constants A, pACD, or S are not provided, it may
#'   be derived from those given. A warning is generally produced when this 
#'   conversion is performed.
#'   
#'   The returned numeric vector is augmented with a \code{'parameters'} list 
#'   attribute describing which biometry and IOL parameters were used to 
#'   calculate each value. For example, if the Hoffer Q ELP is calculated given 
#'   an axial length, corneal power (via the \code{K} parameter), and an 
#'   A-constant (via the \code{A} parameter), then the \code{'parameters'} list 
#'   attribute will have a list named \code{Hoffer.Q} with three values: 
#'   \code{L}, \code{K}, and \code{A}.
#' @author Eric N. Brown \email{eric.n.brown@@gmail.com}
#' @examples
#' # Get the effective lens position of a normal eye with for the
#' # Alcon SA60AT lens using the Hoffer Q formula. This will compute
#' # the required pACD IOL constant from the provided A constant.
#' (elp <- ELP(L = 24, K = 44, A = 118.4, which = 'Hoffer.Q'))
#'  
#' # Check which parameters were used to calculate the ELP
#' attr(elp, 'parameters')$Hoffer.Q
#' 
#' # Get the ELP of a normal eye with modern formulas (Hoffer Q, SRK/T,
#' # and Holladay 1). Five warnings will be output for the conversion of
#' # the A constant to ACD for the SRK/T and Hoffer Q formulas, using a
#' # standard corneal index of refraction (since one wasn't provided) to
#' # convert corneal power to radius of curvature, and for approximating
#' # the Holladay 1 surgeon factor from the provided A constant.
#' (elp <- ELP(L = 24, K = 44, A = 118.4, which = 'modern'))
#' 
#' # Get the ELP of a normal eye with the Holladay 1 formula. Although
#' # both the IOL A constant and surgeon factor are provided, since the
#' # formula requires the surgeon factor, the A constant will be ignored
#' (elp <- ELP(L = 24, K = 44, A = 118.4, S = 1.45, which = 'Holladay.1'))
#' attr(elp, 'parameters')$Holladay.1
ELP <- function(L, K, A, ELP = NULL, Rx = 0, V = 13, which = "SRK/T", ...) {
  which <- match.arg(which)
  
  # This is a placeholder for the ELP.functions list, which is assumed to 
  # contain the vectorized implementations of the various ELP formulae.
  # The actual content of this list should match the original code's list.
ELP.functions <- list(
  "SRK/T" = SRK.T.ELP,
  "Holladay 1" = Holladay.1.ELP,
  "Hoffer Q" = Hoffer.Q.ELP
)
 # Placeholder
  
  result <- list()
  
  for (i in which) {
    if (!i %in% names(ELP.functions)) {
      warning("Unknown ELP method requested: ", i, ".")
      next
    }
    args <- names(match.call()) %in% names(formals(ELP.functions[[i]]))
    args <- as.list(match.call())[args]
    result[[i]] <- do.call(ELP.functions[[i]], args)
  }
  
  # Remember the names of the ELP functions
  functions <- names(result)
  function.arguments <- vector(mode = 'character')
  ELP_result <- vector(mode = 'numeric')
  
  for (i in functions) {
    ELP_result[[i]] <- result[[i]]
    attr(ELP_result,'parameters')[[i]] <- attr(result[[i]],'parameters')
    function.arguments[[i]] <- paste(names(attr(result[[i]],'parameters')), collapse=', ')
  }
  names(function.arguments) <- NULL
  attr(ELP_result, 'call') <- match.call()
  attr(ELP_result, 'function') <- functions
  attr(ELP_result, 'function.arguments') <- function.arguments
  class(ELP_result) <- 'ELP'
  
  return(ELP_result)
}


#' @export
print.ELP <- function(x, ...) {
  # Remove all attributes other than names
  n <- names(x)
  attributes(x) <- NULL
  names(x) <- n
  # Print the object
  print(unclass(x))
}