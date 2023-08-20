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
Power <- function(L, K, A, ELP = NULL, Rx = 0, V = 13, which = 'all') {
  cl <- match.call()
  
  # Determine which equations to use
  if ('all' %in% which) {
    which <- names(Power.functions)
    which <- which[which != 'all']
  }
  if ('modern' %in% which) {
    which <- c(which, 'SRK.T', 'Holladay.1', 'Hoffer.Q', 'Haigis')
    which <- which[which != 'modern']
  }
  which <- unique(which)
  
  results_df <- data.frame(matrix(nrow=length(L), ncol=length(which)))
  colnames(results_df) <- which

  for (i in which) {
    if (is.null(Power.functions[[i]])) {
      warning("Unknown Power method requested: ", i, ".")
      next
    }
    
    # Check if ELP is missing and compute it if necessary
    if (is.null(ELP) || is.na(ELP[1])) {
      if (i %in% names(ELP.functions)) {
        ELP <- ELP(L = L, K = K, A = A, which = i)
      } else {
        warning(paste("ELP not provided and cannot be computed for method:", i))
        next
      }
    }
    
    args <- names(cl) %in% names(formals(Power.functions[[i]]))
    args_list <- as.list(cl)[args]
    
    temp_result <- numeric(length(L))
    for (j in seq_len(length(L))) {
      args_single <- lapply(args_list, function(arg) {
        if(length(arg) > 1) arg[j] else arg
      })
      temp_result[j] <- do.call(Power.functions[[i]], args_single)
    }
    
    results_df[[i]] <- temp_result
  }
  
  return(results_df)
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