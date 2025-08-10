#' Minimal High-Performance Employment Transformation Core
#'
#' @description 
#' This is the absolute fastest implementation for transforming employment records
#' into temporal segments. Contains only the essential event-based logic with minimal
#' overhead. Use this for production workloads requiring maximum performance.
#'
#' @param dt A data.table containing employment contract records with required columns:
#'   \itemize{
#'     \item{\code{cf}}: Person identifier
#'     \item{\code{inizio}}: Contract start date (Date or numeric)
#'     \item{\code{fine}}: Contract end date (Date or numeric) 
#'     \item{\code{prior}}: Employment type (0/negative = part-time, positive = full-time)
#'     \item{\code{id}}: Contract identifier
#'   }
#'
#' @return A data.table with temporal segments containing:
#'   \itemize{
#'     \item{\code{cf}}: Person identifier
#'     \item{\code{inizio}}: Segment start date
#'     \item{\code{fine}}: Segment end date
#'     \item{\code{arco}}: Number of overlapping contracts (0 = unemployment)
#'     \item{\code{prior}}: Employment type for the segment
#'     \item{\code{id}}: Contract ID (0 for unemployment periods)
#'     \item{\code{durata}}: Duration of the segment
#'     \item{\code{stato}}: Employment status classification
#'   }
#'
#' @details
#' This core function implements the fastest possible event-based temporal transformation.
#' It assumes input data is already validated and uses standardized column names.
#' No error checking or validation is performed for maximum speed.
#' 
#' The algorithm:
#' 1. Creates start events (value=1) and end events (value=-1, date=fine+1)
#' 2. Sorts events by person and date
#' 3. Uses cumulative sum to track overlapping contracts (arco)
#' 4. Creates segments between consecutive events
#' 5. Classifies employment states based on arco and prior values
#'
#' @export
#' @importFrom data.table data.table setorder rbindlist fcase shift
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' dt <- data.table(
#'   cf = c("A001", "A001", "B002"),
#'   inizio = as.Date(c("2023-01-01", "2023-06-01", "2023-02-01")),
#'   fine = as.Date(c("2023-05-31", "2023-12-31", "2023-11-30")),
#'   prior = c(1, 0, 1),
#'   id = 1:3
#' )
#' result <- vecshift_core(dt)
#' }
vecshift_core <- function(dt) {
  
  setorder(
    rbindlist(
      list(
        dt[, .(id, cf, cdata = inizio, value = 1, prior)]
      , dt[, .(id, cf, cdata = fine + 1, value = -1, prior = 0)]
      )
    )
  , cf, cdata)[
    , arco := cumsum(value)][
      , prior := fcase(prior <= 0, 0, default = 1)
    ][
      , .(
        cf = cf[1:(length(cf)-1)]
      , acf = cf[2:(length(cf))]
      , inizio = cdata[1:(length(cf)-1)]
      , fine = cdata[2:(length(cf))]
      , arco = arco[1:(length(cf)-1)]
      , prior = prior[1:(length(cf)-1)]
      , id = id[1:(length(cf)-1)]
      )
    ][cf == acf][, acf := NULL][arco == 0, id := 0][
      , durata := fcase(arco >= 1, fine - inizio, default = fine - inizio - 1)
    ][
      , stato := fcase(
        arco == 0 & durata <= 8, "disoccupato",
        arco == 0 & durata > 8, "disoccupato", 
        arco == 1 & prior == 1, "occ_ft",
        arco == 1 & prior == 0, "occ_pt",
        arco > 1 & (prior > shift(prior, type = "lag")), "over_pt_ft",
        arco > 1 & (prior < shift(prior, type = "lag")), "over_ft_pt", 
        arco > 1 & (prior == shift(prior, type = "lag")) & prior == 0, "over_pt_pt",
        default = "over_ft_ft"
      )
    ][durata > 0]
}