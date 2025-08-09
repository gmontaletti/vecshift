#' Transform Employment Records into Temporal Segments
#'
#' @description 
#' Processes employment contract records with temporal boundaries to create 
#' continuous temporal segments that track employment status over time. The function
#' identifies unemployment periods (when no contracts are active), single employment
#' periods, and overlapping employment situations.
#'
#' @param dt A data.table containing employment contract records with the following required columns:
#'   \itemize{
#'     \item{\code{id}}: Contract identifier (unique key for each employment contract)
#'     \item{\code{cf}}: Person identifier (e.g., fiscal code)
#'     \item{\code{INIZIO}}: Contract start date (Date or numeric)
#'     \item{\code{FINE}}: Contract end date (Date or numeric)
#'     \item{\code{prior}}: Employment type indicator (0 or negative for part-time, positive for full-time)
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
#'     \item{\code{stato}}: Employment status classification:
#'       \itemize{
#'         \item{\code{disoccupato}}: Unemployment period (arco = 0)
#'         \item{\code{occ_ft}}: Single full-time employment
#'         \item{\code{occ_pt}}: Single part-time employment
#'         \item{\code{over_pt_ft}}: Overlapping employment, part-time to full-time
#'         \item{\code{over_ft_pt}}: Overlapping employment, full-time to part-time
#'         \item{\code{over_pt_pt}}: Overlapping part-time employments
#'         \item{\code{over_ft_ft}}: Overlapping full-time employments
#'       }
#'   }
#'
#' @export
#' @importFrom data.table data.table setorder rbindlist fcase shift
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' # Create sample employment data
#' dt <- data.table(
#'   id = 1:3,
#'   cf = c("ABC123", "ABC123", "DEF456"),
#'   INIZIO = as.Date(c("2023-01-01", "2023-06-01", "2023-02-01")),
#'   FINE = as.Date(c("2023-05-31", "2023-12-31", "2023-11-30")),
#'   prior = c(1, 0, 1)  # 1 = full-time, 0 = part-time
#' )
#' 
#' # Transform to temporal segments
#' result <- vecshift(dt)
#' print(result)
#' }
vecshift <- function(dt) {
  # Load required package
  require("data.table")
  
  # Input validation
  if (!inherits(dt, "data.table")) {
    stop("Input 'dt' must be a data.table object. Use as.data.table() to convert.")
  }
  
  # Check for required columns
  required_cols <- c("id", "cf", "INIZIO", "FINE", "prior")
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Validate column types
  if (!is.numeric(dt$INIZIO) && !inherits(dt$INIZIO, "Date")) {
    stop("Column 'INIZIO' must be numeric or Date type")
  }
  if (!is.numeric(dt$FINE) && !inherits(dt$FINE, "Date")) {
    stop("Column 'FINE' must be numeric or Date type")
  }
  if (!is.numeric(dt$prior)) {
    stop("Column 'prior' must be numeric")
  }
  
  # Check for logical consistency
  if (any(dt$FINE < dt$INIZIO, na.rm = TRUE)) {
    warning("Some records have FINE < INIZIO. These may produce unexpected results.")
  }
  
  # Main processing logic (unchanged)
  setorder(
    rbindlist(
      list(
        dt[, .(id, cf, cdata = INIZIO, value = 1
               , prior
               )]
      , dt[, .(id, cf, cdata = FINE + 1, value = -1
               , prior = 0
               )]
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
    # , prio_dop = prior[2:(length(cf))]


        # , clsd = arco[which(.I %% 2 == 0)]
    , id = id[1:(length(cf)-1)]
    # , id_aft = id[2:(length(cf))]
    # , p_bef = prior[which(.I %% 2 != 0)]
    # , p_aft = prior[which(.I %% 2 == 0)]
    )][cf == acf][, acf := NULL][arco == 0, id := 0][, durata := fcase(arco >= 1, fine-inizio,
  default =  -1+fine-inizio)
  ][, stato := fcase(arco == 0 & durata <= 8, "disoccupato",
                     arco == 0 & durata > 8, "disoccupato",
                     arco == 1 & prior == 1, "occ_ft",
                     arco == 1 & prior == 0, "occ_pt",
                     arco > 1  & (prior > shift(prior, type = "lag")), "over_pt_ft",
                     arco > 1  & (prior < shift(prior, type = "lag")), "over_ft_pt",
                     arco > 1  & (prior == shift(prior, type = "lag")) & prior == 0, "over_pt_pt",
                     default = "over_ft_ft"
  )][durata > 0]
  
  #[inizio != fine]
  
  
}


