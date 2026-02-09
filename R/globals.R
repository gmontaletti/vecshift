# Global variables to avoid R CMD check warnings

# data.table non-standard evaluation variables
globalVariables(c(
  # Common data.table variables
  "cf",
  "inizio",
  "fine",
  "arco",
  "id",
  "prior",
  "durata",
  "stato",
  "cdata",
  "value",
  "acf",

  # analyze_employment_transitions specific variables
  "transition_duration",
  "from",
  "to",
  "weight",
  "variable",

  # Other common variables from various functions
  ".N",
  ".SD",
  "..required_standard",
  "..extra_cols",

  # Employment status variables
  "ft_contracts",
  "pt_contracts",
  "employment_rate",
  "has_overlap",
  "has_unemployment",
  "n_segments",
  "n_statuses",

  # vecshift core variables
  "cf_int",
  "over_id",
  "first_in_over",

  # Merge functions variables
  "status_change",
  "group_id",
  "prev_fine",
  "has_gap",
  "subgroup_id",
  "is_consecutive",
  "is_employment",
  "employment_group",
  "final_group",
  "employment_status",
  "record_durata",
  "collapsed",
  "n_periods",
  "consolidation_group",
  "temp_group",

  # add_external_events specific variables
  "event_name",
  "event_start",
  "event_end",
  "row_id",
  "overlap_days",
  "temp_distance",

  # data.table operators and special symbols
  ":=",
  ".",
  ".I",
  "N",

  # Internal helper function variables
  "prior_max",
  "prior_min",
  "..consolidated_cols",
  ".internal_idx",
  ".row_id",
  "midpoint",
  "distance",
  "i.midpoint",
  "first_inizio",
  "earliest_event",
  "first_contract",
  "row_num",
  "x.sequence_label",

  # validate functions variables
  "duration_matches",
  "elapsed_time",
  "total_duration",
  "discrepancy",
  "over_id_gap",
  "max_over_id",
  "unique_over_ids"
))
