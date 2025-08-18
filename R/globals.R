# Global variables to avoid R CMD check warnings

# data.table non-standard evaluation variables
globalVariables(c(
  # Common data.table variables
  "cf", "inizio", "fine", "arco", "id", "prior", "durata", "stato", 
  "INIZIO", "FINE", "cdata", "value", "acf",
  
  # analyze_employment_transitions specific variables
  "transition_duration", "from", "to", "weight", "variable",
  
  # Other common variables from various functions
  ".N", ".SD", "..required_standard", "..extra_cols",
  
  # Employment status variables
  "ft_contracts", "pt_contracts", "employment_rate", "has_overlap",
  "has_unemployment", "n_segments", "n_statuses",
  
  # Merge functions variables
  "status_change", "group_id", "prev_fine", "has_gap", "subgroup_id",
  "is_consecutive", "is_employment", "employment_group", "final_group",
  "employment_status",
  
  # add_external_events specific variables
  "event_name", "event_start", "event_end", "row_id", "overlap_days",
  "temp_distance"
))