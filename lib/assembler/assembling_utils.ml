
type assembling_options = {
  opt_verbose : bool; (** Verbose mode *)
  opt_strict  : bool; (** Strict mode *)
  opt_as8     : bool; (** Allows as8 special directives *)
  opt_format  : bool; (** Reformat input file *)
  opt_interm  : bool; (** Keep intermediate file *)
  opt_input   : string; (** Input file *)
  opt_output  : string; (** Input file *)
}