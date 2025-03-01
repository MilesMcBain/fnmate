# 0.2.0

- Help users avoid accidentally generating unwanted definition
  - `{fnmate}` will hesitate to generate a defintion for a function that is available in the global evironment, either loaded via a package, or defined locally.
  - option `fnmate_banned_names` is provided to allow configuration of a list of function names that can never have definitions generated. A classic example you may want to configure would be `tar_target()`.
- Turn on/off roxygen template generation with option `fnmate_generate_roxygen`.
- Improve handling of jumps where multiiple matching definitions are available (often a side effect of unwated definition generation)
  - Users are warned if multiple defintions are found. All paths are reported, as is the final selected jump target.
  - When deciding where to jump preference is given to paths that are decendent from the path of the jump source.
  - A glob can be configured to match to paths that should be preferred in this situation in option `fnmate_preferred_jump_paths`. E.g. `R/*.R` for R sources in the common location.
- An error no longer occurs if the project is not yet a git repository. This happened because the git `user.name` is used for the Author name in the Roxygen template. A placeholder is used.

