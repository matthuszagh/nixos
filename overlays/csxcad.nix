final: prev:

{
  # csxcad = (prev.csxcad.override { }).overrideAttrs (oldAttrs: {
  #   src = prev.fetchFromGitHub {
  #     owner = "matthuszagh";
  #     repo = "CSXCAD";
  #     rev = "8ee68d4ce44125f7ba38ad7f52c74b2572798dc4";
  #     sha256 = "0dwz34a6m682jv8gx51jlpbql9s0rpj2b58yvlx8m7zy1xr4hcdf";
  #   };
  # });

  csxcad = prev.csxcad;
}
