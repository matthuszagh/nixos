self: super:

let custompkgs = import <custompkgs> { };
in
{
  verilog = (super.verilog.overrideAttrs (old: {
    buildInputs = with super; [ autoconf gperf flex bison ];
  }));
}
