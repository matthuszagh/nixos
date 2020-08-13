self: super:

{
  symbiyosys = (super.symbiyosys.override {}).overrideAttrs (old: {
    propagatedBuildInputs = (if old ? propagatedBuildInputs then old.propagatedBuildInputs else []) ++ [
      super.yices
    ];
  });
}
