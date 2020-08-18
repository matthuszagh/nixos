self: super:

{
  emacs = (super.pkgs.emacsUnstable.override {
    withGTK3 = true;
    withXwidgets = true;
    withX = true;
    srcRepo = true;
    withCsrc = true;
  }).overrideAttrs (old: {
    CFLAGS = "-O3 -march=native -momit-leaf-frame-pointer";
  });

  # emacsPackagesFor = emacs: (
  #   (super.emacsPackagesFor emacs).overrideScope'(eself: esuper: let

  #     orgPackages.org = (import ./org-mode.nix {
  #       pkgs = super.pkgs;
  #       emacs = emacs;
  #     });
  #     # orgPackages = esuper.orgPackages.override {
  #     #   generated = ./org-mode.nix;
  #     # };

  #     epkgs = esuper.override {
  #       inherit orgPackages;
  #     };
  #   in epkgs
  #   )
  # );
}
