{ config, pkgs, ... }:
let
  custompkgs = import <custompkgs> { };

  emacsEnv = (pkgs.emacsPackagesGen ((pkgs.emacsGcc.override {
    withGTK3 = true;
    withXwidgets = true;
    withX = true;
    srcRepo = true;
    withCsrc = true;
  }).overrideAttrs (old: {
    CFLAGS = "-O3 -march=native -momit-leaf-frame-pointer";
  }))).emacsWithPackages (epkgs: (with epkgs.elpaPackages; [
    aggressive-indent
    auctex
    exwm
    debbugs
    org-edna
    undo-tree
  ]) ++ (with epkgs.melpaPackages; [
    alert
    auctex-latexmk
    company
    # faster, more predictable completion searching
    prescient
    company-prescient
    clang-format
    flycheck-clang-analyzer
    emms
    pulseaudio-control
    super-save
    # jump out of parentheses and quotes with tab. works with
    tab-jump-out

    # programming
    cmake-mode
    cmake-font-lock
    elpy
    py-autopep8
    blacken
    yapfify
    sage-shell-mode
    ob-sagemath
    python-info
    python-docstring
    rust-mode
    # a compiler explorer-like implementation in Emacs.
    rmsbolt
    # automatically compile outdated elisp files
    auto-compile
    # automatically create banner comments
    banner-comment
    # terminal integration
    vterm
    # 256 colors in terminals
    # TODO fix
    # eterm-256color
    # switch between current buffer and vterm
    vterm-toggle
    # cython support
    cython-mode
    flycheck-cython
    # colors in info-mode
    info-colors
    hydra
    perspective
    haskell-mode
    json-mode
    lsp-python-ms # microsoft python language server
    nixpkgs-fmt

    # html colors in org export
    htmlize

    # vim-like integration
    evil
    evil-collection
    evil-surround
    evil-magit
    evil-numbers # increment and decrement numbers at point

    cask
    cask-mode
    flycheck-cask
    emr
    elsa
    flycheck
    flycheck-elsa
    wgrep
    dumb-jump
    realgud

    # Database viewer
    edbi

    paradox
    general
    git-gutter
    git-timemachine
    github-notifier
    helm
    helm-descbinds
    helm-exwm
    helm-eww
    helm-xref
    helm-org-rifle
    helm-projectile
    helm-recoll
    helm-systemd
    helm-rg
    helm-org
    # helm-ls-git
    helpful
    ht
    sx
    elfeed

    # fontification in spice buffers
    spice-mode

    notmuch
    helm-notmuch
    ledger-mode
    flycheck-ledger
    evil-ledger

    # windows
    popwin

    # shell
    bash-completion
    fish-completion

    # lsp
    lsp-mode
    lsp-ui
    company-lsp
    ccls
    # debug adapter protocol
    dap-mode

    # writing
    writegood-mode
    define-word
    langtool
    ## perform helm copies/renames asynchronously
    async
    ## execute org source blocks asynchronously
    ob-async
    # convert pcre regexes to emacs syntax
    pcre2el

    magit
    forge # integrates magit with github, gitlab, etc.
    markdown-mode
    multiple-cursors
    nix-mode
    nix-update
    direnv
    no-littering
    nov
    org-board
    org-drill
    org-edit-latex
    # org-ql
    # TODO wrap with org-ql
    ts
    dash
    dash-functional
    f
    s
    org-super-agenda
    # TODO fix
    # peg
    ov
    # TODO end
    # poly-org
    # polymode
    projectile
    rainbow-delimiters
    scad-mode
    slime
    slime-company
    smart-mode-line
    spaceline
    sourcerer-theme
    naysayer-theme
    symon
    transient
    use-package
    which-key
    x86-lookup
    # yasnippet
    # yasnippet-snippets
  ]) ++ (with epkgs.orgPackages; [
  ]) ++ (with epkgs; [
    pdf-tools
  ]) ++ (with custompkgs; [
    org-mode
    org-recoll
    layers
    justify-kp
    org-db
    org-ql
    yasnippet
    helm-ls-git
  ]));
in
{
  # TODO fix (subdirs issue)
  # home-manager.users.matt = { ... }: {
  #   programs.emacs = {
  #     enable = true;
  #     package = emacsEnv;
  #   };
  #   services.emacs.enable = true;
  # };

  environment.systemPackages = with pkgs; [
    emacsEnv

    # C / C++
    gdb
    lldb
    clang-tools
    clang-analyzer
    bear
    cppcheck
    # Python
    # python3Packages.python-language-server
    python3Packages.black
    python3
    # hdl
    verilator
    # bash
    # tex/latex
    # fortran
    # rust
    rustc
    rls
    rustfmt
    cargo
    # tex
    # needed for debug adapter protocol
    nodejs
    nixpkgs-fmt

    ## language servers
    lua53Packages.digestif
    shellcheck
    nodePackages.bash-language-server
    nodePackages.typescript-language-server
    nodePackages.typescript
    python-language-server
    # TODO add fortran-language-server (github: hansec/fortran-language-server) to nixpkgs

    # search
    ripgrep
    recoll

    # math / science
    # circuit simulation
    ngspice

    # utilities
    imagemagick
    ispell
    ghostscript
    languagetool
    gimp

    # needed for edbi
    # perl-with-packages
    perlPackages.RPCEPCService
    perlPackages.DBI
    perlPackages.DBDPg
    perlPackages.LaTeXML

    # GUI
    #
    # Graphical applications are bundled with Emacs because I use
    # Emacs as my window manager. If a new graphical environment
    # is setup, such as a desktop manager, these programs can be
    # copied there as well.
    gsettings-desktop-schemas # needed with next
    # next-gtk-webkit
    anki
    vlc
  ] ++ (with custompkgs; [
    sbcl
    brainworkshop
    gnucap
  ]);
}
