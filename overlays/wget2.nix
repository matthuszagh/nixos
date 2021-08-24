final: prev:

{
  wget2 = (prev.wget2.override { }).overrideAttrs (oldAttrs: {
    src = prev.fetchFromGitLab {
      owner = "gnuwget";
      repo = "wget2";
      rev = "7c7bbf2c2752f1038f10fb298330fe7c93811030";
      sha256 = "sha256-dqtpnyy93Y43y+a6Y9nfnhb9x6H0IQ98/I/8kpTsgpc=";
    };

    patches = [ ];
  });
}
