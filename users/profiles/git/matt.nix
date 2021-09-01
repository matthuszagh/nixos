{ ... }:

{
  programs.git = {
    userName = "Matt Huszagh";
    userEmail = "huszaghmatt@gmail.com";
    extraConfig = {
      github.user = "matthuszagh";
      gitlab.user = "matthuszagh";
      # adds `--prune` to every `git fetch` and `git pull`
      fetch.prune = true;
    };
    lfs.enable = true;

    # TODO configure signing. this is someone elses, just to get an idea.
    # signing = {
    #   signByDefault = true;
    #   key = "9CBF84633C7DDB10";
    # };
  };
}
