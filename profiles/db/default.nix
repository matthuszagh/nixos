{ pkgs
, ...
}:

{
  imports = [
    ./postgres
  ];

  environment.systemPackages = with pkgs; [
    # needed for edbi
    perlPackages.RPCEPCService
    perlPackages.DBI
    perlPackages.DBDPg
    perlPackages.LaTeXML

    # required by org-roam
    sqlite
  ];
}
