{ pkgs }:

{
  environment.systemPackages = with pkgs; [
    verilator
    python3Packages.hdl-checker
  ];
}
