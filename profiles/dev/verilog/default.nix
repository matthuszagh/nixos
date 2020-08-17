{ pkgs }:

{
  environment.systemPackages = with pkgs; [
    verilator
  ]
}
