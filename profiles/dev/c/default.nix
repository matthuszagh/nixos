{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    gdb
    lldb
    clang-tools
    clang-analyzer
    bear
    cppcheck
  ];
}
