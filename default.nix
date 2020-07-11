{ config, pkgs, ... }:

{

  imports = [];
  programs.firejail.enable = true;
  services.openssh.forwardX11 = true;
  nixpkgs.config.allowUnfree = true;
}
