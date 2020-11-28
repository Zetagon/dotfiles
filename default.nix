{ config, pkgs, ... }:

{

  imports = [];
  programs.firejail.enable = true;
  services.openssh.forwardX11 = true;
  nixpkgs.config.allowUnfree = true;
  services.blueman.enable = true;
  programs.light.enable = true; # Control screen brightness
}
