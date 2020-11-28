# ~/.config/nixpkgs/home.nix
{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  imports = [
  ];

  home.packages = [
    pkgs.ripgrep
    pkgs.transmission
    pkgs.xorg.fontmiscmisc
    pkgs.haskellPackages.xmobar
    pkgs.htop
    pkgs.racket
    pkgs.killall
    pkgs.sqlite
    pkgs.syncthing
    pkgs.gparted
    pkgs.x2x
    pkgs.python3
    pkgs.keepassx2
    pkgs.gnumake
    pkgs.tmux
    pkgs.rofi	
    pkgs.xbindkeys
    pkgs.xcape
    pkgs.xorg.xmodmap
    pkgs.texlive.combined.scheme-medium
    pkgs.mu
    pkgs.isync
    pkgs.gimp
    pkgs.mpv
    pkgs.vim
  ];

  # home.file = {
    # ".xbindkeysrc".source = ./xbindkeysrc;
    # ".doom.d/config.org".source = ./doom/config.org;
    # ".config" = {
      # source = ./config;
      # recursive = true;
    # };
    # ".xmonad" = {
      # source = ./xmonad;
      # recursive = true;
    # };
    # ".xmobarrc".source = ./xmobar;
  # };



  xsession.profileExtra = ''
                        setxkbmap -option "caps:ctrl_modifier"
                        setxkbmap -layout us,se -option grp:shifts_toggle

                        # Make tab a superkey when held down, tab when pressed alone
                        xmodmap -e "keysym Tab = Hyper_L"
                        xmodmap -e "remove mod4 = Hyper_L"
                        xmodmap -e "keycode any = Tab"
                        xcape -e "Hyper_L=Tab"
                        xcape -e "Caps_Lock=Escape"
                        '';
}
