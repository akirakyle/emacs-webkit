{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    buildInputs =  with pkgs; [
      gcc pkg-config gtk3 webkitgtk glib-networking wrapGAppsHook
      gdb
    ];

    GIO_EXTRA_MODULES="${pkgs.glib-networking}/lib/gio/modules:${pkgs.dconf.lib}/lib/gio/modules";
}
