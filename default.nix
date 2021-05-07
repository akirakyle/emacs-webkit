{ pkgs ? import <nixpkgs> {}
, trivialBuild ? pkgs.emacsPackages.trivialBuild
# user arguments
, packageSrc ? ./.
, packageVersion ? "git"
}:

trivialBuild rec {
  pname = "emacs-webkit";

  src = packageSrc;
  version = packageVersion;

  buildPhase = ''
    make all
    export LD_LIBRARY_PATH=$PWD:$LD_LIBRARY_PATH
  '';
  nativeBuildInputs = with pkgs; [ pkg-config wrapGAppsHook ];
  gstBuildInputs = with pkgs; with gst_all_1; [
    gstreamer gst-libav
    gst-plugins-base
    gst-plugins-good
    gst-plugins-bad
    gst-plugins-ugly
  ];
  buildInputs = with pkgs; [
    webkitgtk
    glib gtk3
    glib-networking
    gsettings-desktop-schemas
  ] ++ gstBuildInputs;

  GIO_EXTRA_MODULES = "${pkgs.glib-networking}/lib/gio/modules:${pkgs.dconf.lib}/lib/gio/modules";
  GST_PLUGIN_SYSTEM_PATH_1_0 = pkgs.lib.concatMapStringsSep ":" (p: "${p}/lib/gstreamer-1.0") gstBuildInputs;

  postInstall = ''
    cp *.so *.js *.css $out/share/emacs/site-lisp/
    mkdir $out/lib && cp *.so $out/lib/
  '';
}
