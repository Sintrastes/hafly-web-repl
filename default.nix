{ system ? builtins.currentSystem
, obelisk ? import ./.obelisk/impl {
    inherit system;
    config.android_sdk.accept_license = true;
  }
}:
with obelisk;
project ./. ({ pkgs, ... }: {

  packages = {
    hafly = ./hafly;
    hafly-web-repl = ./.;
  };

  overrides = self: super: {
    zlib = self.callHackageDirect {
        pkg = "zlib";
        ver = "0.6.3.0";
        sha256 = "3PLCQ94ONQtjQc8AqVMgCVrZZW766T8PDevOvKC4VDw=";
      } {};
  };
})