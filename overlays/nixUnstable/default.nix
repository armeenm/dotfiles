{ pkgs, ... }:

pkgs.nixUnstable.override {
  patches = [ ./unset-is-macho.patch ];
}
