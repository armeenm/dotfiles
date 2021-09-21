{ lib, ... }:
rec {
  toAttrPath = base: rest:
    if lib.isString rest
    then toAttrPath (lib.splitString "." rest)
    else if lib.isList rest
    then builtins.foldl' (lib.flip builtins.getAttr) base rest
    else null;
}
