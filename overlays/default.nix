final: prev: {
  ol = {
    mathematica = prev.callPackage ./mathematica { };
    seafile-shared = prev.callPackage ./seafile-shared { };
    discord = prev.callPackage ./discord { };
  };
}
