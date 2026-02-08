{
  description = "lf c library";

  outputs = { self, nixpkgs }: {
    overlay = final: prev: rec {
      # Just copies headers into nix store for easy #include
      lf = prev.stdenv.mkDerivation rec {
        name = "lf";
        src = ./.;
        dontBuild = true;
        allowSubstitutes = false;
        installPhase = '' mkdir -p $out/include && cp -r $src/* $out/include/'';
      };

      # Standard derivation template for building Single Translation Unit programs
      mkSTU = let 
        mk = {name, src
        , main ? name #default main src file is $name.cpp or $name.c
        , buildInputs ? []
        , flags ? ""
        , debug ? false
        , cpp ? false
        , executable ? true
        , installPhase ? ''
          runHook preInstall;
          install -Dt $out/bin ${name};
          runHook postInstall;
        ''
        , ...}@args:
          prev.stdenv.mkDerivation (args // {
            inherit name src installPhase;
            buildInputs = [ lf ] + buildInputs;
            doCheck = !debug;
            dontStrip = debug;
            allowSubstitutes = false;
            buildPhase = 
            ''${if cpp then "$CXX" else "$CC"} \
              ${if executable then "-o ${name}" else "-o {name}.o -c"} \
              ${src}/${main}.${if cpp then "cpp" else "c"} \
              ${flags} ${if debug then "-g -O0" else "-O2"}
            '';
          });
        in args: (mk args) // { debug = mk (args // { debug = true; }); };
    };
  };

}