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
        installPhase = '' mkdir -p $out/include && cp -r $src/lf.h $out/include/'';
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
        , ...}@args:
          prev.stdenv.mkDerivation ({
            inherit name src;
            buildInputs = [ lf ] ++ buildInputs;
            doCheck = !debug;
            dontStrip = debug;
            allowSubstitutes = false;
            dontPatch = true;
            dontUpdateAutotoolsGnuConfigScripts = true;
            dontConfigure = true;
            dontFixup = true;
            dontInstall = true;
            buildPhase = ''
              mkdir -p ${if executable then "$out/bin" else "$out/lib"}
              ${if cpp then "$CXX" else "$CC"} \
              ${if executable then "-o $out/bin/${name}" else "-o ${name}.o -c"} \
              ${src}/${main}.${if cpp then "cpp" else "c"} \
              ${flags} ${if debug then "-g -O0 -D_FORTIFY_SOURCE=0" else "-O2"}
              ${if !executable then "ar -rcs $out/lib/lib${name}.a ${name}.o" else ""}
            '';
          } // args);
        in args: (mk args) // { debug = mk ({ debug = true; } // args); };
    };
  };
}