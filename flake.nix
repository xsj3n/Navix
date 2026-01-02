{
  description = "Haskell dev shell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-cabal31420.url = "github:nixos/nixpkgs/f4b140d5b253f5e2a1ff4e5506edbf8267724bde";
    };

  outputs = { self , nixpkgs, nixpkgs-cabal31420, ... }:
  let
    name = "Navix"; 
    system = "x86_64-linux";
    src = ./.;
    pkgs = import nixpkgs { inherit system; };
    hpkgs = pkgs.haskellPackages;
    c31420 = import nixpkgs-cabal31420 { inherit system; };
    haskell_tools = with hpkgs; [
      haskell-language-server
      c31420.cabal-install
    ];
    tools = with pkgs; [
      zlib
      openssl
      postgresql
      haskell.compiler.native-bignum.ghcHEAD
      pkg-config
    ];

    shellPackages = with pkgs; [
      hpkgs.hoogle
    ];
        
    nativeBuildInputs = haskell_tools ++ tools ++ shellPackages;
    nonFhsShell = pkgs.mkShell
    {
      inherit  name nativeBuildInputs;
      shellHook = ''
      if [ ! -d ./database ]; then
        initdb -D ./database
       fi

      rm /tmp/.s.PG* &> /dev/null # there is a lock which may need to be removed
      
      # Set PGDATA or pass -D to specify DB directory
      # Set PGHOST or pass -h to specify socket directory 
      # -o specifices additional cmdline flags, with the -k specifying where to put unix socket file
      # -l specifies logfile path | start spins up the server in the backgroud
      export PGDATA=./database
      export PGHOST=/tmp
      pg_ctl -o "-k /tmp" -l logfile start 

      export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${pkgs.lib.makeLibraryPath nativeBuildInputs }
      '';
      LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath nativeBuildInputs;
    };
    fhsShell = pkgs.buildFHSEnv
    {
      inherit name;
      targetPkgs = pkgs: nativeBuildInputs;
      profile = ''
      export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${pkgs.lib.makeLibraryPath nativeBuildInputs }
      '';
      runScript = "bash";
    };
  in
  {
     devShells."${system}".default = nonFhsShell;
     packages."${system}".default = hpkgs.callCabal2nix "${name}" src { };
  };
}
