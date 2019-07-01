with import <nixpkgs> { };

let jekyll_env = bundlerEnv {
    name = "lightandlight.github.io";
    ruby = ruby;
    gemfile = ./Gemfile;
    lockfile = ./Gemfile.lock;
    gemset = ./gemset.nix;
  };
in
  stdenv.mkDerivation {
    name = jekyll_env.name;
    buildInputs = [ jekyll_env ];
    shellHook = "exec ${jekyll_env}/bin/jekyll serve --watch --drafts --verbose";
  }
