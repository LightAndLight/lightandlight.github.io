---
layout: post
title: Configuring Agda's standard library on NixOS
date: 2019-05-18 09:49:00+1000
author: ielliott95
permalink: /agda-nixos/
tags:
    - programming
---

I've been using Agda on NixOS for a while (mostly via `agda-mode` in Emacs), but I remember it was a bit
difficult to get going the very first time. Hopefully this becomes a searchable reference
to getting it all set up quickly.

---

Prerequisites:

* Working NixOS installation
* Agda >=2.5.1

1. Install `AgdaStdlib` globally

   ``` nix
   # /etc/nixos/configuration.nix

   ...
   
   environment.systemPackages = with pkgs; [
   
     ...
   
     AgdaStdLib
    
   ];
   
   ...
   ```
   
2. Link `/share/agda`

   ``` nix
   # /etc/nixos/configuration.nix
   
   ...
   
   environment.pathsToLink = [ "/share/agda" ];
   
   ...
   ```
  
3. Rebuild: `sudo nixos-rebuild switch`
   
4. Navigate to or create `~/.agda`

5. Create 3 files in `~/.agda`: `defaults`, `libraries`, `standard-library.agda-lib`

   ``` shell_session
   [isaac:~/.agda]$ touch defaults
   [isaac:~/.agda]$ touch libraries
   [isaac:~/.agda]$ touch standard-library.agda-lib
   ```
   
6. Edit `standard-library.agda-lib`

   ``` shell_session
   [isaac:~/.agda]$ cat << EOF >> standard-library.agda-library
   > name: standard-library
   > include: /run/current-system/sw/share/agda/
   > EOF
   ```
   
   This says that there is a library located at the relevant NixOS path.
   
7. Edit `libraries`

   ``` shell_session
   [isaac:~/.agda]$ echo "/home/isaac/.agda/standard-library.agda-lib" >> libraries
   ```
   
   This registers the `.agda-lib` file with Agda.
   
8. Edit `defaults`
   
   ``` shell_session
   [isaac:~/.agda]$ echo "standard-library" >> defaults
   ```
   
   This tells Agda to include the `standard-library` library by default.
   
To check your installation, try compiling a simple Agda file:

``` shell_session
[isaac:~]$ cat << EOF >> Test.agda
> module Test where
> open import Data.Nat
> EOF
[isaac:~]$ agda Test.agda
Checking Test (/home/isaac/Test.agda).

```

Let me know whether or not this works for you :)
