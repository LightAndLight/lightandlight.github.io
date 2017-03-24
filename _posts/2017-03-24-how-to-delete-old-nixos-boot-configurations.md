---
layout: post
permalink: /how-to-delete-old-nixos-boot-configurations/
title: How to delete old NixOS boot configurations
---

[NixOS](https://nixos.org/) is a Linux distribution with declarative
configuration management. Your system configuration is specified in a
set of files, and can run a command to update your system to the current
specification. A consequence of this is that your entire system
configuration can be versioned.

Every time you rebuild your NixOS configuration, a new entry is added to
the bootloader. This is helpful if you ever make a configuration change
that breaks on your machine because you can reboot into the last known
working state and try something different.

If you don't need to have access to all your old configurations, you
can delete them:

1. Delete the old (exludes the current) package configurations for the
   NixOS system `sudo nix-env -p /nix/var/nix/profiles/system
   --delete-generations old`
 
2. Collect garbage `nix-collect-garbage -d`

3. View the remaining generation `nix-env -p /nix/var/nix/profiles/system
   --list-generations`. Take note of this for the next step.

4. Remove unnecessary boot loader entries. I use `systemd-boot`, so all
   my entries are located in `/boot/loader/entries`. To remove all the
   old entries, run `sudo bash -c "cd /boot/loader/entries; ls | grep
   -v <current-generation-name> | xargs rm` (you might want to back up
   the entries somewhere to be safe)
