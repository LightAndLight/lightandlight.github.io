---
title: Per-project Nix substituters
permalink: /per-project-nix-substituters
date: 2024-08-10
---

<div id="toc"><!-- generated --></div>

A lot of Nix projects end up with their own binary cache (also known as a "substituter").
Nix can be quite slow when it's configured to use many project-specific substituters[^slow],
because it queries them all when checking if it needs to build a derivation. In a specific
project, most of the other substituters Nix knows about are irrelevant, and querying them
is wasted effort. My solution is to only enable <https://cache.nixos.org> globally, and
to selectively enable other substituters while I'm working on a project that needs them.
Here's how I do it.

## Setup

In `configuration.nix`, set the system's trusted substituters[^trusted-substituters]:

```nix
nix.settings.trusted-substituters = [
  "https://cache.nixos.org/"
  "substituter-1"
  "substituter-2"
  "..."
  "substituter-N"
];

nix.settings.trusted-public-keys = [
  "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
  "other-public-key-1"
  "other-public-key-2"
  "..."
  "other-public-key-N"
];
```

And set Nix's default substituter:
  
```nix
nix.settings.substituters = [
  "https://cache.nixos.org/"
];
```

By default, only the substituters in `substituters` will be used by Nix.
`trusted-substituters` lists the substituters that regular users are allowed to use in Nix builds.

## Without Flakes

To enable other substituters, use the `NIX_CONFIG` environment variable
with the `extra-substituters`[^extra]<sup>,</sup>[^substituters] setting:

```bash
export NIX_CONFIG="extra-substituters = substituter-42"
```

Or use `substituters` to override the system's substituters:

```bash
export NIX_CONFIG="substituters = substituter-42"
```

If the substituters named in these local settings aren't defined in the `trusted-substituters`
system setting,
then Nix will ignore them and print the following message:

```
warning: ignoring untrusted substituter '{substituter URL}', you are not a trusted user.
```

I use [direnv](https://direnv.net/) to automatically set and unset `NIX_CONFIG` by putting the `export NIX_CONFIG=...`
command inside a `.envrc` in the project root.

## With Flakes

The `nixConfig` attribute can be used to set Nix configuration from inside a Flake[^nixConfig]:

```nix
{
  nixConfig.extra-substituters = [
    "substituter-42"
  ];
  
  inputs = ...;
  
  outputs = ...;
}
```

By default, Nix will ask for permission to use the config specified by the Flake.
I use [nix-direnv](https://github.com/nix-community/nix-direnv) to automatically load and unload [Nix develop shells](https://nix.dev/manual/nix/2.18/command-ref/new-cli/nix3-develop) when switching projects,
and the permission prompt [breaks direnv on my shell](https://github.com/direnv/direnv/issues/1022).

To work around this, I enable the [`accept-flake-config`](https://nix.dev/manual/nix/2.18/command-ref/conf-file#conf-accept-flake-config) Nix option in my `configuration.nix`: 

```nix
nix.extraOptions = ''
  experimental-features = nix-command flakes
  accept-flake-config = true
'';
```

[^slow]:
    Some relevant discussions:

    * <https://discourse.nixos.org/t/a-common-public-nix-cache/26998#many-caches-are-bad-2>
    * <https://github.com/NixOS/nix/issues/3019>

[^trusted-substituters]: [`trusted-substituters` &mdash; `nix.conf` reference](https://nix.dev/manual/nix/2.18/command-ref/conf-file#conf-trusted-substituters)

[^extra]: > A configuration setting usually overrides any previous value.
      > However, for settings that take a list of items, you can prefix the name of the setting
      > by `extra-` to append to the previous value. 
      >
      > &mdash; [`nix.conf` reference](https://nix.dev/manual/nix/2.18/command-ref/conf-file#file-format)

[^substituters]: [`substituters` &mdash; `nix.conf` reference](https://nix.dev/manual/nix/2.18/command-ref/conf-file#conf-substituters)

[^nixConfig]: See `nixConfig` in [Flake format &mdash; Nix Flake reference](https://nix.dev/manual/nix/2.18/command-ref/new-cli/nix3-flake.html#flake-format>)
