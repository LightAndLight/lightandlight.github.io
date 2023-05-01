---
title: WebGL Slower Than Canvas?
author: ielliott95
permalink: /webgl-slower-than-canvas
date: 2021-05-20 09:45:00 +1000
tags:
    - programming
---

I've recently been working on [web a graphics project](https://github.com/LightAndLight/rust-wasm-gol/)
where I played a [Canvas API](https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API) implementation
against a [WebGL](https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API) implementation. When running the app in Firefox on Linux, I was surprised to find that the WebGL version was 10-30% *slower* 
(in frames per second) than the Canvas version. 

After much searching I was able to diagnose and fix the problem, which came 
down to graphics drivers and browser rendering settings. If your problem is
similar to mine, here's how you might be able to solve it:

1. Check GPU driver status in the browser.

    In Firefox, navigate to `about:support`. `Ctrl+F Graphics`. Look at the `WebGL 1 Driver {Renderer,Version}`
    rows. If you don't see your GPU manufacturer and model in these rows, then you need to install
    the correct drivers. 
    
    For reference, I'm running a Nvidia GeForce RTX2070. Without drivers, my driver renderer was `VMware, Inc. -- llvmpipe (LLVM 9.0.1, 256 bits)` 
    and my driver version was `3.1 Mesa 20.1.10`.

2. Install GPU drivers.

    I'm on NixOS, so this was as simple as adding

    ```
    nixpkgs.config.allowUnfree = true;
    services.xserver.videoDrivers = [ "nvidia" ];
    ```

    to my `configuration.nix`, then running `sudo nixos-rebuild switch && reboot`.

3. Confirm driver installation.

    Repeat step 1. If you still can't see your GPU manufacturer and model, then I can't help you. 
    
    After step 2, my driver renderer was `NVIDIA Corporation -- GeForce RTX 2070/PCIe/SSE2` and driver version was
    `4.6.0 NVIDIA 455.38`.

4. Enable [WebRender](https://hacks.mozilla.org/2017/10/the-whole-web-at-maximum-fps-how-webrender-gets-rid-of-jank/).

    In the Graphics section of `about:support`, check the `Compositing` row. If it says `WebRender`, then you're
    done. If it says `Basic`, then you need to enable WebRender.

    Navigate to `about:config`, move past the warning, and search for `gfx.webrender.enabled`. Set it to `true` and
    restart Firefox. Confirm this change by checking `Compositing` row in `about:support`.

Before, each [requestAnimationFrame](https://developer.mozilla.org/en-US/docs/Web/API/window/requestAnimationFrame) call
lasted 14-15ms in my Canvas implementation, but the app was running well below 60fps. Each frame lasted ~10ms in my WebGL implementation,
but the framerate was even worse than the Canvas version!

After following these instructions, both my Canvas implementation and WebGL implementation run at 60fps. Canvas' frame 
duration didn't appear to change, but WebGL's frame duration dropped to ~4ms. Yay!