---
title: Troubleshooting LightDM's test mode
permalink: /troubleshooting-lightdms-test-mode
date: 2024-08-03
---

<div id="toc" style="float: right;"><!-- generated --></div>

Recently I've been tweaking the appearance of [my desktop environment](https://github.com/LightAndLight/personal-configs),
which consists of [XMonad](https://xmonad.org/) and [Taffybar](https://github.com/taffybar/taffybar),
with [LightDM](https://github.com/canonical/lightdm) as the display manager,
all tied together using [NixOS](https://nixos.org/) and [home-manager](https://github.com/nix-community/home-manager).

Taffybar was very easy to customise because it uses a single CSS file.
XMonad was also not too bad, but needs a custom [X compositor](https://wiki.archlinux.org/title/Xorg#Composite) to get really nice results (I'm using [picom](https://github.com/yshui/picom)).
LightDM is the last thing on my list.

If I'm going to be tweaking my [greeter](https://wiki.archlinux.org/title/LightDM#Greeter)'s appearance,
I don't want to repeatedly log in and out to test it.
I'd like to preview the changes in a window while I've got my editor open in another.

[LightDM on ArchWiki](https://wiki.archlinux.org/title/LightDM#Testing) says to run `lightdm --test-mode`, but this didn't work for me.

## Summary

### Symptoms

* `lightdm --test-mode --debug` failed

  * Greeter logs (`.cache/lightdm/log/seat0-greeter.log`) show `Failed to open PAM session: Authentication failure`

* `sudo -u lightdm --test-mode --debug` reports `Failed to get D-Bus connection`

  * X logs (`/var/lib/lightdm/.cache/lightdm/log/x-1.log`) contain

    ```
    Authorization required, but no authorization protocol specified


    Xephyr cannot open host display. Is DISPLAY set?
    ```

### Solution

Run `xhost +SI:localuser:lightdm` to allow the `lightdm` user to connect to the X user,
then run `sudo -u lightdm --test-mode`.

## Details

When I ran `lightdm --test-mode` a small black window briefly appeared and then the program exited with status 1.
I added the `--debug` flag, and saw that a subprocess had failed:

```
$ lightdm --test-mode --debug
...
[+0.07s] DEBUG: Session pid=29039: Started with service 'lightdm-greeter', username '{my username}'
[+0.08s] DEBUG: Session pid=29039: Authentication complete with return value 0: Success
[+0.08s] DEBUG: Seat seat0: Session authenticated, running command
[+0.08s] DEBUG: Session pid=29039: Not setting XDG_VTNR
[+0.08s] DEBUG: Session pid=29039: Running command /nix/store/ikwkdnyzd8xflr3j7cabmy6vr3srvp3j-lightdm-gtk-greeter-2.0.8/bin/lightdm-gtk-greeter
[+0.08s] DEBUG: Creating shared data directory /var/lib/lightdm-data/{my username}
[+0.08s] WARNING: Could not create user data directory /var/lib/lightdm-data/{my username}: Error creating directory /var/lib/lightdm-data/{my username}: Permission denied
[+0.08s] DEBUG: Session pid=29039: Logging to /home/{my username}/.cache/lightdm/log/seat0-greeter.log
[+0.08s] DEBUG: Greeter closed communication channel
[+0.08s] DEBUG: Session pid=29039: Exited with return value 1
...
```

The output also told me where the subprocess stored its logs, so I checked there:

```
$ cat /home/{my username}/.cache/lightdm/log/seat0-greeter.log
Failed to open PAM session: Authentication failure
```

A [PAM](https://en.wikipedia.org/wiki/Linux_PAM) error.

`man pam` says that PAM errors are often logged to `syslog`, so I checked the [systemd journal](https://wiki.archlinux.org/title/Systemd/Journal):

```
$ journalctl --user -b -r -g pam -n 10
Aug 03 11:22:44 {my hostname} lightdm[29039]: pam_systemd(lightdm-greeter:session): Failed to create session: Access denied
Aug 03 11:22:44 {my hostname} lightdm[29039]: pam_succeed_if(lightdm-greeter:session): requirement "user = lightdm" not met by user "{my username}"
...
```

It seems that LightDM doesn't like being run as a normal user, and would rather be run as the `lightdm` user.
I found that the LightDM README actually has [a note](https://github.com/canonical/lightdm?tab=readme-ov-file#display-setup-script) about using `sudo -u lightdm` for `--test-mode`,
so I tried it:

```
$ sudo -u lightdm lightdm --test-mode --debug
[+0.00s] DEBUG: Logging to /var/lib/lightdm/.cache/lightdm/log/lightdm.log
[+0.00s] DEBUG: Starting Light Display Manager 1.32.0, UID=78 PID=30100
...
[+0.00s] DEBUG: Loading configuration from /etc/lightdm/lightdm.conf
[+0.00s] DEBUG: Running in user mode
[+0.00s] DEBUG: Using Xephyr for X servers
...
[+0.00s] DEBUG: Using D-Bus name org.freedesktop.DisplayManager
...
[+0.00s] DEBUG: Seat seat0: Starting
[+0.00s] DEBUG: Seat seat0: Creating greeter session
[+0.00s] DEBUG: Loading users from org.freedesktop.Accounts
[+0.00s] DEBUG: Seat seat0: Creating display server of type x
[+0.00s] DEBUG: Seat seat0: Starting local X display
[+0.01s] DEBUG: XServer 1: Logging to /var/lib/lightdm/.cache/lightdm/log/x-1.log
[+0.01s] DEBUG: XServer 1: Writing X server authority to /var/lib/lightdm/.cache/lightdm/run/root/:1
[+0.01s] DEBUG: XServer 1: Launching X Server
[+0.01s] DEBUG: Launching process 30107: /run/current-system/sw/bin/Xephyr :1 -seat seat0 -auth /var/lib/lightdm/.cache/lightdm/run/root/:1 -nolisten tcp
[+0.01s] DEBUG: XServer 1: Waiting for ready signal from X server :1
Failed to get D-Bus connection
```

That didn't work either.
I checked the X server logs that were generated:

```
$ sudo cat /var/lib/lightdm/.cache/lightdm/log/x-1.log
Authorization required, but no authorization protocol specified


Xephyr cannot open host display. Is DISPLAY set?
```

I didn't undertand what this meant. Is `DISPLAY` set?

```
$ sudo -u lightdm env | rg DISPLAY
DISPLAY=:0
```

It is. I was stumped.

After lots of searching and browsing,
a [Kagi search](https://kagi.com) for `lightdm --test-mode` turned up [Problems with lightdm test mode - Raspberry Pi Forums](https://forums.raspberrypi.com/viewtopic.php?t=220324),
which lead to [Capture Your LightDM Login Screen in Ubuntu Unity | UbuntuHandbook](https://ubuntuhandbook.org/index.php/2014/05/capture-your-lightdm-login-screen-in-ubuntu-14-04/),
which uses a variation of `sudo -u lightdm lightdm --test-mode`
after calling `xhost +SI:localuser:lightdm`.

`man xhost` says:

> The xhost program is used to add and delete host names or user names to the list allowed to make connections to the X server.
> In the case of hosts, this provides a rudimentary form of privacy control and security.
> It is only sufficient for a workstation (single user) environment, although it does limit the worst abuses.
> Environments which require more sophisticated measures should implement the user-based mechanism or use the hooks in the protocol for passing other authentication data to the server.

That seemed promising.

```
$ xhost +SI:localuser:lightdm
localuser:lightdm being added to access control list
```

Running LightDM again worked!

```
$ sudo -u lightdm lightdm --test-mode --debug
...
[+0.09s] DEBUG: Session pid=40112: Started with service 'lightdm-greeter', username 'lightdm'
[+0.09s] DEBUG: Session pid=40112: Authentication complete with return value 0: Success
[+0.09s] DEBUG: Seat seat0: Session authenticated, running command
[+0.09s] DEBUG: Session pid=40112: Not setting XDG_VTNR
[+0.09s] DEBUG: Session pid=40112: Running command /nix/store/ikwkdnyzd8xflr3j7cabmy6vr3srvp3j-lightdm-gtk-greeter-2.0.8/bin/lightdm-gtk-greeter
[+0.09s] DEBUG: Creating shared data directory /var/lib/lightdm-data/lightdm
[+0.09s] DEBUG: Session pid=40112: Logging to /var/lib/lightdm/.cache/lightdm/log/seat0-greeter.log
[+0.16s] DEBUG: Greeter connected version=1.32.0 api=1 resettable=false
[+0.35s] DEBUG: Greeter start authentication
[+0.35s] DEBUG: Session: Not setting XDG_VTNR
[+0.35s] DEBUG: Session pid=40144: Started with service 'lightdm', username '(null)'
[+0.35s] DEBUG: Session pid=40144: Got 1 message(s) from PAM
[+0.35s] DEBUG: Prompt greeter with 1 message(s)
```

My greeter appeared in a small window.
I don't know what the fundamental problem was; I know basically nothing about this area of Linux.
I'll probably learn more about it one day.
For now I'm content with having fixed my issue.
