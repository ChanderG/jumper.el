# Jumper

Fast single-character buffer switch ala registers.

## Motivation

In Emacs, we usually have 100s of buffers. This model doesn't lend itself to the "tab-bar" approach which other editors (and softwares like Browsers etc) have. Instead we use (fuzzy) search from a list of items such as `switch-to-buffer` or `helm-mini` (and the like). This model works very well for general use, getting to any arbitrary buffer in a few characters of search.

On the other end, we have `previous-buffer` to switch between 2 active buffers. This also works very well.

I have observed that in my own use, I tend to frequently switch between a small set of buffers, where small is in the range 3-5 in a single working session.

This number of buffers is an awkward position: too many for the simple alternative buffer switch, too few that the overhead of a searcher (opening, searching a few chars and hitting Enter) adds visible delays. I find myself getting frustated in these scenarios and Jumper is meant to solve this narrow problem space.

## Alternatives

I realized after creating Jumper that Emacs does have a solution for this problem out of the box: Registers. You can build a workflow like Jumper quite easily with Registers. But, since I already have this built, I will continue on this approach.

## Install

Git clone or obtain this folder via quelpa:
```
cd ~/Customization
git clone https://github.com/ChanderG/jumper.el
```

Setup keys to trigger jumper as needed:
``` el
(use-package jumper.el
  :load-path "~/Customization/jumper.el/"
  :bind (("C-c j" . jumper-trigger)
         :map evil-normal-state-map
         ("j" . jumper-trigger)))
```

## Usage/Features

Bind the Jumper trigger into a easy to use key. I use `j` since I use evil-mode and still use the arrow-keys all the time.

1. Fast creation of jump locations. `j .` adds the current buffer into the jumper listing using the first character of the buffer name, casted to lowercase. 
2. Another option is to use `j + x` where the current buffer will be added using the shortcut x. The idea is to use memorable characters. You should use `j .` all the time (assuming no first char collisions) and use `+` only to manage the rare conflict.
2. Quick switch. `j x` will take you to the buffer marked using `x`.
3. Quick removal. `j - x` will remove the jump location x from the listing.
4. Quick clear out. `j J` will clear out all entries. 
5. Lack of persistance is a feature. Jumper listing is meant to be a cache - easily discarded and built up as required.

The usage assumption of Jumper is that this listing will change completely as you work on different tasks.

