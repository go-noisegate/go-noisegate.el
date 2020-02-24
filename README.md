# hornet.el

Run hornet tests from Emacs.

# Prerequisite

* `hornet` command must be installed.

# Install

Add the following snippets to your init file.

```
(add-to-list 'load-path "[the path to the hornet.el directory]")
(require 'hornet)
```

TODO: use MELPA

# Commands

### hornet-test

Run hornet tests for the package to which the current file belongs.

To specify the hornet args, use the prefix arg (e.g. `C-u M-x hornet-test`. Then, you will be prompted). As the special case, the value from the most recent history is used when the prefix arg is `-` (e.g. `M-- M-x hornet-test`).

Some useful args are:
* `-tags tag,list` specifies the build tags.
* `-p off` specifies the sequential testing.

# Example key bindings

```
(define-key go-mode-map (kbd "C-c C-h") 'hornet-test)
```
