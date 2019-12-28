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

It includes the following interactive commands.

## hornet-test

Run hornet tests for the package to which the current file belongs.
