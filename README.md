# hornet.el

Run hornet from Emacs.

## Prerequisites

* [Hornet](github.com/ks888/hornet) is installed.

## Install

Copy `hornet.el` to `load-path` and add the following snippets to your init file.

```
(require 'hornet)
```

Or you may add the repository root to the `load-path`.

```
(add-to-list 'load-path "[the path to the repository root]")
(require 'hornet)
```

TODO: support MELPA

## Configuration

The typical configuration is:
* When the file is saved, automatically call `hornet hint` command, which notifies the hornet server of the changed filename and position.
* When you want to run the test, type `C-c C-h`, which calls `hornet test` command. It runs the tests based on the previous hints.

Here is the configuration to achieve this:

```
(add-hook 'go-mode-hook
          (lambda () (add-hook 'after-save-hook 'hornet-hint)))
(define-key go-mode-map (kbd "C-c C-h") 'hornet-test)
```

The document below assumes you configured your editor in this way.

## Quickstart

This quickstart shows you how to use hornet to help your coding.

### Set up

1. Run the server program (`hornetd`) if it's not running yet.

   ```sh
   $ hornetd
   ```

2. Download the sample repository.

   ```sh
   $ go get -u github.com/ks888/hornet-tutorial
   ```

### Coding

Let's assume you just implemented some [functions](https://github.com/ks888/hornet-tutorial/blob/master/math.go) (`SlowAdd` and `SlowSub`) and [tests](https://github.com/ks888/hornet-tutorial/blob/master/math_test.go) (`TestSlowAdd`, `TestSlowAdd_Overflow` and `TestSlowSub`) in the `hornet-tutorial` repository.

1. Run the tests

   Open `math.go` in the the repository root and type `C-c C-h`. It runs all the tests in the package. You will see the output like this:

   ```
   hornet test  /Users/yagami/go/src/github.com/ks888/hornet-tutorial/math.go:#31
   No important tests. Run all the tests:
   === RUN   TestSlowAdd_Overflow
   --- PASS: TestSlowAdd_Overflow (1.00s)
   === RUN   TestSlowAdd
   --- PASS: TestSlowAdd (1.00s)
   === RUN   TestSlowSub
   --- FAIL: TestSlowSub (1.00s)
       math_test.go:22: wrong result: 2
   FAIL (1.032443223s)
   ```

   Obviously there is one failed test.

2. Fix the bug

   Fix the `SlowSub` function. `return a + b` at the line 12 should be `return a - b`. Then save it.

   Now the `hornet hint` command automatically runs and it notifies the hornet server of the changed filename and position.

3. Run the tests again

   When you type `C-c C-h` again, the previous hint is considered.

   ```
   hornet test  /Users/yagami/go/src/github.com/ks888/hornet-tutorial/math.go:#166
   Found important tests. Run them first:
   === RUN   TestSlowSub
   --- PASS: TestSlowSub (1.00s)

   Run other tests:
   === RUN   TestSlowAdd
   --- PASS: TestSlowAdd (1.00s)
   === RUN   TestSlowAdd_Overflow
   --- PASS: TestSlowAdd_Overflow (1.00s)
   PASS (1.037108699s)
   ```

   Based on the hint, hornet runs `TestSlowSub` first because it's affected by the previous change.

   Note that the total test time is `1.033799777s` here because the tests run in parallel. When you run the same tests using `go test`, it will take about 3 seconds.

## How-to guides

### Run tests in sequence

Some tests fail when they are executed in parallel. You can run tests in sequence using the prefix arg: type `C-u C-c C-h` and then enter `--parallel off` or `-p off`.

### Specify the build tags

You can use the prefix arg: type `C-u C-c C-h` and then enter `--tags tags,list`.

### Run the same command as before

It's bothersome to specify the same args again and again. To run the same command, specify `-` as the prefix arg: type `M-- C-c C-h` or `C-u - C-c C-h`.

## Command reference

`hornet.el` includes the following interactive commands.

### hornet-hint

Notifies the hornet server of the changed filename and position. The current cursor is used as the position.

### hornet-test

Runs tests based on the previous hints. The current cursor is also considered as one hint.

Use the prefix arg to specify the args of the `hornet hint` cli command: `C-u M-x hornet-test` then you will be prompted. When the prefix arg is `-`, the value from the most recent history is used.
