# hornet.el

Run [hornet](https://github.com/ks888/hornet) from Emacs.

## Features

Hornet is the Golang test runner for the speedster.

The core features are:
* **Change-driven**: by the integration with your editor, hornet knows what changes you made. It runs the tests affected by these changes first.
* **Tuned for high-speed**: hornet implements some strategies to run the tests faster, including tests in parallel. You may disable these features for safety.

## Prerequisites

* Go 1.13 or later
* Linux or Mac OS X

## Install

1. Hornet has the server program (`hornetd`) and client program (`hornet`). Install both:

   ```
   $ go get -u github.com/ks888/hornet/cmd/hornet && go get -u github.com/ks888/hornet/cmd/hornetd
   ```

2. Copy [`hornet.el`](https://raw.githubusercontent.com/ks888/hornet.el/master/hornet.el) to `load-path` and add the following snippets to your init file.

   ```
   (require 'hornet)
   ```

   Or you may add the repository root to the `load-path`.

   ```
   (add-to-list 'load-path "[path to this repository]")
   (require 'hornet)
   ```

   TODO: support MELPA

## Configuration

The typical configuration is:
* While you edit a file, the plugin updates the list of changes you made.
* When the file is saved, sends the list of changes to the hornet server.
* To run the test, type `C-c C-h`, which calls `hornet-test` command. It runs the tests affected by your changes first and then runs the rest.

Here is the configuration to achieve this:

```
(add-hook 'go-mode-hook
          (lambda () (add-hook 'after-change-functions 'hornet-record-change)))
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

   Open `math.go` in the the repository root and type `C-c C-h` (or call the `hornet-test` command directly). It runs all the tests in the current package.

   ```
   hornet test  /Users/yagami/go/src/github.com/ks888/hornet-tutorial/math.go:#0-0
   Changed: []

   Run affected tests:

   Run other tests:
   === RUN   TestSlowSub
   --- FAIL: TestSlowSub (1.00s)
       math_test.go:22: wrong result: 2
   === RUN   TestSlowAdd
   --- PASS: TestSlowAdd (1.00s)
   === RUN   TestSlowAdd_Overflow
   --- PASS: TestSlowAdd_Overflow (1.00s)
   FAIL (1.034643007s)
   ```

   * `Changed` and `Run affected tests` are empty since we don't make any changes yet.
   * One failed test. We will fix this next.
   * The test time is `1.034643007s` because the tests run in parallel. When you run the same tests using `go test`, it takes about 3 seconds.

2. Fix the bug

   Fix [the `SlowSub` function](https://github.com/ks888/hornet-tutorial/blob/master/math.go#L12). `return a + b` at the line 12 should be `return a - b`. Then save it.

   While you edit the file, the plugin updates the list of changes you made. When you save the file, it sends the list of changes to the hornet server.

3. Run the tests again

   When you type `C-c C-h` again, the previous hint is considered.

   ```
   hornet test  /Users/yagami/go/src/github.com/ks888/hornet-tutorial/math.go:#177-177
   Changed: [SlowSub]

   Run affected tests:
   === RUN   TestSlowSub
   --- PASS: TestSlowSub (1.00s)

   Run other tests:
   === RUN   TestSlowAdd
   --- PASS: TestSlowAdd (1.00s)
   === RUN   TestSlowAdd_Overflow
   --- PASS: TestSlowAdd_Overflow (1.00s)
   PASS (1.041285146s)
   ```

   *The tool knows you've changed the `SlowSub` function and runs affected tests (`TestSlowSub`) first.*

## How-to guides

### Run tests in sequence

Some tests fail when they are executed in parallel. You can run tests in sequence using the prefix arg: type `C-u C-c C-h` and then enter `--parallel off` or `-p off`.

### Specify the build tags

You can use the prefix arg: type `C-u C-c C-h` and then enter `--tags tags,list`.

### Run the same command as before

It's bothersome to specify the same args again and again. To run the same command, specify `-` as the prefix arg: type `M-- C-c C-h` or `C-u - C-c C-h`.

### Run the specified test

Simply type `C-c C-h` when the cursor points to the body of the target test.

Before the tests run, the `hornet test` adds the position of the current cursor to the changes list, so that we can run the test to which the cursor points first without editing the file.

## Command reference

`hornet.el` includes the following interactive commands.

### hornet-record-change

Record the change (the beginning and end of the region just changed).

### hornet-hint

Sends the list of changes to the hornet server.

### hornet-test

Runs the tests in the current package based on the previous hints.

Use the prefix arg to specify the args of the `hornet hint` cli command: `C-u M-x hornet-test` then you will be prompted. When the prefix arg is `-`, the value from the most recent history is used.
