# go-noisegate.el

Emacs plugin for [Noise Gate](https://github.com/go-noisegate/noisegate).

## Prerequisites

* Go 1.13 or later

## Install

1. The tool has the server (`gated`) and cli (`gate`). Install both:

   ```sh
   $ go get -u github.com/go-noisegate/noisegate/cmd/gate && go get -u github.com/go-noisegate/noisegate/cmd/gated
   ```

2. Copy [`go-noisegate.el`](https://raw.githubusercontent.com/go-noisegate/go-noisegate.el/master/go-noisegate.el) to `load-path` and add the following snippets to your init file.

   ```
   (require 'go-noisegate)
   ```

   Or you may add the repository root to the `load-path`.

   ```
   (add-to-list 'load-path "[path to this repository]")
   (require 'go-noisegate)
   ```

   TODO: support MELPA

## Configuration

The typical configuration is:
* While you edit a file, the plugin updates the list of recent changes.
* When the file is saved, the `go-noisegate-hint` command is automatically called. It sends the list of recent changes to the server.
* To run the test, type `C-c C-t`, which calls the `go-noisegate-test` command. It runs the tests affected by the recent changes.
* To run all the tests regardless of recent changes, type `C-c C-a`, which calls the `go-noisegate-test-all` command.


Here is the configuration to achieve this:

```
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'after-change-functions 'go-noisegate-record-change)
            (add-hook 'after-save-hook 'go-noisegate-hint)
            (local-set-key (kbd "C-c C-t") 'go-noisegate-test)
            (local-set-key (kbd "C-c C-a") 'go-noisegate-test-all)))
```

The document below assumes you configured your editor in this way.

## Quickstart

This quickstart shows you how to use the Noise Gate to get faster test results.

### Set up

1. Run the server program (`gated`) if it's not running yet.

   ```sh
   $ gated
   ```

2. Download the quickstart repository.

   ```sh
   $ go get -u github.com/go-noisegate/quickstart
   ```

### Run your tests

Let's assume you just implemented some [functions](https://github.com/go-noisegate/quickstart/blob/master/math.go) (`SlowAdd` and `SlowSub`) and [tests](https://github.com/go-noisegate/quickstart/blob/master/math_test.go) (`TestSlowAdd`, `TestSlowAdd_Overflow` and `TestSlowSub`) at the `quickstart` repository.

1. Run all the tests

   First, check if all the tests are passed. Open `math.go` at the the repository root and type `C-c C-a` (or call the `go-noisegate-test-all` command directly).

   ```
   gate test -bypass /Users/ks888/go/src/github.com/go-noisegate/quickstart/ -- -v 
   Run all tests:
   === RUN   TestSlowAdd
   --- PASS: TestSlowAdd (1.00s)
   === RUN   TestSlowAdd_Overflow
   --- PASS: TestSlowAdd_Overflow (1.00s)
   === RUN   TestSlowSub
   --- FAIL: TestSlowSub (1.00s)
       math_test.go:22: wrong result: 2
   FAIL
   FAIL	github.com/go-noisegate/quickstart	3.013s
   FAIL
   ```

   * One failed test. We will fix this soon.
   * The tool internally calls `go test` and the `-v` option is passed by default. See the [How-to guides](#how-to-guides) section to pass other options.

2. Change the code

   To fix the failed test, change [the `SlowSub` function](https://github.com/go-noisegate/quickstart/blob/master/math.go#L12). `return a + b` at the line 12 should be `return a - b`. Then save it.

   * While you edit the file, the plugin updates the list of changes.
   * When you save the file, the plugin sends the list of changes to the server.

3. Run the tests affected by the recent changes

   Let's check if the test is fixed. Type `C-c C-t` (or call the `go-noisegate-test` command directly).

   ```
   gate test /Users/ks888/go/src/github.com/go-noisegate/quickstart/ -- -v 
   Changed: [SlowSub]
   === RUN   TestSlowSub
   --- PASS: TestSlowSub (1.00s)
   PASS
   ok  	github.com/go-noisegate/quickstart	1.008s
   ```

   * The recent changes are listed at the `Changed: [SlowSub]` line. The list is cleared when all the tests are passed.
   * Based on the recent changes, the tool selects and runs only the `TestSlowSub` test.
   * *You get the faster test results (`3.013s` -> `1.008s`)!*

## How-to guides

### Pass options to `go test`

Use the prefix arg: type `C-u C-c C-t` and then specify the options (e.g. `--tags tags,list`). Once you specify the options, the same options are passed next time.

### Run a specific test

Type `C-c C-t` when the cursor points to the body of the test function.

The current cursor position is also considered as the recent change so that we can run some test without edit.

## Command reference

`go-noisegate.el` includes the following interactive commands.

### go-noisegate-record-change

Record the change (= the byte offsets of the changed region).

### go-noisegate-hint

Sends the list of changes to the server.

### go-noisegate-test

Runs the tests affected by the recent changes.

The current cursor position is also considered as the recent change.

Use the prefix arg to pass the options to `go test`: type `C-u M-x go-noisegate-test` then enter the options.

### go-noisegate-test-all

Runs all the tests in the current package regardless of the recent changes.

Use the prefix arg to pass the options to `go test`: type `C-u M-x go-noisegate-test-all` then enter the options.

## How it works

See [DEVELOPMENT.md](https://github.com/go-noisegate/noisegate/blob/master/DEVELOPMENT.md).
