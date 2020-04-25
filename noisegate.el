;;; noisegate.el --- Emacs plugin for Noise Gate

;; Author: The Noise Gate Authors
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: languages, go, test
;; URL: https://github.com/go-noisegate/noisegate.el

;;; Commentary:

;;; Code:

(require 'compile)

;;; groups, customs, and vars

(defgroup noisegate nil
  "Noise Gate utility"
  :group 'go)

(defvar noisegate-history (list "-v ")
  "History list for args.")

(defvar-local noisegate-changes nil
  "Change list [(begin, end)]")

(defvar noisegate-gate-cmd (noisegate--find-gate-command)
  "the path to the gate command.")

;;; mode

(define-derived-mode noisegate-mode compilation-mode "Noise Gate"
  "Major mode for Noise Gate"
  (setq major-mode 'noisegate-mode)
  (setq mode-name "Noise Gate")
  (setq-local truncate-lines t))

;;; internal funcs

(defun noisegate--clear-buffer (buffer)
  "Kill the BUFFER process and clear the BUFFER."
  (when (get-buffer buffer)
    (when (get-buffer-process (get-buffer buffer))
      (delete-process buffer))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer))))

(defun noisegate--buffer-name (mode-name)
  "Return the buffer name.  MODE-NAME is not used."
  "*Noise Gate*")

(defun noisegate--get-args (history)
  "Get the go test options.
When the prefix arg is not given, the first element of HISTORY is used.
When the single prefix arg is given, a user is prompted."
  (pcase current-prefix-arg
    (`nil (car (symbol-value history)))
    (`(4) (read-shell-command "go test args: " (car (symbol-value history)) history))))

(defun noisegate--get-offset (ch)
  "Make the string representation of CH which the noise gate server can parse."
  (format "#%d-%d" (- (car ch) 1) (- (nth 1 ch) 1)))  ;; emacs offset is 1-based

(defun noisegate--get-offsets ()
  "Make the string representation of changes."
  (mapconcat 'noisegate--get-offset noisegate-changes ","))

(defun noisegate--record-change (begin end length)
  "Record the change.  Assumes [BEGIN, END] (both inclusive).
LENGTH is not used."
  (if (> begin end)
    (noisegate--record-change end begin length)
    (let ((last-change (car noisegate-changes)))
      (if (and last-change
               (<= (car last-change) (+ end 1))
               (<= (- begin 1) (nth 1 last-change)))
          (setcar noisegate-changes (list (min (car last-change) begin) (max (nth 1 last-change) end)))
        (add-to-list 'noisegate-changes (list begin end))))))

(defun noisegate--reset-changes ()
  "Reset the changes so far."
  (setq noisegate-changes nil))

(defun noisegate--find-gopath ()
  "Find GOPATH."
  (with-temp-buffer
    (call-process "go" nil (current-buffer) nil "env" "GOPATH")
    ;; trim right
    (car (split-string (buffer-string)))))

(defun noisegate--find-gate-command ()
  "Find the gate command."
  (let ((cmd-in-gopath (concat (noisegate--find-gopath) "/bin/gate")))
    (if (file-exists-p cmd-in-gopath) cmd-in-gopath "gate")))

;;; API

;;;###autoload
(defun noisegate-record-change (begin end length)
  "Record the change (BEGIN and END).  LENGTH is not used."
  (interactive)
  (when (string-suffix-p ".go" buffer-file-name)
    (noisegate--record-change begin (- end 1) length)))

;;;###autoload
(defun noisegate-hint ()
  "Send the list of changes to the server."
  (interactive)
  (when (and (string-suffix-p ".go" buffer-file-name) noisegate-changes)
    (let ((offsets (noisegate--get-offsets)))
      (noisegate--reset-changes)
      (start-process "noisegate-hint" "*Messages*" noisegate-gate-cmd "hint" (concat buffer-file-name ":" offsets)))))

;;;###autoload
(defun noisegate-test ()
  "Run the tests affected by the recent changes."
  (interactive)
  (when (string-suffix-p ".go" buffer-file-name)
    (let ((buffer "*Noise Gate*")
          (offset (noisegate--get-offset (list (point) (point)))))
      (noisegate--clear-buffer buffer)
      (call-process noisegate-gate-cmd nil nil nil "hint" (concat buffer-file-name ":" offset))
      (compilation-start (concat noisegate-gate-cmd " test "  (file-name-directory buffer-file-name) " -- " (noisegate--get-args 'noisegate-history))
                         'noisegate-mode
                         'noisegate--buffer-name)
      (with-current-buffer "*Noise Gate*"
        (rename-buffer buffer)))))

;;;###autoload
(defun noisegate-test-all ()
  "Run all the tests in the current package regardless of the recent changes."
  (interactive)
  (when (string-suffix-p ".go" buffer-file-name)
    (let ((buffer "*Noise Gate*"))
      (noisegate--clear-buffer buffer)
      (compilation-start (concat noisegate-gate-cmd " test -bypass " (file-name-directory buffer-file-name) " -- " (noisegate--get-args 'noisegate-history))
                         'noisegate-mode
                         'noisegate--buffer-name)
      (with-current-buffer "*Noise Gate*"
        (rename-buffer buffer)))))

(provide 'noisegate)
;;; noisegate.el ends here
