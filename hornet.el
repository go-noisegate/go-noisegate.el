;;; hornet.el --- Run hornet tests

(require 'compile)

;;; groups, customs, and vars

(defgroup hornet nil
  "Hornet utility"
  :group 'go)

(defvar hornet-history nil
  "History list for args")

(defvar-local hornet-changes nil
  "Change list [(begin, end)]")

;;; mode

(define-derived-mode hornet-mode compilation-mode "Hornet"
  "Major mode for hornet"
  (setq major-mode 'hornet-mode)
  (setq mode-name "Hornet")
  (setq-local truncate-lines t))

;;; internal funcs

(defun hornet--clear-buffer (buffer)
  (when (get-buffer buffer)
    (when (get-buffer-process (get-buffer buffer))
      (delete-process buffer))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer))))

(defun hornet--buffer-name (mode-name)
  "*Hornet*")

(defun hornet--get-args (history)
  (pcase current-prefix-arg
    (`nil "")
    (`-   (car (symbol-value history)))
    (`(4) (read-shell-command "hornet args: " (car (symbol-value history)) history))))

(defun hornet--get-offsets ()
  (mapconcat (function (lambda (ch) (format "#%d-%d" (- (car ch) 1) (- (nth 1 ch) 1))))  ;; emacs offset is 1-based
             hornet-changes ","))

(defun hornet--record-change (begin end length)
  "Record the change. Assumes [begin, end] (both inclusive)."
  (if (> begin end)
    (hornet--record-change end begin length)
    (let ((last-change (car hornet-changes)))
      (if (and last-change
               (<= (car last-change) (+ end 1))
               (<= (- begin 1) (nth 1 last-change)))
          (setcar hornet-changes (list (min (car last-change) begin) (max (nth 1 last-change) end)))
        (add-to-list 'hornet-changes (list begin end))))))

(defun hornet--reset-changes ()
  (setq hornet-changes nil))

;;; API

;;;###autoload
(defun hornet-record-change (begin end length)
  "Record the change. [begin, end)"
  (interactive)
  (when (string-suffix-p ".go" buffer-file-name)
    (hornet--record-change begin (- end 1) length)))

;;;###autoload
(defun hornet-hint ()
  "Sends the list of changes to the hornet server."
  (interactive)
  (when (string-suffix-p ".go" buffer-file-name)
    (hornet--record-change (point) (point) 0)
    (let ((offsets (hornet--get-offsets)))
      (hornet--reset-changes)
      (start-process "hornet-hint" nil "hornet" "hint" (concat buffer-file-name ":" offsets)))))

;;;###autoload
(defun hornet-test ()
  "Runs the tests in the current package based on the previous hints."
  (interactive)
  (when (string-suffix-p ".go" buffer-file-name)
    (hornet--record-change (point) (point) 0)
    (let ((buffer "*Hornet*")
          (offsets (hornet--get-offsets)))
      (hornet--reset-changes)
      (hornet--clear-buffer buffer)
      (compilation-start (concat "hornet test " (hornet--get-args 'hornet-history) " " buffer-file-name ":" offsets)
                         'hornet-mode
                         'hornet--buffer-name)
      (with-current-buffer "*Hornet*"
        (rename-buffer buffer)))))

(provide 'hornet)
