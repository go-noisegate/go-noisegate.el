;;; hornet.el --- Run hornet tests

(require 'compile)

;;; groups, customs, and vars

(defgroup hornet nil
  "Hornet utility"
  :group 'go)

(defvar hornet-history nil
  "History list for args")

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

;;; API

;;;###autoload
(defun hornet-hint ()
  "Notifies the hornet of the change of the file in the current buffer. The current cursor is used as the offset."
  (interactive)
  (when (string-match "\.go$" buffer-file-name)
    (start-process "hornet-hint" nil "hornet" "hint" (concat buffer-file-name ":#" (number-to-string (- (point) 1))))))

;;;###autoload
(defun hornet-test ()
  "Runs the tests based on the previous hints. The position of the current cursor is also used as the `hint`."
  (interactive)
  (let ((buffer "*Hornet*"))
    (when (string-match "\.go$" buffer-file-name)
      (hornet--clear-buffer buffer)
      (compilation-start (concat "hornet test " (hornet--get-args 'hornet-history) " " buffer-file-name ":#" (number-to-string (- (point) 1)))
                         'hornet-mode
                         'hornet--buffer-name)
      (with-current-buffer "*Hornet*"
        (rename-buffer buffer)))))

(provide 'hornet)
