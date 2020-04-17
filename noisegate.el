;;; noisegate.el --- Run Noise Gate from Emacs

(require 'compile)

;;; groups, customs, and vars

(defgroup noisegate nil
  "Noise Gate utility"
  :group 'go)

(defvar noisegate-history (list "-v ")
  "History list for args")

(defvar-local noisegate-changes nil
  "Change list [(begin, end)]")

;;; mode

(define-derived-mode noisegate-mode compilation-mode "Noise Gate"
  "Major mode for Noise Gate"
  (setq major-mode 'noisegate-mode)
  (setq mode-name "Noise Gate")
  (setq-local truncate-lines t))

;;; internal funcs

(defun noisegate--clear-buffer (buffer)
  (when (get-buffer buffer)
    (when (get-buffer-process (get-buffer buffer))
      (delete-process buffer))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer))))

(defun noisegate--buffer-name (mode-name)
  "*Noise Gate*")

(defun noisegate--get-args (history)
  (pcase current-prefix-arg
    (`nil (car (symbol-value history)))
    (`(4) (read-shell-command "go test args: " (car (symbol-value history)) history))))

(defun noisegate--get-offset (ch)
  (format "#%d-%d" (- (car ch) 1) (- (nth 1 ch) 1)))  ;; emacs offset is 1-based

(defun noisegate--get-offsets ()
  (mapconcat 'noisegate--get-offset noisegate-changes ","))

(defun noisegate--record-change (begin end length)
  "Record the change. Assumes [begin, end] (both inclusive)."
  (if (> begin end)
    (noisegate--record-change end begin length)
    (let ((last-change (car noisegate-changes)))
      (if (and last-change
               (<= (car last-change) (+ end 1))
               (<= (- begin 1) (nth 1 last-change)))
          (setcar noisegate-changes (list (min (car last-change) begin) (max (nth 1 last-change) end)))
        (add-to-list 'noisegate-changes (list begin end))))))

(defun noisegate--reset-changes ()
  (setq noisegate-changes nil))

;;; API

;;;###autoload
(defun noisegate-record-change (begin end length)
  "Record the change."
  (interactive)
  (when (string-suffix-p ".go" buffer-file-name)
    (noisegate--record-change begin (- end 1) length)))

;;;###autoload
(defun noisegate-hint ()
  "Sends the list of changes to the server."
  (interactive)
  (when (and (string-suffix-p ".go" buffer-file-name) noisegate-changes)
    (let ((offsets (noisegate--get-offsets)))
      (noisegate--reset-changes)
      (start-process "noisegate-hint" "*Messages*" "gate" "hint" (concat buffer-file-name ":" offsets)))))

;;;###autoload
(defun noisegate-test ()
  "Runs the tests affected by the recent changes."
  (interactive)
  (when (string-suffix-p ".go" buffer-file-name)
    (let ((buffer "*Noise Gate*")
          (offset (noisegate--get-offset (list (point) (point)))))
      (noisegate--clear-buffer buffer)
      (call-process "gate" nil nil nil "hint" (concat buffer-file-name ":" offset))
      (compilation-start (concat "gate test "  (file-name-directory buffer-file-name) " -- " (noisegate--get-args 'noisegate-history))
                         'noisegate-mode
                         'noisegate--buffer-name)
      (with-current-buffer "*Noise Gate*"
        (rename-buffer buffer)))))

(defun noisegate-test-all ()
  "Runs all the tests in the current package regardless of the recent changes."
  (interactive)
  (when (string-suffix-p ".go" buffer-file-name)
    (let ((buffer "*Noise Gate*"))
      (noisegate--clear-buffer buffer)
      (compilation-start (concat "gate test -bypass " (file-name-directory buffer-file-name) " -- " (noisegate--get-args 'noisegate-history))
                         'noisegate-mode
                         'noisegate--buffer-name)
      (with-current-buffer "*Noise Gate*"
        (rename-buffer buffer)))))

(provide 'noisegate)
