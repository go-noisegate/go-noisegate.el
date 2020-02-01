;;; flycheck-hornet.el --- Set up hornet

(require 'flycheck)

;;; TODO: support the `addr` option.

(flycheck-define-checker hornet-setup
  "Sets up the specified file and its repository to execute the upcoming test quickly."
  ;; workaround to avoid watching an unmodified file. `:predicate` option is somehow slow.
  :command ("hornet" "setup" (eval (unless (buffer-modified-p) "--help")) source-original)
  :error-patterns  ((error line-start (file-name) ":" line ":" column ": " (message) line-end)
                    (error line-start (file-name) ":" line ":" (message) line-end))
  :modes go-mode
  )

;;;###autoload
(defun flycheck-hornet-setup ()
  "Sets up the flycheck hornet."
  (interactive)
  (add-to-list 'flycheck-checkers 'hornet-setup)
  )

(provide 'flycheck-hornet)
