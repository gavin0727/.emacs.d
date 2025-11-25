(use-package ace-window
  :hook (emacs-startup . ace-window-display-mode)
  :bind (("C-c w s" . ace-swap-window))
  :config
  ;; Select widnow via `M-1'...`M-9'
  (defun aw--select-window (number)
    "Slecet the specified window."
    (when (numberp number)
      (let ((found nil))
        (dolist (win (aw-window-list))
          (when (and (window-live-p win)
                     (eq number
                         (string-to-number
                          (window-parameter win 'ace-window-path))))
            (setq found t)
            (aw-switch-to-window win)))
        (unless found
          (message "No specified window: %d" number)))))

  (defvar my-aw-keys-mode-map (make-sparse-keymap)
    "Keymap for `my-aw-keys-mode'.")

  (dotimes (i 9)
    (let ((n (1+ i)))
      (define-key my-aw-keys-mode-map (kbd (format "M-%d" n))
        `(lambda ()
           (interactive)
           (aw--select-window ,n)))))

  (define-minor-mode my-aw-keys-mode
    "Global minor mode to override M-1..M-9 for ace-window."
    :global t
    :keymap my-aw-keys-mode-map)

  (my-aw-keys-mode 1))

(defun kill-buffer-with-window ()
  "Kill current buffer and close its window if more than one window exists."
  (interactive)
  (kill-buffer)
  (unless (one-window-p)
    (delete-window)))

(defun kill-buffer-without-window ()
  "Kill current buffer without confirm."
  (interactive)
  (kill-buffer))

(global-set-key (kbd "C-x k") #'kill-buffer-without-window)
(global-set-key (kbd "C-x K") #'kill-buffer-with-window)

(provide 'init-window)
