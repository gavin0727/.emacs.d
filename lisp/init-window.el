(use-package neotree
  :ensure t
  :bind ([f8] . my/neotree-toggle)
  :config
  (setq neo-window-width
        (max 30 (min 50 (/ (frame-width) 10))))
  (setq neo-theme 'nerd-icons)
  (defun my/project-root ()
    "Return the current project root using built-in project.el (Git repo preferred)."
    (or (when-let ((proj (project-current)))
          (expand-file-name (project-root proj)))
        default-directory))

  (defun my/neotree-project-dir ()
    "Open NeoTree at the project root and jump to current buffer."
    (interactive)
    (let ((project-dir (my/project-root))
          (current-file (buffer-file-name)))
      (neotree-dir project-dir)
      (when current-file
        (neotree-find current-file))))

  (defun my/neotree-toggle ()
    "Toggle neotree window, opening at project root and highlighting current buffer."
    (interactive)
    (if (neo-global--window-exists-p)
        (neotree-hide)
      (my/neotree-project-dir))))

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

(provide 'init-window)
