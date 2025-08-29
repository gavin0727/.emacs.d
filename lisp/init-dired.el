(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("C-c C-p" . wdired-change-to-wdired-mode))
  :config
  ;; Guess a default target directory
  (setq dired-dwim-target t)

  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)

  (setq dired-clean-confirm-killing-deleted-buffers nil)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-auto-revert-buffer #'dired-directory-changed-p)
  (setq dired-hide-details-hide-symlink-targets nil)
  ;; Show directory first
  (setq dired-listing-switches "-alh --group-directories-first"))

;; Colorful dired
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; Shows icons in dired
(use-package nerd-icons-dired
  :diminish
  :hook (dired-mode . nerd-icons-dired-mode))

(provide 'init-dired)

