;; Jump to arbitrary positions
(use-package avy
  :ensure t
  ;; integrate with isearch and others
  ;; C-' to select isearch-candidate with avy
  :hook (after-init . avy-setup-default)
  :bind (("M-j" . nil)
         ("M-j M-j" . avy-goto-char-timer)
         ("M-j M-l" . avy-goto-line))
  :custom
  (avy-background t)
  (avy-all-windows nil)
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?u ?i ?o ?p))
  ;; overlay is used during isearch, `pre' style makes avy keys evident.
  (avy-styles-alist '((avy-isearch . pre))))

;; Writable grep buffer
(use-package wgrep
  :ensure t
  :hook (grep-setup . wgrep-setup)
  :custom
  (wgrep-change-readonly-file t))

(use-package iedit
  :ensure t
  :bind (("M-;" . iedit-mode))
  :config
  (with-eval-after-load 'iedit
    (define-key iedit-mode-keymap (kbd "M-'") 'iedit-show/hide-context-lines)
    (define-key iedit-mode-keymap (kbd "M-;") 'iedit--quit)))

(use-package ivy
  :defer 1
  :demand
  :hook (after-init . ivy-mode)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-initial-inputs-alist nil
        ivy-count-format "%d/%d "
        enable-recursive-minibuffers t
        ivy-re-builders-alist '((t . ivy--regex-ignore-order))))

(use-package counsel
  :after (ivy)
  :bind (("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c f"   . counsel-recentf)
         ("C-c e"   . counsel-git)
         ("C-c s"   . counsel-rg)
         ("M-g i"   . counsel-imenu))
  :config
  (setq counsel-rg-base-command
        "rg -S --no-heading --line-number %s --glob '!TAGS'"))

(use-package swiper
  :after (ivy)
  :bind (("C-s" . swiper)
         ("C-r" . swiper-isearch-backward))
  :config (setq swiper-action-recenter t
                swiper-include-line-number-in-search t))

(use-package ivy-rich
  :ensure t
  :after counsel
  :init
  (ivy-rich-mode 1)

  :config
  (defun my-ivy-rich-file-transformer ()
    '(:columns
      ((ivy-rich-candidate (:width 40))
       (ivy-rich-file-permissions (:width 11))
       (ivy-rich-file-size (:width 7))
       (ivy-rich-file-last-modified-time
        (:width 20 :face font-lock-comment-face)))))

  (dolist (cmd '(counsel-find-file
                 counsel-recentf
                 counsel-git
                 project-find-file))
    (setf (alist-get cmd ivy-rich-display-transformers-list)
          (my-ivy-rich-file-transformer))))

(use-package nerd-icons-ivy-rich
  :ensure t
  :after (counsel ivy-rich)
  :init (nerd-icons-ivy-rich-mode 1))

(provide 'init-edit)
