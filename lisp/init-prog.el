;; smart comment
(use-package prog-mode
  :ensure nil
  :hook
  (prog-mode . (lambda ()
                 (local-unset-key (kbd "C-c C-c"))
                 (local-set-key (kbd "C-c C-c") #'comment-dwim))))

;; show TAB
(use-package whitespace
  :ensure nil
  :hook (prog-mode . whitespace-mode)
  :config
  (setq whitespace-style '(face tabs tab-mark trailing))
  (setq backward-delete-char-untabify-method 'nil)
  (set-face-attribute 'whitespace-tab nil
                      :foreground "#636363"
                      :background (face-background 'default nil t))
  (setq whitespace-display-mappings
        '((tab-mark 9 [124 9] [92 9]))))

;; xref
(use-package xref
  :ensure nil
  :hook ((xref-after-return xref-after-jump) . recenter)
  :custom
  ;; Emacs 28+
  ;;
  ;; `project-find-regexp' can be faster when setting `xref-search-program' to
  ;;  `ripgrep'.
  (xref-search-program (cond ((executable-find "rg") 'ripgrep)
                             ((executable-find "ugrep") 'ugrep)
                             (t 'grep)))
  (xref-history-storage 'xref-window-local-history)
  (xref-show-xrefs-function #'xref-show-definitions-completing-read)
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

;; auto complete
(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 3)
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.3)
  (setq company-show-numbers t)
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence))
  (setq company-backends
        '((company-etags
           company-dabbrev-code)
          company-dabbrev)))

;; code navi
(use-package counsel-etags
  :ensure t
  :bind (("M-." . counsel-etags-find-tag-at-point))
  :init
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        'counsel-etags-virtual-update-tags 'append 'local)))
  :config
  (setq tags-revert-without-query t)
  (setq counsel-etags-update-interval 60)
  (push "build" counsel-etags-ignore-directories)

  (defun my/counsel-etags-recenter (&rest _)
    (recenter))
  (advice-add 'counsel-etags-find-tag-at-point :after #'my/counsel-etags-recenter))


(require 'init-c)
(require 'init-elisp)

(provide 'init-prog)
