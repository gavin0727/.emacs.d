;; show TAB
(use-package whitespace
  :ensure nil
  :hook (prog-mode . whitespace-mode)
  :config
  (setq whitespace-style '(face tabs tab-mark trailing))
  (setq backward-delete-char-untabify-method 'nil)
  (custom-set-faces
   '(whitespace-tab ((t (:foreground "#636363" :background "#303030")))))
  (setq whitespace-display-mappings
        '((tab-mark 9 [124 9] [92 9]))))

(use-package flycheck
  :ensure t
  :config
  (setq truncate-lines nil)
  :hook
  (c-mode . flycheck-mode)
  (c++-mode . flycheck-mode))

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

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0
        company-tooltip-align-annotations t))

(use-package lsp-mode
  :ensure t
  :hook ((c-mode c++-mode) . lsp-deferred)
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil
        lsp-enable-snippet nil
        lsp-idle-delay 0.2
        lsp-clients-clangd-args '("--header-insertion=never"
                                  "--cross-file-rename")
        lsp-enable-on-type-formatting nil
        lsp-enable-indentation nil
        lsp-enable-symbol-highlighting nil
        lsp-headerline-breadcrumb-icons-enable nil))

(require 'init-c)
(require 'init-elisp)

(provide 'init-prog)
