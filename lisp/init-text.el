;; Pixel alignment for org/markdown tables
(use-package valign
  :ensure t
  :hook ((markdown-mode org-mode) . valign-mode))

;; The markdown mode is awesome! unbeatable
(use-package markdown-mode
  :ensure t
  :mode ("README\\(?:\\.md\\)?\\'" . gfm-mode)
  :hook (markdown-mode . visual-line-mode)
  :custom
  (markdown-header-scaling t)
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-gfm-uppercase-checkbox t)
  (markdown-enable-prefix-prompts nil)
  (markdown-fontify-code-blocks-natively t))

(use-package org
  :ensure nil
  :hook (org-mode . visual-line-mode)
  :custom
  ;; prettify
  (org-startup-indented t)
  (org-fontify-todo-headline nil)
  (org-fontify-done-headline t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  (org-list-demote-modify-bullet '(("+" . "-") ("1." . "a.") ("-" . "+")))
  ;; image
  (org-image-actual-width nil)
  ;; more user-friendly
  (org-imenu-depth 4)
  (org-clone-delete-id t)
  (org-use-sub-superscripts '{})
  (org-yank-adjusted-subtrees t)
  (org-ctrl-k-protect-subtree 'error)
  (org-fold-catch-invisible-edits 'show-and-error)
  ;; call C-c C-o explicitly
  (org-return-follows-link nil)
  ;; todo
  (org-todo-keywords '((sequence "TODO(t)" "HOLD(h!)" "WIP(i!)" "WAIT(w!)" "|" "DONE(d!)" "CANCELLED(c@/!)")))
  (org-todo-keyword-faces '(("TODO"       :foreground "#7c7c75" :weight bold)
                            ("HOLD"       :foreground "#feb24c" :weight bold)
                            ("WIP"        :foreground "#0098dd" :weight bold)
                            ("WAIT"       :foreground "#9f7efe" :weight bold)
                            ("DONE"       :foreground "#50a14f" :weight bold)
                            ("CANCELLED"  :foreground "#ff6480" :weight bold)))
  (org-use-fast-todo-selection 'expert)
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-priority-faces '((?A :foreground "red")
                        (?B :foreground "orange")
                        (?C :foreground "yellow")))
  (org-global-properties '(("EFFORT_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00")
                           ("APPT_WARNTIME_ALL" . "0 5 10 15 20 25 30 45 60")
                           ("STYLE_ALL" . "habit")))
  (org-columns-default-format "%25ITEM %TODO %SCHEDULED %DEADLINE %3PRIORITY %TAGS %CLOCKSUM %EFFORT{:}")
  ;; Remove CLOSED: [timestamp] after switching to non-DONE states
  (org-closed-keep-when-no-todo t)
  ;; log
  (org-log-repeat 'time)
  (org-log-done 'time)
  (org-ellipsis "⤵")
  ;; refile
  (org-refile-use-cache nil)
  (org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  ;; goto. We use minibuffer to filter instead of isearch.
  (org-goto-auto-isearch nil)
  (org-goto-interface 'outline-path-completion)
  ;; tags, e.g. #+TAGS: keyword in your file
  (org-use-fast-tag-selection t)
  (org-fast-tag-selection-single-key t)
  ;; archive
  (org-archive-location "%s_archive::datetree/")
  ;; id
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

;; Keep track of tasks
(use-package org-agenda
  :ensure nil
  :hook (org-agenda-finalize . org-agenda-to-appt)
  :custom
  (org-agenda-insert-diary-extract-time t)
  (org-agenda-start-on-weekday 0)
  (org-agenda-inhibit-startup t)
  (org-agenda-time-leading-zero t)
  (org-agenda-columns-add-appointments-to-effort-sum t)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-window-setup 'current-window))

;; Write codes in org-mode
(use-package org-src
  :ensure nil
  :hook (org-babel-after-execute . org-redisplay-inline-images)
  :custom
  (org-confirm-babel-evaluate nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'other-window)
  (org-src-lang-modes '(("C"      . c)
                        ("C++"    . c++)
                        ("bash"   . sh)
                        ("cpp"    . c++)
                        ("dot"    . graphviz-dot) ;; was `fundamental-mode'
                        ("elisp"  . emacs-lisp)
                        ("ocaml"  . tuareg)
                        ("shell"  . sh)))
  (org-babel-load-languages '((C          . t)
                              (dot        . t)
                              (emacs-lisp . t)
                              (eshell     . t)
                              (python     . t)
                              (shell      . t))))

(provide 'init-text)
