;;
;; Speed up Startup Process
;;

;; Optimize Garbage Collection for Startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Optimize `auto-mode-alist`
(setq auto-mode-case-fold nil)

(setq read-process-output-max (* 4 1024 1024))

(setq debug-on-error init-file-debug)

(setq package-archives
      '(("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("org" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

(require 'package)
(setq package-check-signature nil)

(unless (bound-and-true-p package--initialized)
    (package-initialize))

(unless package-archive-contents
    (package-refresh-contents))

(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-enable-imenu-support t
      use-package-expand-minimally t)

(require 'use-package)

(add-to-list 'load-path
             (expand-file-name (concat user-emacs-directory "lisp")))

(setq custom-file (locate-user-emacs-file "custom.el"))

(use-package no-littering
  :ensure t
  :demand t)

(require 'init-base)
(require 'init-tools)
(require 'init-ui)
(require 'init-vcs)
(require 'init-edit)
(require 'init-window)
(require 'init-dired)
(require 'init-prog)
(require 'init-terminal)
(require 'init-markdown)
(require 'init-org)

(when (file-exists-p custom-file)
  (load custom-file))
