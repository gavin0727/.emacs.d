(require 'init-funcs)

(use-package elisp-mode
  :ensure nil
  :hook
  ((lisp-mode emacs-lisp-mode) . disable-tabs))

(provide 'init-elisp)

