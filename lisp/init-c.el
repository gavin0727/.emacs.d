(require 'init-funcs)

(use-package c++-ts-mode
    :ensure nil ;; emacs built-in
    :config
    (setq c-ts-mode-indent-offset custom-tab-width)
    :hook
    ((c-ts-mode c++-ts-mode) . enable-tabs))

(provide 'init-c)
