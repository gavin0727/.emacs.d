(require 'init-funcs)

(use-package cc-mode
  :ensure nil
  :init
  (setq-default c-basic-offset custom-tab-width)
  :hook
  ((c-mode c++-mode) . enable-tabs))

(provide 'init-c)
