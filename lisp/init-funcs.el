;; custom tab width
(setq custom-tab-width 4)

;; Two callable functions for enabling/disabling tabs in Emacs
(defun disable-tabs ()
  (setq indent-tabs-mode nil))

(defun enable-tabs ()
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width))

(provide 'init-funcs)
