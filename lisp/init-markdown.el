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

(provide 'init-markdown)
