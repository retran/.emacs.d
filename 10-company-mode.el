(use-package company
  :ensure t
  :diminish company-mode
  :config
  (setq company-require-match nil))

(use-package company-quickhelp
  :ensure t
  :init
  (company-quickhelp-mode 1))
