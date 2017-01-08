(diminish 'eldoc-mode)

(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(add-hook 'lisp-interaction-mode-hook 'company-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-elisp))
