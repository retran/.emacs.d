(setq inhibit-splash-screen t
      initial-scratch-message nil
      ring-bell-function 'ignore)

(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-frame-font "Consolas 12")
(blink-cursor-mode 1)
(set-default 'cursor-type 'bar)
(fset 'yes-or-no-p 'y-or-n-p)
(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))
(column-number-mode t)
(global-linum-mode t)
(global-hl-line-mode)
(setq hl-line-sticky-flag nil)
(setq show-paren-style 'parenthesis)
(show-paren-mode 2)

(use-package smart-mode-line
  :ensure t
  :init
  (progn
    (rich-minority-mode 1)
    (setf rm-blacklist "")
    (setq sml/no-confirm-load-theme t)
    (sml/setup)))

(use-package color-theme
  :ensure t)

(use-package gotham-theme
  :ensure t)

(defadvice load-theme (before theme-dont-propagate activate)
 (mapc #'disable-theme custom-enabled-themes))

(defun remacs-load-theme (theme)
  (load-theme theme t)
  )

(defun remacs-light-theme ()
  (interactive)
  (remacs-load-theme 'leuven))

(defun remacs-dark-theme ()
  (interactive)
  (remacs-load-theme 'gotham))

(remacs-dark-theme)

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t))

(use-package counsel
  :ensure t)

(use-package evil
  :ensure t
  :init
  (evil-mode 1)
  (setq sentence-end-double-space nil))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (which-key-setup-minibuffer)
  (which-key-mode))

(use-package winum
  :ensure t
  :init
  (winum-mode)
  (winner-mode))

(use-package linum-relative
  :ensure t)

(use-package git-gutter-fringe+
  :ensure t
  :diminish git-gutter+-mode
  :config
  (setq git-gutter-fr+-side 'right-fringe)
  (git-gutter-fr+-minimal)
  :init
  (global-git-gutter+-mode))
