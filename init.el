;;; init.el --- Personal emacs configuration of Andrew Vasilyev

;;; Commentary:

;;; Code:

;(package-initialize)

;; create customization file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; bootstrap use-package
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      (append package-archives
	      '(("melpa" . "http://melpa.org/packages/")
		("marmalade" . "http://marmalade-repo.org/packages/")
		("gnu" . "http://elpa.gnu.org/packages/")
		("elpy" . "http://jorgenschaefer.github.io/packages/"))))

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; don't backup files
(setq make-backup-files nil)
(setq auto-save-list-file-name nil)
(setq auto-save-default nil)

;; use utf-8 by default
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(use-package exec-path-from-shell
	     :ensure t
	     :config (exec-path-from-shell-initialize))

(setq inhibit-splash-screen t
      initial-scratch-message nil
      ring-bell-function 'ignore)

(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 120))

(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-frame-font "Consolas 14")
(blink-cursor-mode 1)
(set-default 'cursor-type 'bar)
(fset 'yes-or-no-p 'y-or-n-p)
(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))
(column-number-mode t)
(global-linum-mode t)
(global-hl-line-mode)
(show-paren-mode 2)
(setq show-paren-style 'parenthesis)

(use-package color-theme
  :ensure t
  :config
  (defadvice load-theme (before theme-dont-propagate activate)
    (mapc #'disable-theme custom-enabled-themes))
  (use-package gotham-theme
    :ensure t
    :config
    (progn
      (use-package powerline
        :ensure t
        :config
        (powerline-center-theme))
      (defun remacs|load-theme (theme)
        (load-theme theme t)
        (powerline-reset))
      (defun remacs|light-theme ()
        (interactive)
        (remacs|load-theme 'leuven))
      (defun remacs|dark-theme ()
        (interactive)
        (remacs|load-theme 'gotham))
      (remacs|dark-theme))))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (progn
    (setq ivy-use-virtual-buffers t)
    (use-package counsel
      :ensure t)
    (ivy-mode t)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-s") 'swiper)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-find-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)))

;; hydra?
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (progn
    (which-key-setup-minibuffer)
    (which-key-mode)))

(use-package winum
  :ensure t
  :config
  (progn
    (winum-mode)
    (winner-mode)))

(use-package linum
  :ensure t)

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config))

(use-package rainbow-delimiters
  :ensure t)

(setq-default indent-tabs-mode nil)

;; comapnymode
(use-package company
  :ensure t
  :diminish company-mode
  :config
  (progn
    (setq company-require-match nil)
    (use-package company-quickhelp
      :ensure t
      :config
      (company-quickhelp-mode 1))))

;; yasnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

;; flycheck
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (progn
    (global-flycheck-mode)))

;; projectile
(use-package projectile
  :ensure t
  :config
  (progn
    (projectile-mode t)
    (use-package ggtags
      :ensure t)
    (use-package counsel-projectile
      :ensure t
      :config
      (counsel-projectile-on))))

;; elisp-mode configuration
(diminish 'eldoc-mode)

(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)

(add-hook 'lisp-interaction-mode-hook 'company-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook #'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook #'smartparens-mode)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-elisp))

;; configure git modes
(use-package magit
  :ensure t
  :config
  (progn
    (use-package evil-magit
      :ensure t)
    (use-package git-gutter-fringe+
      :ensure t
      :diminish git-gutter+-mode
      :config
      (progn
        (setq git-gutter-fr+-side 'right-fringe)
        (git-gutter-fr+-minimal)
        (global-git-gutter+-mode)))
    (use-package git-auto-commit-mode
      :ensure t
      :config
      (setq gac-ask-for-summary-p nil))))

;; clojure
(use-package cider
  :ensure t
  :config
  (progn
    (use-package clojure-cheatsheet
      :ensure t
      :config
      (define-key clojure-mode-map (kbd "C-c C-h") #'clojure-cheatsheet))
    (add-hook 'cider-repl-mode-hook #'company-mode)
    (add-hook 'cider-mode-hook #'company-mode)
    (add-hook 'cider-repl-mode-hook #'smartparens-mode)
    (add-hook 'cider-mode-hook #'smartparens-mode)
    (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'cider-mode-hook #'rainbow-delimiters-mode)))

;; TODO reconfugure below

;; org-mode configuration

(use-package org
  :ensure t
  :init
  (setq remacs|log-path (expand-file-name "~/notes/log/"))
  (setq remacs|backlog-file (expand-file-name "~/notes/log/backlog.org"))
  (setq org-agenda-files `(,remacs|log-path))
  (setq org-refile-targets
      '((nil :maxlevel . 1)
        (org-agenda-files :maxlevel . 1)))
  (setq org-refile-use-outline-path 'file)
  (setq org-default-notes-file remacs|backlog-file)
  (setq org-capture-templates
        '(("t" "TODO" entry (file remacs|backlog-file)
           "* TODO %?\n  %i\n")))
  :config
  (defun remacs|reorder-date (date)
    (list (nth 2 date)
          (nth 0 date)
          (nth 1 date)))
  (defun remacs|filename-for-date (date)
    (concat remacs|log-path
            (format "L%s.org"
                    (string-join
                     (mapcar
                      (lambda (i) (format "%02d" i))
                      (remacs|reorder-date date))
                     ""))))
  (defun remacs|format-date (date)
    (string-join (mapcar (lambda (i) (format "%02d" i)) date) "-"))
  (defun remacs|log-entry-header (date)
    (let ((date-short (remacs|format-date (remacs|reorder-date date)))
          (date-long (calendar-date-string date)))
      (format
       "#+TITLE: %s\n#+AUTHOR: %s\n#+DATE: %s\n#+FILETAGS: log\n\n"
       date-long
       user-full-name
       date-short)))
  (defun remacs|insert-log-entry-header (&optional date)
    (interactive)
    (insert
     (remacs|log-entry-header
      (if date date (calendar-current-date)))))
  (defun remacs|prompt-for-date ()
    (let ((decoded
           (decode-time
            (org-time-string-to-time (org-read-date)))))
      (list (nth 4 decoded)
            (nth 3 decoded)
            (nth 5 decoded))))
  (defun remacs|insert-log-entry-header-with-specified-date ()
    (interactive)
    (remacs|insert-log-entry-header (remacs|prompt-for-date)))
  (defun remacs|refile-to (file)
    (let ((pos (save-excursion
                 (find-file file)
                 (end-of-buffer))))
      (org-refile nil nil
                  (list nil file nil pos))
      (save-buffer)))
  (defun remacs|refile-to-backlog ()
    (interactive)
    (org-schedule '(4))
    (org-mark-ring-push)
    (remacs|refile-to remacs|backlog-file)
    (org-mark-ring-goto)
    (save-buffer))
  (defun remacs|refile (&optional date)
    (interactive)
    (let ((d (if date date (calendar-current-date))))
      (org-schedule
       t (remacs|format-date (remacs|reorder-date d)))
      (org-mark-ring-push)
      (remacs|open-log-entry d)
      (org-mark-ring-goto)
      (org-mark-ring-push)
      (remacs|refile-to
       (remacs|filename-for-date d))
      (org-mark-ring-goto)
      (save-buffer)))
  (defun remacs|refile-to-specified-date ()
    (interactive)
    (remacs|refile (remacs|prompt-for-date)))
  (defun remacs|open-backlog ()
    (interactive)
    (find-file remacs|backlog-file))
  (defun remacs|open-log-entry (&optional date)
    (interactive)
    (let* ((filename
           (remacs|filename-for-date
            (if date date (calendar-current-date))))
           (new-file (not (file-exists-p filename))))
      (find-file filename)
      (when new-file
        (remacs|insert-log-entry-header date)
        (save-buffer))))
  (defun remacs|open-log-entry-to-specified-date ()
    (interactive)
    (remacs|open-log-entry (remacs|prompt-for-date)))
  (defun remacs|capture-task ()
    (interactive)
    (org-capture nil "t")))

;; typescript
(use-package tide
  :ensure t
  :config
  (progn
    (defun setup-tide-mode ()
      (interactive)
  (tide-setup)
   (flycheck-mode +1)
   (setq flycheck-check-syntax-automatically '(save mode-enabled))
   (eldoc-mode +1)
   (tide-hl-identifier-mode +1)
   (company-mode +1))
    (setq company-tooltip-align-annotations t)
    (add-hook 'before-save-hook 'tide-format-before-save)
    (add-hook 'typescript-mode-hook #'setup-tide-mode)
    (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t
                                :placeOpenBraceOnNewLineForFunctions nil))))

(use-package ng2-mode
  :ensure t)

;; markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(provide 'init)

;;; init.el ends here

