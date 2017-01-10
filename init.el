;;; init.el --- Personal emacs configuraiton of Andrew Vasilyev

;;; Commentary:

;;; Code:

;(package-initialize)

;; create customization file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
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

(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-frame-font "Consolas 13")
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

(use-package avy
  :ensure t)

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


(setq-default indent-tabs-mode nil)

(use-package general :ensure t
  :config
  (general-evil-setup t)
  (general-define-key
   :states '(normal)
   :prefix "SPC"
   
   ":" 'counsel-M-x
   "/" 'swiper

   ;; winum
   "0" 'winum-select-window-0-or-10
   "1" 'winum-select-window-1
   "2" 'winum-select-window-2
   "3" 'winum-select-window-3
   "4" 'winum-select-window-4
   "5" 'winum-select-window-5
   "6" 'winum-select-window-6
   "7" 'winum-select-window-7
   "8" 'winum-select-window-8
   "9" 'winum-select-window-9
   
   ;; file
   "ff" 'counsel-find-file
   "fp" 'counsel-projectile
   "fS" 'projectile-save-project-buffers
   "fc" 'projectile-switch-project
   "fd" 'projectile-dired

   ;; search
   "ss" 'counsel-projectile-ag

   ;; formatting
   "Fw" 'whitespace-cleanup
   
   ;; buffer
   "bd" 'kill-this-buffer
   "bb" 'ivy-switch-buffer
   "bn" 'next-buffer
   "bp" 'previous-buffer
   "<tab>" 'mode-line-other-buffer

   ;; window
   "wd" 'delete-window
   "wv" 'split-window-vertically
   "wh" 'split-window-horizontally
   "wm" 'delete-other-windows
   "wb" 'balance-windows
   "wu" 'winner-undo
   "wr" 'winner-redo

   ;; toggle
   "tn" 'linum-mode
   "tw" 'whitespace-mode
   "tr" 'linum-relative-toggle
   "tg" 'git-gutter+-mode

   ;; themes
   "Tl" 'remacs-light-theme
   "Td" 'remacs-dark-theme

   ;; navigation
   "gc" 'avy-goto-char
   "gl" 'avy-goto-line
   "SPC" 'avy-goto-word-1
   "gb" 'pop-global-mark

   ;; vcs
   "vv" 'magit-status
   "vb" 'magit-blame
   "vl" 'magit-log-current
   )
)

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (setq company-require-match nil))

(use-package company-quickhelp
  :ensure t
  :init
  (company-quickhelp-mode 1))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package projectile
  :ensure t
  :init
  (projectile-global-mode t))

(use-package counsel-projectile
  :ensure t)

(use-package magit
  :ensure t)

(use-package evil-magit
  :ensure t)

(diminish 'eldoc-mode)

(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(add-hook 'lisp-interaction-mode-hook 'company-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-elisp))

(custom-set-variables
  '(eclim-eclipse-dirs '("~/eclipse"))
  '(eclim-executable "~/eclipse/eclim"))

;; https://github.com/syl20bnr/spacemacs/blob/master/layers/%2Blang/java/funcs.el
(defun spacemacs/java-completing-dot ()
  "Insert a period and show company completions."
  (interactive "*")
  (spacemacs//java-delete-horizontal-space)
  (insert ".")
  (company-emacs-eclim 'interactive))

;; https://github.com/syl20bnr/spacemacs/blob/master/layers/%2Blang/java/funcs.el
(defun spacemacs/java-completing-double-colon ()
  "Insert double colon and show company completions."
  (interactive "*")
  (spacemacs//java-delete-horizontal-space)
  (insert ":")
  (let ((curr (point)))
    (when (s-matches? (buffer-substring (- curr 2) (- curr 1)) ":")
      (company-emacs-eclim 'interactive))))

;; https://github.com/syl20bnr/spacemacs/blob/master/layers/%2Blang/java/funcs.el
(defun spacemacs//java-delete-horizontal-space ()
  (when (s-matches? (rx (+ (not space)))
                    (buffer-substring (line-beginning-position) (point)))
    (delete-horizontal-space t)))

(add-hook 'java-mode-hook 'company-mode)

(use-package eclim
  :ensure t
  :init
  (global-eclim-mode)
  :config
  (setq help-at-pt-display-when-idle t
        help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer)
  (evil-define-key 'insert java-mode-map
    (kbd ".") 'spacemacs/java-completing-dot
    (kbd ":") 'spacemacs/java-completing-double-colon))

(use-package company-emacs-eclim
  :ensure t
  :init
  (company-emacs-eclim-setup))

(general-define-key
 :keymaps 'eclim-mode-map
 :states '(normal)
 :prefix "SPC"

 ;; navigation
 "gg" 'eclim-java-find-declaration
 "gs" 'eclim-java-find-generic
 "gr" 'eclim-java-find-references
 "gt" 'eclim-java-find-type
 "gc" 'eclim-java-call-hierarchy
 "gh" 'eclim-java-hierarchy
 "gm" 'counsel-imenu

 ;; refactor
 "rr" 'eclim-java-refactor-rename-symbol-at-point
 "rm" 'eclim-java-refactor-move-class
 "ri" 'eclim-java-import-organize
 "re" 'eclim-java-implement
 "rg" 'eclim-java-generate-getter
 "rs" 'eclim-java-generate-setter
 "rb" 'eclim-java-generate-getter-and-setter
 "rc" 'eclim-java-constructor
 
 ;; problems
 "cc" 'eclim-problems-correct
 "ca" 'eclim-problems

 ;; formatting
 "Ff" 'eclim-java-format
 )

(provide 'init)
