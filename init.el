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
(setq show-paren-style 'parenthesis)
(show-paren-mode 2)

(use-package color-theme
  :ensure t
  :config
  (use-package gotham-theme
    :ensure t
    :config
    (progn
      (use-package powerline
        :ensure t
        :config
        (powerline-center-evil-theme))
      
      (defun remacs|load-theme (theme)
        (load-theme theme t)
        (powerline-reset))
    
      (defun remacs|light-theme ()
        (interactive)
        (remacs|load-theme 'leuven))
    
      (defun remacs|dark-theme ()
        (interactive)
        (remacs|load-theme 'gotham))

      (general-define-key
       :states '(normal visual)
       :prefix "SPC"
       "Tl" 'remacs|light-theme
       "Td" 'remacs|dark-theme)
      
      (remacs|dark-theme))))


(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (progn
    (setq ivy-use-virtual-buffers t)
    (use-package counsel
      :ensure t)
    (ivy-mode t)))

(use-package evil
  :ensure t
  :config
  (progn
    (setq sentence-end-double-space nil)
    (evil-mode t)))

(use-package avy
  :ensure t)

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
    (general-define-key
     :states '(normal visual)
     :prefix "SPC"
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
     "wd" 'delete-window
     "wv" 'split-window-vertically
     "wh" 'split-window-horizontally
     "wm" 'delete-other-windows
     "wb" 'balance-windows
     "wu" 'winner-undo
     "wr" 'winner-redo)
    (winum-mode)
    (winner-mode)))

(use-package linum
  :ensure t
  :config
  (progn
    (use-package linum-relative
      :ensure t)))


(setq-default indent-tabs-mode nil)

(use-package general
  :ensure t
  :config
  (general-evil-setup t)
  (general-define-key
   :states '(normal visual)
   :prefix "SPC"
   
   ":" 'counsel-M-x
   "/" 'swiper

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

   ;; toggle
   "tn" 'linum-mode
   "tw" 'whitespace-mode
   "tr" 'linum-relative-toggle
   "tg" 'git-gutter+-mode

   ;; navigation
   "gc" 'avy-goto-char
   "gl" 'avy-goto-line
   "SPC" 'avy-goto-word-1
   "gb" 'pop-global-mark
   )
)

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

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (global-flycheck-mode))

(use-package projectile
  :ensure t
  :config
  (progn
    (projectile-mode t)
    (use-package counsel-projectile
      :ensure t)))

;; configure git modes
(use-package magit
  :ensure t
  :config
  (progn
    (use-package evil-magit
      :ensure t)
    (general-define-key
     :states '(normal visual)
     :prefix "SPC"
     "vv" 'magit-status
     "vb" 'magit-blame
     "vl" 'magit-log-current)
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

;; org-mode configuration



;; org-evil !!!


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
                  (list nil file nil pos))))

  (defun remacs|refile-to-backlog ()
    (interactive)
    (org-mark-ring-push)
    (remacs|refile-to remacs|backlog-file)
    (org-mark-ring-goto))

  (defun remacs|refile (&optional date)
    (interactive)
    (org-mark-ring-push)
    (remacs|open-log-entry date)
    (org-mark-ring-goto)
    (org-mark-ring-push)
    (remacs|refile-to (remacs|filename-for-date
                       (if date date (calendar-current-date))))
    (org-mark-ring-goto))

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
        (remacs|insert-log-entry-header date))))

  (defun remacs|open-log-entry-to-specified-date ()
    (interactive)
    (remacs|open-log-entry (remacs|prompt-for-date)))
  
  (general-define-key
   :states '(normal visual)
   :prefix "SPC"
   "aa" 'remacs|open-log-entry
   "ad" 'remacs|open-log-entry-to-specified-date
   "ab" 'remacs|open-backlog)

  (general-define-key
   :keymaps 'org-mode-map
   :states '(normal visual)
   :prefix "SPC"
   "arb" 'remacs|refile-to-backlog
   "arr" 'remacs|refile
   "ard" 'remacs|refile-to-specified-date))

;; elisp-mode configuration
(diminish 'eldoc-mode)

(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(add-hook 'lisp-interaction-mode-hook 'company-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-elisp))

;; java-mode configuration
(use-package eclim
  :ensure t
  :init
  (custom-set-variables
   '(eclim-eclipse-dirs '("~/eclipse"))
   '(eclim-executable "~/eclipse/eclim"))
  :config
  (progn
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
    
    (setq help-at-pt-display-when-idle t
          help-at-pt-timer-delay 0.1)
    (help-at-pt-set-timer)
    (global-eclim-mode)
    (evil-define-key 'insert java-mode-map
      (kbd ".") 'spacemacs/java-completing-dot
      (kbd ":") 'spacemacs/java-completing-double-colon)
    (general-define-key
     :keymaps 'eclim-mode-map
     :states '(normal visual)
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
     "Ff" 'eclim-java-format)
    (use-package company-emacs-eclim
      :ensure t
      :config
      (company-emacs-eclim-setup)
      (add-hook 'java-mode-hook 'company-mode))))

(provide 'init)
;;; init.el ends here
