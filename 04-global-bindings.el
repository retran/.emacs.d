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
