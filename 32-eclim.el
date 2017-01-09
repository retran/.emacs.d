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
