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
