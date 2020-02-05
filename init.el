(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; ensure to use use-package
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))
(require 'use-package)

(use-package helm
   :ensure t)

(use-package helm-config
  :config
  (helm-mode t)
  :bind
  ("M-x" . 'helm-M-x)
  ("C-x b" . 'helm-mini)
  ("C-x C-f" . 'helm-find-files)
  ("C-x C-b" . 'helm-buffers-list)
  ("M-y" . 'helm-show-kill-ring)
  ("C-c C-f" . 'helm-find)
  ("C-x C-o" . 'helm-occur)
  )

(use-package helm-company
  :ensure t
  )

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  )

(use-package helm-flycheck
  :ensure t
  :config
  (eval-after-load 'flycheck
    '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck)))

(use-package helm-swoop
  :ensure t)

(use-package helm-gtags
  :ensure t
  :config
  (helm-gtags-mode t)
  (add-hook 'c-mode-hook 'gtags-mode)
  (add-hook 'c++-mode-hook 'gtags-mode)
  (add-hook 'go-mode-hook 'gtags-mode)
  (add-hook 'rust-mode-hook 'grags-mode)
  (setq helm-gtags-path-style 'root)
  (setq helm-gtags-ignore-case nil)
  :bind
  ("M-t" . 'helm-gtags-find-tag)
  ("M-r" . 'helm-gtags-find-rtag)
  ("M-s" . 'helm-gtags-find-symbol)
  ("C-t" . 'helm-gtags-pop-stack))

(use-package helm-ls-git
  :ensure t
  :bind
  ("C-x C-d" . 'helm-browse-project))

(use-package helm-descbinds
  :ensure t
  :bind
  ("C-x ?" . 'helm-descbinds))

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0)
  (setq company-echo-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  :bind
  (:map company-active-map
        ("M-n" . nil)
        ("M-p" . nil)
        ("C-n" . 'company-select-next)
        ("C-p" . 'company-select-previous)
        ("<tab>" . 'company-complete-selection))
  (:map company-search-map
        ("C-n" . 'company-select-next)
        ("C-p" . 'company-select-previous))
  )

(use-package company-c-headers
  :ensure t
  :config
  (add-hook 'company-backends 'company-c-headers)
  (add-hook 'company-c-headers-path-system "/usr/include/c++/7.4.0")
  (add-hook 'company-c-headers-path-system "/usr/include/"))

(use-package irony
  :ensure t
  :config
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :config
  (setq elpy-rpc-virtualenv-path 'current)
  (when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)))

(use-package tuareg
  :ensure t
  :config
  (setq tuareg-indent-align-with-first-arg t))

(use-package company-go
  :ensure t
  :config
  (add-to-list 'company-backends 'company-go))

(use-package beacon
  :ensure t
  :config
  (beacon-mode t))

(use-package git-gutter+
  :ensure t
  :config
  (global-git-gutter+-mode)
  (global-set-key (kbd "C-x g") 'git-gutter+-mode)
  (global-set-key (kbd "C-x G") 'global-git-gutter+-mode)
  :bind
  ("C-x n" . 'git-gutter+-next-hunk)
  ("C-x p" . 'git-gutter+-previous-hunk)
  ("C-x v" . 'git-gutter+-show-hunk)
  ("C-x r" . 'git-gutter+-revert-hunk)
  ("C-x t" . 'git-gutter+-stage-hunks)
  ("C-x c" . 'git-gutter+-commit)
  ("C-x C" . 'git-gutter+-stage-and-commit)
  ("C-x C-y" . 'git-gutter+-stage-and-commit-whole-buffer)
  ("C-x U" . 'git-gutter+-unstage-whole-buffer))

(use-package rust-mode
  :ensure t
  :config
  (setq company-tooltip-align-annotations t)
  ;; add path of racer, rustfmt
  ;;(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))
  ;;(add-hook 'rust-mode-hook (lambda()
  ;;                          (racer-mode))))
  )

(use-package racer
  :ensure t
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  )

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(use-package ace-window
  :ensure t
  :bind
  ("C-x o" . 'ace-window)
  :custom
  (aw-keys '(?j ?k ?l ?i ?o ?h ?y ?u ?p))
  :custom-face
  (aw-leading-char-face ((t (:height 4.0 :foreground "#f1fa8c"))))
  )

(use-package zoom-window
  :ensure t
  :bind
  ("C-x C-z" . 'zoom-window-zoom))

;; ~ don't create backup-file foo.txt~
(setq make-backup-files nil)
;; # don't create auto-save-file #foo.txt#
(setq auto-save-default nil)
;; don't create ~/.emacs.d/auto-save-list/
(setq auto-save-list-file-prefix nil)
;; use spaces over tabs
(setq-default tab-width 4 indent-tabs-mode nil)

(setq ring-bell-function 'ignore)

(set-default-coding-systems 'utf-8)

(menu-bar-mode -1)

;;line and column number
(column-number-mode t)
(global-linum-mode t)
;; automatically insert )
(electric-pair-mode t)

(show-paren-mode 1)

(setq inhibit-startup-message t)

(set-scroll-bar-mode 'right)

(put 'upcase-region 'disabled nil)

(load-theme #'rebecca t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-keys (quote (106 107 108 105 111 104 121 117 112)))
 '(package-selected-packages
   (quote
    (helm-swoop elpy helm-config zoom-window use-package tuareg rtags rebecca-theme racer helm-ls-git helm-gtags helm-flycheck helm-descbinds git-gutter+ flycheck-irony dashboard company-jedi company-irony company-go company-c-headers beacon ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:height 4.0 :foreground "#f1fa8c")))))
