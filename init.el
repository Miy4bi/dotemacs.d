(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defvar favorite-packages
  '(
    helm
    flycheck
    flycheck-irony
    rtags
    irony
    company-irony
    company
    company-c-headers
    tuareg
    jedi-core
    company-jedi
    helm-flycheck
    helm-gtags
    helm-ls-git
    zoom
    beacon
    rebecca-theme
    use-package
    ;; git
    git-gutter+
    ;; rust settings
    rust-mode
    racer
    ;; go settings
    go-mode
    company-go
    ))

(dolist (package favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; use-package-settings
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(use-package helm-config
  :config
  (helm-mode t)
  :bind
  ("M-x" . 'helm-M-x)
  ("C-x b" . 'helm-mini)
  ("C-x C-f" . 'helm-find-files)
  ("C-x C-b" . 'helm-buffers-list)
  ("M-y" . 'helm-show-kill-ring)
  ("C-c f" . 'helm-find)
  ("C-s" . 'helm-occur))

(use-package helm-flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;;(use-package flycheck
;;  :config
;;  (add-hook 'flycheck-mode-hook #'global-flycheck-mode))

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0)
  (setq company-echo-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-h") nil))

(use-package company-c-headers
  :config
  (add-hook 'company-backends 'company-c-headers)
  (add-hook 'company-c-headers-path-system "/usr/include/c++/7.4.0")
  (add-hook 'company-c-headers-path-system "/usr/include/"))

(use-package irony
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package tuareg
  :config
  (setq tuareg-indent-align-with-first-arg t))

(use-package jedi-core
  :config
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t)
  (add-to-list 'company-backends 'company-jedi))

(use-package company-go
  :config
  (add-to-list 'company-backends 'company-go))

(use-package beacon
  :config
  (beacon-mode t))

(use-package helm-gtags
  :config
  (helm-gtags-mode t))

(use-package helm-ls-git
  :bind
  ("C-c l" . 'helm-browse-project))

(use-package zoom
  :config
  (zoom-mode t))

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
  ("C-x r" . 'git-gutter+-revert-hunks)
  ("C-x t" . 'git-gutter+-stage-hunks)
  ("C-x c" . 'git-gutter+-commit)
  ("C-x C" . 'git-gutter+-stage-and-commit)
  ("C-x C-y" . 'git-gutter+-stage-and-commit-whole-buffer)
  ("C-x U" . 'git-gutter+-unstage-whole-buffer))

(use-package rust-mode
  :config
  (setq company-tooltip-align-annotations t)
  ;; add path of racer, rustfmt
  (add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))
  (add-hook 'rust-mode-hook (lambda()
                            (racer-mode))))

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


(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(custom-enabled-themes (quote (rebecca)))
 '(custom-safe-themes
   (quote
    ("f633d825e380caaaefca46483f7243ae9a663f6df66c5fad66d4cab91f731c86" default)))
 '(package-selected-packages
   (quote
    (go-mode py-autopep8 company-jedi zoom helm-gtags beacon rebecca-theme use-package perl6-mode helm-flycheck jedi-core tuareg company-c-headers company-irony rtags flycheck-irony flycheck helm))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
