(require 'package)

;; add MELPA repo
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defvar favorite-packages
  '(
    ;; helm
    helm
    ;; flychake
    flycheck
    ;; flycheck-irony
    flycheck-irony
    ;; rtags
    rtags
    ;; irony
    irony
    ;; company-irony
    company-irony
    ;; company
    company
    ;; company-c-headers
    company-c-headers
    ;; tuareg
    tuareg
    ;; jedi-core
    jedi-core
    ;; company-jedi
    company-jedi
    ;; helm-flychecl
    helm-flycheck
    ;; helm-gtags
    helm-gtags
    ;; zoom
    zoom
    ;; perl6
    perl6-mode
    ;;beacon
    beacon
    ;; use-package
    use-package
    ))

;;
(dolist (package favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; use-package-settings
(eval-when-compile
  (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

;; helm-settings
(use-package helm-config
  :config
  (helm-mode t)
  ;;(setq rtags-display-result-backend 'helm)
  (setq helm-move-to-line-cycle-in-source t
        helm-ff-search-library-in-sexp t
        helm-scroll-amount 8
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line t)
  :bind
  ("M-x" . 'helm-M-x)
  ("C-x b" . 'helm-mini)
  ("C-x C-f" . 'helm-find-files)
  ("C-x C-b" . 'helm-buffers-list))

;; helm-flycheck-settings 
(use-package helm-flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; flycheck-settings
;;(use-package flycheck
;;  :config
;;  (add-hook 'flycheck-mode-hook #'global-flycheck-mode))

;; company-settings
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-h") nil))

;; company-c-headers
(use-package company-c-headers
  :config
  (add-hook 'company-backends 'company-c-headers)
  (add-hook 'company-c-headers-path-system "/usr/include/c++/5.4.0/"))

;; irony-settings
(use-package irony
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; tuareg-settings
(use-package tuareg
  :config
  (setq tuareg-indent-align-with-first-arg t))

;; jedi-core-settings
(use-package jedi-core
  :config
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t)
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-to-list 'company-backends 'company-jedi))

;;(use-package perl6-mode
;;  :ensure t
;;  :defer t)

(use-package beacon
  :config
  (beacon-mode t))

(use-package helm-gtags
  :config
  (helm-gtags-mode t))

(use-package zoom
  :config
  (zoom-mode t))

;;refresh package archive
;;(package-refresh-contents)

;; ~ don't create backup-file foo.txt~
(setq make-backup-files nil)
;; # don't create auto-save-file #foo.txt#
(setq auto-save-default nil)
;; don't create ~/.emacs.d/auto-save-list/
(setq auto-save-list-file-prefix nil)
;; use spaces over tabs
(setq-default tab-width 4 indent-tabs-mode nil)

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
    (company-jedi zoom helm-gtags beacon rebecca-theme use-package perl6-mode helm-flycheck jedi-core tuareg company-c-headers company-irony rtags flycheck-irony flycheck helm))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
