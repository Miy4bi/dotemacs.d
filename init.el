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
    ;; helm-ls-git
    helm-ls-git
    ;; helm-git-grep
    helm-git-grep
    ;; symbol-overlay
    symbol-overlay
    ;; zoom
    zoom
    ;; beacon
    beacon
    ;; magit
    magit
    ;; markdonw-mode
    markdown-mode
    ;; rust-mode
    rust-mode
    ;; rebecca-theme
    rebecca-theme
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
  :bind
  ("M-x" . 'helm-M-x)
  ("C-x b" . 'helm-mini)
  ("C-x C-f" . 'helm-find-files)
  ("C-x C-b" . 'helm-buffers-list)
  ("M-y" . 'helm-show-kill-ring)
  ("C-c m a n" . 'helm-man-woman)
  ("C-c f" . 'helm-find)
  ("C-c o" . helm-occur))


;; helm-flycheck-settings
(use-package helm-flycheck
  :hook
  ('after-init-hook #'global-flycheck-mode))

;; flycheck-settings
;; (use-package flycheck
;;  :config
;;  (add-hook 'flycheck-mode-hook #'global-flycheck-mode))

;; company-settings
(use-package company
  :init
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  :config
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-h") nil)
  :hook
  ('after-init-hook 'global-company-mode))

  

;; company-c-headers
(use-package company-c-headers
  :hook
  ('company-backends 'company-c-headers)
  ('company-c-headers-path-system "/usr/include/c++/7.3.0/"))

;; irony-settings
(use-package irony
  :hook
  ('c++-mode-hook 'irony-mode)
  ('c-mode-hook 'irony-mode)
  ('irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; tuareg-settings
(use-package tuareg
  :init
  (setq tuareg-indent-align-with-first-arg t))

;; jedi-core-settings
(use-package jedi-core
  :init
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t)
  :hook
  ('python-mode-hook 'jedi:setup)
  :config
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

(use-package helm-ls-git
  :bind
  ("C-c l" . 'helm-browse-project)
  )

(use-package helm-git-grep
  :bind
  ("C-c g" . 'helm-git-grep))

(use-package symbol-overlay
  :config
  (global-set-key (kbd "M-i") 'symbol-overlay-put)
  (global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
  (global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
  (global-set-key (kbd "C-g") 'symbol-overlay-remove-all))

(use-package zoom
  :config
  (zoom-mode t))

(use-package magit
  :bind
  ("C-x g" . 'magit-status))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t))

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
    (rust-mode markdown-mode magit company-jedi zoom helm-gtags beacon rebecca-theme use-package perl6-mode helm-flycheck jedi-core tuareg company-c-headers company-irony rtags flycheck-irony flycheck helm))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
