;; ~ don't create backup-file foo.txt~
(setq make-backup-files nil)
;; don't create ~/.emacs.d/auto-save-list/
(setq auto-save-list-file-prefix nil)
;; use spaces over tabs
(setq-default tab-width 4 indent-tabs-mode nil)

(setq ring-bell-function 'ignore)

(set-default-coding-systems 'utf-8)
;; don't display tool bar
(tool-bar-mode -1)
;; don't display menu bar
(menu-bar-mode -1)
;;line and column number
(column-number-mode t)
(setq linum-format "%4d ")
(global-linum-mode t)
;; automatically insert )
(electric-pair-mode t)
;; enable show-paren-mode
(show-paren-mode t)
;; don't display startup message
(setq inhibit-startup-message t)
;; set scroll bar right
(set-scroll-bar-mode 'right)

(load-theme 'deeper-blue t)
