;;; early-init.el --- early init code -*- lexical-binding: t -*-
;;; Commentary:
;; See https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html for why this file exists.

;;; Code:

;; Disable the menubar and toolbar all the time.
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Stop the cursor from blinking.
(blink-cursor-mode -1)

;; Disable package.el in favor of elpaca.el.
(setq package-enable-at-startup nil)

;; Needed in order to use emacs-lsp-booster.
;;(setenv "LSP_USE_PLISTS" "true")

;; Allow emacs to automatically revert if a file changes on disk instead of asking.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Revert.html
(global-auto-revert-mode t)

;; Turn on font-lock for all buffers all the time.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Font-Lock.html
(global-font-lock-mode t)

;; Highlight matching pairs of parentheses.
(show-paren-mode t)

(provide 'early-init)
;;; early-init.el ends here
