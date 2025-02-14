;;; early-init.el --- early init code -*- lexical-binding: t -*-
;;; Commentary:
;; See https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html for why this file exists.

;;; Code:

;; Disable the menubar and toolbar all the time.
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Disable package.el in favor of elpaca.el.
(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
