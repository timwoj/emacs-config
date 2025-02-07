;; Add custom .el path to load-path
(add-to-list 'load-path "~/.emacs.d/custom")
(add-to-list 'load-path "/opt/homebrew/Cellar/clang-format/19.1.7/share/clang")
(add-to-list 'load-path "~/.local/bin")

(require 'package)
(add-to-list 'package-archives
         '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Setup tree-sitter
(eval-when-compile
  (require 'treesit))

;; (use-package treesit-auto
;;   :config
;;   (global-treesit-auto-mode))

;; Setup use-package
(eval-when-compile
  (require 'use-package))

(use-package vlf)
(use-package cmake-mode)
(use-package lsp-mode
  :ensure t
  :hook ((c-mode c++-mode) . lsp)
  :config
  (setq lsp-clients-clangd-args '("-j=4" "--background-index" "--log=error" "--compile-commands-dir=build"))
  :commands lsp)
(use-package lsp-ui
  :commands lsp-ui-mode)
(use-package yasnippet)
(use-package clang-format)

(use-package tiny)
(tiny-setup-default)

;; Custom major modes for zeek development plus LSP support for zeek-language-server

;; Zeek BIF mode, used by polymode for simple highlighting
(use-package zeek-bif-mode)
(use-package polymode
  :ensure t
  :mode("\.bif$" . poly-bif-mode)
  :config
  (define-hostmode poly-bif-hostmode :mode 'zeek-bif-mode)
  (define-innermode poly-cpp-bif-innermode
                    :mode 'c++-mode
                    :head-matcher "^.*%\{"
                    :tail-matcher "^.*%\}"
                    :head-mode 'host
                    :tail-mode 'host)
  (define-polymode poly-bif-mode
                   :hostmode 'poly-bif-hostmode
                   :innermodes '(poly-cpp-bif-innermode)))

;; Zeek script mode, plus LSP support for zeek-language-server. This requires
;; installation of https://github.com/bbannier/zeek-language-server.
(use-package zeek-mode)
(add-hook 'zeek-mode-hook #'lsp)
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
           '(zeek-mode . "zeek"))

  (lsp-register-client
;;   (make-lsp-client :new-connection (lsp-stdio-connection '("zeek-language-server" "-f" "debug"))
   (make-lsp-client :new-connection (lsp-stdio-connection '("zeek-language-server"))
                    :activation-fn (lsp-activate-on "zeek")
                    :server-id 'zeek)))

;; Custom major mode for spicy development
(use-package spicy-ts-mode)
(add-to-list 'auto-mode-alist '("\\.spicy$" . spicy-ts-mode))
(autoload 'spicy-ts-mode "spicy")

;; Setup using bison mode for .y and .l files
(use-package bison-mode)

;; A few options to make lsp-mode faster (1 MB instead of the 4k default)
(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold 1600000)

;; make yes-no questions just be y-n
(fset 'yes-or-no-p 'y-or-n-p)

; turn off auto-fill-mode which causes auto line wrapping, but turn it on for c-mode
(auto-fill-mode 0)
(setq c-mode-hook 'turn-on-auto-fill)

; set text mode as the default major mode
(setq default-major-mode 'text-mode)

; disable a couple of minor modes that I don't care about
(setq-default abbrev-mode nil)
(setq-default eldoc-mode nil)

;; set shell mode variables
(setq shell-prompt-pattern "[A-Za-z]* \[[0-9]*\]% ")

;; Hide any "special" buffers by default
(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*")

;; Keybinds
(global-set-key [prior]   'scroll-down) ; Page Up
(global-set-key [next]    'scroll-up)   ; Page Down
(global-set-key [home]    'beginning-of-buffer)
(global-set-key [end]     'end-of-buffer)
(global-set-key [mouse-2] 'yank)
(global-set-key [f1]      'goto-line)
(global-set-key [f3]      'indent-region)
(global-set-key [f4]      'replace-regexp)
(global-set-key [f6]      'start-kbd-macro)
(global-set-key [f7]      'end-kbd-macro)
(global-set-key [f8]      'call-last-kbd-macro)
(global-set-key [f12]     'whitespace-cleanup)

; fix the Delete key so it does what it's supposed to
(global-set-key [del] 'delete-char)

; another comment region
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

(global-set-key (kbd "C-x b") 'switch-to-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "black" :foreground "white"))))
 '(cursor ((t (:background "blue"))))
 '(font-lock-number-face ((t (:inherit 'font-lock-constant-face))))
 '(highlight ((t (:background "blue"))))
 '(mode-line ((t (:background "blue" :foreground "white" :box (:line-width -1 :style released-button)))))
 '(region ((t (:background "blue" :foreground "white")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(c-basic-offset 4)
 '(c-default-style "k&r")
 '(c-noise-macro-names '("constexpr"))
 '(c-syntactic-indentation t)
 '(c-syntactic-indentation-in-macros t)
 '(case-fold-search t)
 '(completion-ignored-extensions
   '(".o" ".elc" "~" ".bin" ".lbin" ".fasl" ".dvi" ".toc" ".log" ".aux" ".lof" ".blg" ".bbl" ".glo" ".idx" ".lot" ".tmp"))
 '(current-language-environment "English")
 '(fill-column 80)
 '(global-auto-revert-mode t)
 '(global-font-lock-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(message-log-max t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(10 ((shift) . 1) ((control))))
 ;; use package-install-selected-packages to install all of these automatically.
 '(package-selected-packages
   '(tiny lsp-ui polymode treesit-auto rust-mode bison-mode flycheck flymake yasnippet vlf yaml-mode use-package cmake-mode dap-mode smart-tab smart-tabs-mode))
 '(scroll-step 1)
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(truncate-partial-width-windows nil)
 ;; this lets emacs use a dot-file that's symlinked to a git repo without complaining
 '(vc-follow-symlinks nil)
 '(visible-bell t)
 '(which-function-mode t)
 '(whitespace-style
   '(face trailing tabs spaces lines newline empty indentation space-before-tab space-mark tab-mark newline-mark)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Stuff specific to C programming
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clang-format-save-hook-for-this-buffer ()
  "Create a buffer local save hook."
  (add-hook 'before-save-hook
            (lambda ()
              (when (locate-dominating-file "." ".clang-format")
                (clang-format-buffer))
              ;; Continue to save.
              nil)
            nil
            ;; Buffer local hook.
            t))
;(add-hook 'c-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))
(add-hook 'c++-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))

;; I don't know why I need to do this, but c++-mode is forgetting that
;; it should be the mode for .h files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Force some keywords in C++ to be specific font faces
(add-hook 'c++-mode-hook
      #'(lambda()
         (font-lock-add-keywords
          nil '(;; PREPROCESSOR_CONSTANT
            ("\\<[A-Z]+[A-Z_]+\\>" . font-lock-constant-face)
            ;; hexadecimal numbers
            ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
            ;; integer/float/scientific numbers
            ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>" . font-lock-constant-face)
            ;; user-types (customize!)
            ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(t\\|type\\|ptr\\)\\>" . font-lock-type-face)
            ("\\<\\(xstring\\|xchar\\)\\>" . font-lock-type-face)
            ))
         ) t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; This unfortunately has to load after everything else or it doesn't work.
;(use-package smart-tabs-mode
;	     :ensure t
;	     :config
;	     (smart-tabs-insinuate 'zeek))

(defun define-intrusive (class-name)
  "Surround current word or region with given text."
  (interactive "sClass name: ")
  (insert "using " class-name "Ptr = zeek::IntrusivePtr<" class-name ">;\n\n"))
(global-set-key [f2] 'define-intrusive)

(defun depr-using (name namespace)
  "Deprecate a symbol with a using statement"
  (interactive "sClass name: \nsNamespace: ")
  (insert "using " name " [[deprecated(\"Remove in v5.1. Use " namespace "::" name ".\")]] = " namespace "::" name ";\n"))

(defun depr-constexpr (name namespace)
  "Deprecate a symbol with a constexpr statement"
  (interactive "sName: \nsNamespace: ")
  (insert "constexpr auto " name " [[deprecated(\"Remove in v5.1. Use " namespace "::" name ".\")]] = " namespace "::" name ";\n"))

(defun depr-var (type name namespace)
  "Deprecate an extern variable"
  (interactive "sType: \nsName: \nsNamespace: ")
  (insert "extern " type "& " name " [[deprecated(\"Remove in v5.1. Use " namespace "::" name ".\")]];"))

(defun ns-region (namespace)
  "Wrap a region with a namespace declaration"
  (interactive "sNamespace: ")
  (let (pos1 pos2 bds)
    (if (and transient-mark-mode mark-active)
    (progn
      (goto-char (region-end))
      (insert "\n} // namespace " namespace)
      (goto-char (region-beginning))
      (insert "namespace " namespace " {\n\n"))
      (progn
    (setq bds (bounds-of-this-at-point 'symbol))
    (goto-char (cdr bds))
    (insert "\n} // namespace " namespace)
    (goto-char (car bds))
    (insert "namespace " namespace " {\n\n")))))

(defun disable-deprecation ()
  "Disable deprecation warnings for a block"
  (interactive "")
  (let (pos1 pos2 bds)
    (if (and transient-mark-mode mark-active)
    (progn
      (goto-char (region-end))
      (insert "#pragma GCC diagnostic pop\n")
      (goto-char (region-beginning))
      (insert "#pragma GCC diagnostic push")
      (insert "\n#pragma GCC diagnostic ignored \"-Wdeprecated-declarations\"\n")
      (progn
    (setq bds (bounds-of-this-at-point 'symbol))
    (goto-char (cdr bds))
    (insert "#pragma GCC diagnostic pop\n")
    (goto-char (car bds))
    (insert "\n#pragma GCC diagnostic push")
    (insert "\n#pragma GCC diagnostic ignored \"-Wdeprecated-declarations\""))))))

(defun move-region ()
  "Wrap a region with std::move()"
  (interactive "")
  (let (pos1 pos2 bds)
    (if (and transient-mark-mode mark-active)
    (progn
      (goto-char (region-end))
      (insert ")")
      (goto-char (region-beginning))
      (insert "std::move(")
      (progn
    (setq bds (bounds-of-this-at-point 'symbol))
    (goto-char (cdr bds))
    (insert ");")
    (goto-char (car bds))
    (insert "std::move("))))))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
