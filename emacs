;; Add custom .el path to load-path
(add-to-list 'load-path "~/.emacs.d/custom")

(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Setup use-package
(eval-when-compile
  (require 'use-package))

(use-package vlf)
(use-package cmake-mode)
(use-package lsp-mode
  :hook ((c-mode c++-mode) . lsp)
  :config
  (setq lsp-clients-clangd-args '("-j=4" "--background-index" "--log=error" "--compile-commands-dir=build"))
;  (setq lsp-eldoc-enable-hover nil)
  :commands lsp)
(use-package lsp-ui
  :commands lsp-ui-mode)
(use-package yasnippet)
(use-package zeek-mode)

(add-hook 'zeek-mode-hook #'lsp)
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
	       '(zeek-mode . "zeek"))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "zeek-language-server")
                    :activation-fn (lsp-activate-on "zeek")
                    :server-id 'zeek)))

;; Setup using bison mode and flex mode
(require 'flex)
(add-to-list 'auto-mode-alist '("\\.l$" . flex-mode))
(autoload 'flex-mode "flex")

(require 'bison-mode)
(add-to-list 'auto-mode-alist '("\\.y$" . bison-mode))
(autoload 'bison-mode "bison-mode")

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
(abbrev-mode -1)
(eldoc-mode -1)

;; set shell mode variables
(setq shell-prompt-pattern "[A-Za-z]* \[[0-9]*\]% ")

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
 '(highlight ((t (:background "blue"))))
 '(mode-line ((t (:background "blue" :foreground "white" :box (:line-width -1 :style released-button)))))
 '(region ((t (:background "blue" :foreground "white")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(c-noise-macro-names '("constexpr"))
 '(c-syntactic-indentation t)
 '(c-syntactic-indentation-in-macros t)
 '(case-fold-search t)
 '(completion-ignored-extensions
   '(".o" ".elc" "~" ".bin" ".lbin" ".fasl" ".dvi" ".toc" ".log" ".aux" ".lof" ".blg" ".bbl" ".glo" ".idx" ".lot" ".tmp"))
 '(current-language-environment "English")
 '(fill-column 100)
 '(global-auto-revert-mode t)
 '(global-font-lock-mode t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(message-log-max t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(10 ((shift) . 1) ((control))))
 '(package-selected-packages
   '(rust-mode bison-mode persp-mode flycheck flymake yasnippet vlf yaml-mode use-package lsp-ui cmake-mode dap-mode lsp-mode smart-tab smart-tabs-mode))
 '(scroll-step 1)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(truncate-partial-width-windows nil)
 '(visible-bell t)
 '(which-function-mode t)
 '(whitespace-style
   '(face trailing tabs spaces lines newline empty indentation space-before-tab space-mark tab-mark newline-mark)))

;; This macro allows me to define a different indentation style based on path matching
(defmacro define-new-c-style (name derived-from style-alist match-path)
  `(progn
     (c-add-style ,name
		  '(,derived-from ,@style-alist))
     (add-hook 'c-mode-hook
	       (lambda ()
		 (let ((filename (buffer-file-name)))
		   (when (and filename
			      (string-match (expand-file-name ,match-path) filename))
		     (c-set-style ,name)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Stuff specific to C programming
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Very basic whitesmiths style for zeek
(define-new-c-style "zeek" "whitesmith"
  ((c-basic-offset . 4)
   (c-offsets-alist . ((statement-case-open . +)
		       (arglist-cont-nonempty . c-lineup-arglist)))
   (tab-width . 4)
   (indent-tabs-mode . t)) "*")

;; I don't know why I need to do this, but c++-mode is forgetting that
;; it should be the mode for .h files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Force some keywords in C++ to be specific font faces
(add-hook 'c++-mode-hook
	  '(lambda()
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
	     (c-set-style "zeek")
	     ) t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; This unfortunately has to load after everything else or it doesn't work.
(use-package smart-tabs-mode
	     :ensure t
	     :config
	     (smart-tabs-insinuate 'c 'c++))

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

(put 'downcase-region 'disabled nil)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(put 'upcase-region 'disabled nil)
