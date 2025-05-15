;;; init.el --- init file -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path "~/.emacs.d/custom")
(add-to-list 'load-path "~/.local/bin")

(when (eq system-type 'darwin)
  (add-to-list 'load-path "/opt/homebrew/share/clang"))

;; This is a bunch of setup that needs to happen for elpaca
;; to work. I tried moving this to another file and loading
;; it with load-file, but it made startup really slow.
(setq elpaca-lock-file (expand-file-name "elpaca-lockfile.eld" user-emacs-directory))

(defun my/elpaca-write-lock-file ()
  "Write lock file into a fixed path using `elpaca-lock-file'."
  (interactive)
  (minibuffer-with-setup-hook #'(lambda ()
                                  (insert elpaca-lock-file))
    (call-interactively 'elpaca-write-lock-file)))

;; Avoid to show a message about deprecation of cl package
(setq byte-compile-warnings '(cl-functions))

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))

(add-hook 'after-init-hook #'elpaca-process-queues)

(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;; Avoid needing to use ":ensure t" in every use-package invocation
(setq use-package-always-ensure t)

;; Setup tree-sitter
(eval-when-compile
  (require 'treesit))

(use-package treesit-auto
  :custom (treesit-auto-install t)
  :config
  (global-treesit-auto-mode))

;; vlf allows opening very-large files incrementally without bogging down Emacs
(use-package vlf)
(use-package cmake-mode)
(use-package clang-format)

;; yasnippet is required to make lsp-mode work correctly.
(use-package yasnippet)

;; Setup using bison mode for .y and .l files
(use-package bison-mode)

(use-package lsp-mode
  :hook ((c-mode c++-mode c-ts-mode c++-ts-mode zeek-mode) . lsp)
  :config
  (setq lsp-clients-clangd-args '("-j=4" "--background-index" "--log=error" "--compile-commands-dir=build"))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package tiny
  :config
  (tiny-setup-default))

;; Custom major modes for zeek development plus LSP support for zeek-language-server

;; Zeek BIF mode, used by polymode for simple highlighting
;; TODO: Disabled for now because it's very busted.
;; (use-package zeek-bif-mode
;;   :ensure (:host github :repo "timwoj/zeek-bif-mode"))
;; (use-package polymode
;;   :mode("\.bif$" . poly-bif-mode)
;;   :config
;;   (define-hostmode poly-bif-hostmode :mode 'zeek-bif-mode)
;;   (define-innermode poly-cpp-bif-innermode
;;                     :mode 'c++-ts-mode
;;                     :head-matcher "^.*%\{"
;;                     :tail-matcher "^.*%\}"
;;                     :head-mode 'host
;;                     :tail-mode 'host)
;;   (define-polymode poly-bif-mode
;;                    :hostmode 'poly-bif-hostmode
;;                    :innermodes '(poly-cpp-bif-innermode)))

;; Zeek script mode, plus LSP support for zeek-language-server. This requires
;; installation of https://github.com/bbannier/zeek-language-server.
(use-package zeek-mode
  :ensure (:host github :repo "zeek/emacs-zeek-mode"))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
           '(zeek-mode . "zeek"))

  (lsp-register-client
;;   (make-lsp-client :new-connection (lsp-stdio-connection '("zeek-language-server" "-f" "debug"))
   (make-lsp-client :new-connection (lsp-stdio-connection '("zeek-language-server"))
                    :activation-fn (lsp-activate-on "zeek")
                    :server-id 'zeek)))

;; Custom major mode for spicy development
(use-package spicy-ts-mode
  :ensure (:host github :repo "timwoj/spicy-ts-mode")
  :mode "\\.spicy$"
  :config
  (autoload 'spicy-ts-mode "spicy"))

;; Prefer flycheck over flymake.
(use-package flycheck
  :hook
  (after-init-hook . #'global-flycheck-mode))

;; Set some default options for c-ts-mode. This also the basis for
;; some other modes like c++-ts-mode so they carry over. It's set to
;; :ensure nil only because it's a built-in package.
(use-package c-ts-mode
  :ensure nil
  :hook
  (c-ts-mode . 'turn-on-auto-fill)
  :init
  (setq c-ts-basic-offset 4)
  (setq c-ts-mode-indent-style 'k&r)
  (setq c-ts-mode-indent-offset 4)
  (setq c-ts-syntactic-indentation t)
  (setq c-ts-syntactic-indentation-in-macros t))

;; c++-ts-mode doesn't think it should handle header files and so
;; c++-mode picks them up and breaks stuff.
(use-package c++-ts-mode
  :ensure nil
  :mode "\\.h$")

;; Map yml/yaml to yaml-ts-mode
(use-package yaml-ts-mode
  :ensure nil
  :mode "\\.yaml$"
  :mode "\\.yml$")

(use-package treesit-fold
  :ensure (:host github :repo "emacs-tree-sitter/treesit-fold")
  :hook (yaml-ts-mode . treesit-fold-indicators-mode))

;; A few options to make lsp-mode faster (1 MB instead of the 4k default)
(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold 1600000)

;; make yes-no questions just be y-n
(fset 'yes-or-no-p 'y-or-n-p)

; turn off auto-fill-mode which causes auto line wrapping, but turn it on for c-mode
(auto-fill-mode 0)

; set text mode as the default major mode
(setq default-major-mode 'text-mode)

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
(global-set-key [f11]     'smerge-keep-current)
(global-set-key [f12]     'whitespace-cleanup)

; fix the Delete key so it does what it's supposed to
(global-set-key [del] 'delete-char)

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
 '(completion-ignored-extensions
   '(".o" ".elc" "~" ".bin" ".lbin" ".fasl" ".dvi" ".toc" ".log" ".aux" ".lof" ".blg" ".bbl" ".glo" ".idx" ".lot" ".tmp"))
 '(current-language-environment "English")
 '(fill-column 90)
 ;; Don't insert a mixture of tabs and spaces when indenting
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 ;; Allow the *messages* log to grow unbounded.
 '(message-log-max t)
 '(mouse-wheel-progressive-speed nil)
 ;; Make the mouse wheel scroll a larger amount when the ctrl key is pressed.
 '(mouse-wheel-scroll-amount '(10 ((shift) . 1) ((control))))
 ;; Only scroll one line at a time when the cursor moves out of the buffer window.
 '(scroll-step 1)
 '(tab-width 4)
 ;; If multiple windows are open, this prevents emacs from truncating when the window
 ;; isn't wide enough.
 '(truncate-partial-width-windows nil)
 ;; this lets emacs use a dot-file that's symlinked to a git repo without complaining
 '(vc-follow-symlinks nil)
 ;; Make the emacs window flash on errors instead of an actual audio queue
 '(visible-bell t)
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

(add-hook 'c-ts-mode-hook 'clang-format-save-hook-for-this-buffer)
(add-hook 'c++-ts-mode-hook 'clang-format-save-hook-for-this-buffer)
(add-hook 'before-save-hook 'zeek-format-before-save)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Force some keywords in C++ to be specific font faces
;; TODO: this might not be necessary anymore. c++-ts-mode might highlight all
;; of these correctly.
(add-hook 'c++-ts-mode-hook
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
            ))
         ))

(defun define-intrusive (class-name)
  "Surround current word or region with given CLASS-NAME."
  (interactive "sClass name: ")
  (insert "using " class-name "Ptr = zeek::IntrusivePtr<" class-name ">;\n\n"))
(global-set-key [f2] 'define-intrusive)

(defun depr-using (name namespace)
  "Deprecate a symbol with a using statement, aliasing NAME into NAMESPACE."
  (interactive "sClass name: \nsNamespace: ")
  (insert "using " name " [[deprecated(\"Remove in v5.1. Use " namespace "::" name ".\")]] = " namespace "::" name ";\n"))

(defun depr-constexpr (name namespace)
  "Deprecate a symbol with a constexpr statement, aliasing NAME into NAMESPACE."
  (interactive "sName: \nsNamespace: ")
  (insert "constexpr auto " name " [[deprecated(\"Remove in v5.1. Use " namespace "::" name ".\")]] = " namespace "::" name ";\n"))

(defun depr-var (type name namespace)
  "Deprecate an extern variable, aliasing NAME of TYPE into NAMESPACE."
  (interactive "sType: \nsName: \nsNamespace: ")
  (insert "extern " type "& " name " [[deprecated(\"Remove in v5.1. Use " namespace "::" name ".\")]];"))

(defun ns-region (namespace)
  "Wrap a region with a namespace declaration with a name of NAMESPACE."
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
  "Disable deprecation warnings for a block."
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
  "Wrap a region with std::move()."
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

(defun my/highlight-todo-like-words ()
  "Highlight FIXME and TODO with a different font-face."
  (font-lock-add-keywords
   nil `(("\\<\\(FIXME\\|TODO\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'my/highlight-todo-like-words)
(add-hook 'prog-mode-hook #'which-function-mode)

;; Set the default window size. The numbers are the size in characters using
;; the theme's font.
(setq initial-frame-alist
      (append initial-frame-alist
              '((width  . 110)
                (height . 70))))

(provide 'init)

;;; init.el ends here
