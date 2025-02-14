;; An Emacs mode for Zeek's BIF files

(defvar zeek-bif-mode-hook nil)

;; ---- Syntax Highlighting --------------------------------------------

(defvar zeek-bif-mode-keywords
  `(("\\(@[^#\n]+\\)" (0 font-lock-preprocessor-face t))
    (,(concat "\\<"
	      (regexp-opt '("addr" "any" "bool" "count" "counter" "double"
                        "enum" "file" "int" "interval" "list" "net"
                        "opaque" "paraglob" "pattern" "port" "record"
                        "set" "string" "subnet" "table" "timer" "time"
                        "union" "vector") t)
	      "\\>") (0 font-lock-type-face))
    (,(concat "\\<"
	      (regexp-opt '("function") t)
	      "\\>") (0 font-lock-keyword-face))
    ("\\(&[a-zA-Z_0-9]+\\)" (0 font-lock-builtin-face))
    )
  "Keyword highlighting spec for Zeek mode")

(font-lock-add-keywords 'zeek-bif-mode zeek-bif-mode-keywords)

;; ---- The Syntax Table -----------------------------------------------

(defvar zeek-bif-mode-syntax-table
  (let ((zeek-bif-mode-syntax-table (make-syntax-table)))
    ;; Comment starts/ends in the Zeek language.
    ;; Does not distinguish Zeekygen comments.
    (modify-syntax-entry ?# "<" zeek-bif-mode-syntax-table)
    (modify-syntax-entry ?\n ">" zeek-bif-mode-syntax-table)

    zeek-bif-mode-syntax-table)
  "Syntax table for Zeek mode")

;; ---- Main definitions -----------------------------------------------

;; We add whitespace minor mode by default and configure it to only show us
;; spaces after tabs or right from the start of a line. The face is called
;; whitespace-space-after-tab.
(add-hook 'zeek-bif-mode-hook 'whitespace-mode)
(add-hook 'zeek-bif-mode-hook
          (lambda ()
            (setq whitespace-space-after-tab-regexp '("^\t*\\( +\\)"))
            (setq whitespace-style '(face space-after-tab))
            ))

(defun zeek-bif-mode ()
  "Major mode for editing Zeek BIF files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table zeek-bif-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(zeek-bif-mode-keywords))
  (set (make-local-variable 'comment-start) "#")

  (setq indent-tabs-mode t)
  (setq c-basic-offset 8)
  (setq tab-width 8)
  (local-set-key (kbd "TAB") 'self-insert-command)

  (setq major-mode 'zeek-bif-mode)
  (setq mode-name "Zeek BIF")
  (run-hooks 'zeek-bif-mode-hook))

(provide 'zeek-bif-mode)
