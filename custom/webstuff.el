(use-package web-mode
	     :commands web-mode
	     :init
	     (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
	     (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
	     (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
	     (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
	     (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
	     (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
	     (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
	     (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
	     :hook((web-mode-hook . (lambda ()
				      (if (equal web-mode-content-type "javascript")
					  (web-mode-set-content-type "jsx")
					(message "now set to: %s" web-mode-content-type))))))

(use-package sass-mode
	     ::init
	     (add-to-list 'auto-mode-alist '("\\.sass\\'" . sass-mode)))
