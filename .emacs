(push "~/.emacs.d/color-theme" load-path)
(push "~/.emacs.d/smart-tab" load-path)
(require 'package)
(require 'color-theme)
(require 'smart-tab)


;; Set up TRAMP
;(setq tramp-default-method "plink")
;(customize-set-variable 'tramp-verbose 6 "Enable remote command traces")
;(with-eval-after-load 'tramp (tramp-change-syntax 'simplified))

;; Set smart-tab, which does the "right thing" among hippie-expand,
;; dabbrev-expand, and indent.
(global-smart-tab-mode 1)

;; Make the window title be the current open buffer:
(setq frame-title-format "%b")
(setq icon-title-format "%b")

;; Make clipboard behavior sane
(setq select-active-regions nil)
(setq mouse-drag-copy-region t)
(global-set-key [mouse-2] 'mouse-yank-at-click)

;; Save time and space
(if (functionp 'tool-bar-mode) (tool-bar-mode 0))
(setq inhibit-startup-message t)

;; Who needs that silly startup screen?
(setq inhibit-startup-screen t)

;; Who needs that silly bell sound?
(setq ring-bell-function 'ignore)

;; Make window larger
(add-to-list 'default-frame-alist '(height . 25))
(add-to-list 'default-frame-alist '(width . 100))

;; Adjust the Buffers menu size
(setq buffers-menu-max-size nil)

;; Add .do files to be in tcl-mode:
(add-to-list 'auto-mode-alist '("\\.do\\'" . tcl-mode))

;; Add Makefile.* files to be in makefile-mode
(add-to-list 'auto-mode-alist '("Makefile\\." . makefile-mode))

;; Add .bzl, BUILD, WORKSPACE to python-mode
(add-to-list 'auto-mode-alist '("BUILD\\'" . python-mode))
(add-to-list 'auto-mode-alist '("WORKSPACE\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.bzl\\'" . python-mode))

(add-to-list 'auto-mode-alist '("\\.emacs\\'" . lisp-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (wombat)))
 '(delete-key-deletes-forward t)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(verilog-align-ifelse t)
 '(verilog-auto-delete-trailing-whitespace t)
 '(verilog-auto-inst-param-value t)
 '(verilog-auto-inst-vector nil)
 '(verilog-auto-lineup (quote declarations))
 '(verilog-auto-newline nil)
 '(verilog-auto-save-policy nil)
 '(verilog-auto-template-warn-unused t)
 '(verilog-case-indent 0)
 '(verilog-cexp-indent 2)
 '(verilog-highlight-grouping-keywords t)
 '(verilog-highlight-modules t)
 '(verilog-indent-level 2)
 '(verilog-indent-level-behavioral 2)
 '(verilog-indent-level-declaration 2)
 '(verilog-indent-level-directive 0)
 '(verilog-indent-level-module 2)
 '(verilog-minimum-comment-distance 50)
 '(verilog-tab-to-comment t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "DAMA" :slant normal :weight normal :height 128 :width normal)))))



;; Verilog hacks:
(defun verilog-fix-trailing-comments ()
  "Replaces things like 'endmodule : comment' with endmodule : comment."
  (interactive)
  (if (string= mode-name "Verilog")
      (let (pt)
	(setq pt (point))
	(beginning-of-buffer)
	(while (re-search-forward "endmodule +// +" nil t)
	  (replace-match "endmodule : "))
	(beginning-of-buffer)
	(while (re-search-forward "endtask +// +" nil t)
	  (replace-match "endtask : "))
	(beginning-of-buffer)
	(while (re-search-forward "endpackage +// +" nil t)
	  (replace-match "endpackage : "))
	(beginning-of-buffer)
	(while (re-search-forward "endfunction +// +" nil t)
	  (replace-match "endfunction : "))
	(beginning-of-buffer)
	(while (re-search-forward "endclass +// +" nil t)
	  (replace-match "endclass : "))
	(goto-char pt)
	)
    )
  )

(defun verilog-fix-module-header-indents ()
  "For a region between module and ';' fix some indentation."
  (interactive)
  (if (string= mode-name "Verilog")
      (let ((pt) (modstartpt) (modendpt) (indentstr))
	;; Make indent string
	(setq indentstr "")
	(dotimes (i verilog-indent-level)
	  (setq indentstr (concatenate 'string indentstr " ")))
	(setq pt (point))
	(beginning-of-buffer)
	(while (re-search-forward "^ *module" nil t)

	  (re-search-forward "^ *module " nil t)
	  (setq modstartpt (point))
	  (setq modendpt (point))
	  ;; make sure we can find a ';' or else there's not module header region
	  ;; because import stmts can come before the module header, forward search
	  ;; for an open paren, close paren, then ;. This isn't great, but works
	  ;; unless there are certain comments that break it.
	  (if (re-search-forward ")" nil t)
	      (if (re-search-forward ";" nil t)
		  (setq modendpt (point))))

	  ;; do indent 2 on module parameters
	  (goto-char modstartpt)
	  (while (re-search-forward "^ +parameter " modendpt t)
	    (replace-match (concatenate 'string indentstr "parameter ")))

	  ;; do indent 2 on module input/output/inout
	  (goto-char modstartpt)
	  (while (re-search-forward "^ +input " modendpt t)
	    (replace-match (concatenate 'string indentstr "input ")))
	  (goto-char modstartpt)
	  (while (re-search-forward "^ +output " modendpt t)
	    (replace-match (concatenate 'string indentstr "output ")))
	  (goto-char modstartpt)
	  (while (re-search-forward "^ +inout " modendpt t)
	    (replace-match (concatenate 'string indentstr "inout ")))
	  )

	(goto-char pt)
	)
    )
  )

;; For Windows people, please don't save new files with CR/LF endlines,
;; force save all files in unix mode:
(defun no-junk-please-were-unixish ()
  (let ((coding-str (symbol-name buffer-file-coding-system)))
    (when (string-match "-\\(?:dos\\|mac\\)$" coding-str)
      (set-buffer-file-coding-system 'unix))))
	  
;; get rid of Tabs, except makefiles
(defun untabify-except-makefiles ()
  "Replace tabs with spaces except in makefiles."
  (unless (derived-mode-p 'makefile-mode)
    (untabify (point-min) (point-max))))


;; Functions run on save:
;; always convert files to unix endlines:
(add-hook 'before-save-hook 'no-junk-please-were-unixish)
;; always delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; delete tabs except Makefiles
(add-hook 'before-save-hook 'untabify-except-makefiles)

;; in verilog-mode, fix the verilog-mode trailing comments
(add-hook 'verilog-mode-hook
          #'(lambda ()
              (add-hook 'before-save-hook
                        'verilog-fix-trailing-comments t)
              (add-hook 'before-save-hook
                        'verilog-fix-module-header-indents t)
              (setq indent-tabs-mode nil)
          ))

(add-hook 'tcl-mode-hook
          #'(lambda ()
             (setq indent-tabs-mode nil)
          ))

(add-hook 'sh-mode-hook
          #'(lambda ()
              (setq indent-tabs-mode nil)
          ))


(add-hook 'text-mode-hook
          #'(lambda ()
              (setq indent-tabs-mode nil)
              ))

(add-hook 'yaml-mode-hook
          #'(lambda ()
              (define-key yaml-mode-map "\C-m" 'newline-and-indent)
              ))

(global-set-key (kbd "<f1>") 'find-file)
(global-set-key (kbd "<f3>") 'kill-buffer)
(global-set-key (kbd "C-c i") 'overwrite-mode)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "<f5>") 'query-replace)
(global-set-key (kbd "<f6>") 'query-replace-regexp)

