
(push "~/.emacs.d/color-theme" load-path)
(push "~/.emacs.d/smart-tab" load-path)
(require 'package)
(require 'color-theme)
(require 'smart-tab)

;; Set smart-tab, which does the "right thing" among hippie-expand,
;; dabbrev-expand, and indent.
(global-smart-tab-mode 1)

;; Make clipboard behavior sane
(setq select-active-regions nil)
(setq mouse-drag-copy-region t)
(global-set-key [mouse-2] 'mouse-yank-at-click)

;; Save time and space
(if (functionp 'tool-bar-mode) (tool-bar-mode 0))
(setq inhibit-startup-message t)

;; Who needs that silly startup screen?
(setq inhibit-startup-screen t)

;; Make window larger
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 140))


;; Verilog hacks:
(defun verilog-fix-trailing-comments ()
  "Replaces things like 'endmodule : comment' with endmodule : comment."
  (interactive)
  ;; TODO(aranck): need to get back to point.
  (let (pt)
   (setq pt (point))
   (beginning-of-buffer)
   (while (re-search-forward "endmodule +// +" nil t)
     (replace-match "endmodule : "))
   (beginning-of-buffer)
   (while (re-search-forward "endtask +// +" nil t)
     (replace-match "endtask : "))
   (beginning-of-buffer)
   (while (re-search-forward "endfunction +// +" nil t)
     (replace-match "endfunction : "))
   (goto-char pt)
   )
  )

(defun verilog-fix-module-header-indents ()
  "For a region between module and ';' fix some indentation."
  (interactive)
  (let ((pt) (modstartpt) (modendpt))
    (setq pt (point))
    (beginning-of-buffer)
    (re-search-forward "^ *module " nil t)
    (setq modstartpt (point))
    (re-search-forward ";" nil t)
    (setq modendpt (point))

    ;; do indent 3 on module parameters
    (goto-char modstartpt)
    (while (re-search-forward "^ +parameter " modendpt t)
      (replace-match "   parameter "))

    ;; do indent 2 on module input/output/inout
    (goto-char modstartpt)
    (while (re-search-forward "^   input " modendpt t)
      (replace-match "  input "))
    (goto-char modstartpt)
    (while (re-search-forward "^   output " modendpt t)
      (replace-match "  output "))
    (goto-char modstartpt)
    (while (re-search-forward "^   inout " modendpt t)
      (replace-match "  inout "))

    (goto-char pt)
    )
  )


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (wombat)))
 '(delete-key-deletes-forward t)
 '(line-number-mode t)
 '(show-paren-mode t)
 '(verilog-align-ifelse t)
 '(verilog-auto-delete-trailing-whitespace t)
 '(verilog-auto-inst-param-value t)
 '(verilog-auto-inst-vector nil)
 '(verilog-auto-lineup (quote declarations))
 '(verilog-auto-newline nil)
 '(verilog-auto-save-policy nil)
 '(verilog-auto-template-warn-unused t)
 '(verilog-case-indent 2)
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
 )

;; Functions run on save:
;; always delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; in verilog-mode, fix the verilog-mode trailing comments
(add-hook 'verilog-mode-hook
	  '(lambda ()
	     (add-hook 'before-save-hook
		       'verilog-fix-trailing-comments)
	     (add-hook 'before-save-hook
		       'verilog-fix-module-header-indents)
	     (setq indent-tabs-mode nil)
	     (setq tab-width 2))
	  )

(global-set-key (kbd "<f1>") 'find-file)
(global-set-key (kbd "<f3>") 'kill-buffer)
(global-set-key (kbd "C-c i") 'overwrite-mode)
(global-set-key (kbd "C-c g") 'goto-line)
