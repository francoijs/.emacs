
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; MELPA packages
(require 'package)
(add-to-list 'package-archives
			 '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(pyvenv
    elpy
    flycheck
    ))
;; better-defaults
;; material-theme))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)


;; tabs looks like 4 spaces & indentation is tabs
;; warning: next line must be first
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
                          64 68 72 76 80 84 88 92 96 100 104 108 112
                          116 120))
(setq-default tab-width 4)
(setq-default indent-tabs-mode t)
(setq-default c-basic-offset 4)

;; make
(global-set-key "\C-xc" 'compile)

;; show line numbers
(require 'linum)
(global-linum-mode 1)
(setq linum-format "%d ")

;; show columns
(setq column-number-mode t)
(setq line-number-mode t)

;; paste replace selection
(delete-selection-mode 1)

;; '_' is part of the word in C
;; (modify-syntax-entry ?_ "w")
(add-hook 'c-mode-hook
	  (lambda () (modify-syntax-entry ?_ "w")))

;; remove toolbar
(tool-bar-mode -1)

;; set bg color to light yellow (#FFFFDD)
(setq default-frame-alist
      (append default-frame-alist
       '((foreground-color . "Black")
		 (background-color . "LightYellow"))
))

;; cscope
;;(load-file "xcscope.el")
;;(require 'xcscope)


;; fast switch buffer
   ; necessary support function for buffer burial
    (defun crs-delete-these (delete-these from-this-list)
      "Delete DELETE-THESE FROM-THIS-LIST."
      (cond
       ((car delete-these)
        (if (member (car delete-these) from-this-list)
            (crs-delete-these (cdr delete-these) (delete (car delete-these)
                                                     from-this-list))
          (crs-delete-these (cdr delete-these) from-this-list)))
       (t from-this-list)))
    ; this is the list of buffers I never want to see
    (defvar crs-hated-buffers
      '("KILL" "*Compile-Log*"))
    ; might as well use this for both
    (setq iswitchb-buffer-ignore (append '("^ " "*Buffer") crs-hated-buffers))
    (defun crs-hated-buffers ()
      "List of buffers I never want to see, converted from names to buffers."
      (delete nil
              (append
               (mapcar 'get-buffer crs-hated-buffers)
               (mapcar (lambda (this-buffer)
                         (if (string-match "^ " (buffer-name this-buffer))
                             this-buffer))
                       (buffer-list)))))
    ; I'm sick of switching buffers only to find KILL right in front of me
    (defun crs-bury-buffer (&optional n)
      (interactive)
      (unless n
        (setq n 1))
      (let ((my-buffer-list (crs-delete-these (crs-hated-buffers)
                                              (buffer-list (selected-frame)))))
        (switch-to-buffer
         (if (< n 0)
             (nth (+ (length my-buffer-list) n)
                  my-buffer-list)
           (bury-buffer)
           (nth n my-buffer-list)))))

(global-set-key [(control tab)] 'crs-bury-buffer)
(global-set-key [C-S-iso-lefttab] (lambda ()
                                     (interactive)
                                     (crs-bury-buffer -1)))


;; set default frame size
(defun arrange-frame (w h x y)
  "Set the width, height, and x/y position of the current frame"
  (let ((frame (selected-frame)))
    (delete-other-windows)
    (set-frame-position frame x y)
    (set-frame-size frame w h)))

(arrange-frame 187 70 2 22)


;; C-% to move over parens
(defun goto-match-paren (arg)
  "Go to the matching  if on (){}[], similar to vi style of % "
  (interactive "p")
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc.
  (cond ((looking-at "[\[\(\{]") (forward-sexp))
        ((looking-back "[\]\)\}]" 1) (backward-sexp))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (backward-sexp))
        ((looking-back "[\[\(\{]" 1) (backward-char) (forward-sexp))
        (t nil)))

(global-set-key [(control %)] 'goto-match-paren)


;; highlight parens
;;(load-file "highlight-parentheses.el")
(require 'autopair)
(require 'highlight-parentheses)
(add-hook 'highlight-parentheses-mode-hook
          '(lambda ()
             (setq autopair-handle-action-fns
                   (append
		    (if autopair-handle-action-fns
			autopair-handle-action-fns
		      '(autopair-default-handle-action))
		    '((lambda (action pair pos-before)
			(hl-paren-color-update)))))))
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

(put 'downcase-region 'disabled nil)


;; tern parser for js
(add-to-list 'load-path "~/.emacs.d/node_modules/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
;; (eval-after-load 'tern
;;    '(progn
;;       (require 'tern-auto-complete)
;;       (tern-ac-setup)))

;; 2-spaces indent for web and js
(defun my-setup-indent (n)
  ;; web development
  (setq-local coffee-tab-width n) ; coffeescript
  (setq-local javascript-indent-level n) ; javascript-mode
  (setq-local js-indent-level n) ; js-mode
  (setq-local js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq-local web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq-local web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq-local web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq-local css-indent-offset n) ; css-mode
  )
;; use space instead of tab
(setq indent-tabs-mode nil)
;; indent 2 spaces width
(my-setup-indent 2)


;; ack-grep mode
(add-to-list 'load-path "/path/to/full-ack")
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

;; backup files location
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; ELPY for python
(elpy-enable)
(setq elpy-rpc-python-command "python3")
(elpy-use-ipython "python3")(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "-i")

;; enable flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; markdown mode
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elpy-test-discover-runner-command (quote ("python3" "-m" "unittest")))
 '(python-shell-interpreter "python3")
 '(safe-local-variable-values (quote ((nxml-child-indent . 4)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; lua
(add-to-list 'load-path "~/.emacs.d/lua/")
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
