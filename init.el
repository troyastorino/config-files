(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("ELPA" . "http://tromey.com/elpa/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; add el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get 'sync)

(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-bindings
                      starter-kit-eshell
                      starter-kit-js
                      clojure-mode
                      nrepl
                      rainbow-delimiters
                      auctex
                      regex-tool
                      markdown-mode
                      flymake-jshint
                      org
                      ipython
                      )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(defvar code-editing-mode-hooks '(c-mode-common-hook
                                  clojure-mode-hook
                                  emacs-lisp-mode-hook
                                  java-mode-hook
                                  js-mode-hook
                                  lisp-mode-hook
                                  perl-mode-hook
;                                  python-mode-hook ; for some reason very slow
;                                  in python
                                  sh-mode-hook))

;; Add a hs-minor-mode hook to code editing major modes
(dolist (mode code-editing-mode-hooks)
  (add-hook mode 'hs-minor-mode))

;; Make show-trailing-whitespace default
(setq-default show-trailing-whitespace t)

;; set global indent
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq css-indent-offset 2)
(setq js-indent-offset 2)
(setq python-indent-offset 2)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Fix broken flymake xml init
(defun flymake-xml-init ()
  (list "xmllint" (list "--valid" (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))))

(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))

;; set matlab files to default to octave mode
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; set defaluts for markdown mode
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; add cython files to python mode
(add-to-list 'auto-mode-alist '("\\.pyx$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.pxd$" . python-mode))

;; add .hamlc to haml-mode
(add-to-list 'auto-mode-alist '("\\.hamlc$" . haml-mode))

;; set python tabs
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq tab-width 2))))

;; set tab


;; loadrom color theme
(load-theme 'misterioso)

;; Change some key mappings
(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map "\M-g" 'goto-line)

;; Move between windows with arrow keys
(global-set-key [M-S-left] 'windmove-left)          ; move to left windnow
(global-set-key [M-S-right] 'windmove-right)        ; move to right window
(global-set-key [M-S-up] 'windmove-up)              ; move to upper window
(global-set-key [M-S-down] 'windmove-down)          ; move to downer window

;; Configure for aspell
(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-list-command "list")
(setq ispell-extra-args '("--sug-mode=fast"))

;; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-font-lock-mode 1)
(setq org-log-done t)

;; Turns on flymake for all files which have flymake mode
(add-hook 'find-file-hook 'flymake-find-file-hook)

;; Changed key mappings
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Cosmetic changes
(global-rainbow-delimiters-mode)
(set-face-attribute 'default nil :height 100) ;10 pt font

;; Customizations for LaTeX
(setenv "PATH"
        (concat
         "/usr/texbin" ":"
         (getenv "PATH"))); Add /usr/textbin to PATH

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(setq TeX-PDF-mode t) ;By default compile to PDF

(setq tab-stop-list (number-sequence 2 40 2))
(setq-default fill-column 80)

;; Change flymake xml init
(defun flymake-xml-init ()
  (list "xmllint" (list "val" (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))))

;; http://www.spyfoos.com/index.php/2012/03/17/cljs-template-with-clojureclojurescript-repls-from-emacs/
(defun clojurescript-repl ()
  (interactive)
  (run-lisp "lein trampoline cljsbuild repl-listen"))

;; For SpaceX, make .dispersion and .simulation files json
(add-to-list 'auto-mode-alist '("\\.dispersion" . js-mode))
(add-to-list 'auto-mode-alist '("\\.simulation" . js-mode))

;; jake files
(add-to-list 'auto-mode-alist '("\\.jake" . js-mode))
(add-to-list 'auto-mode-alist '("Jakefile" . js-mode))

;; nrepl configuration
(add-hook 'nrepl-interaction-mode-hook
          'nrepl-turn-on-eldoc-mode)
(setq nrepl-lein-command "lein")

;; Make flymake GUI warnings show up in the mini-buffer
(defun flymake-display-warning (warning) 
  "Display a warning to the user, using lwarn"
  (message warning))

;; scss mode
(setq scss-compile-at-save nil)

;; setup pyde
;(pyde-enable)
;(pyde-use-ipython)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
