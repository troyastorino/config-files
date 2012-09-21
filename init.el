(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("ELPA" . "http://tromey.com/elpa/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; add packages installed outside of the package manager
;(add-to-list 'load-path "~/.emacs.d/vendor")
;(add-to-list 'load-path "~/.emacs.d/vendor/jade-mode/sws-mode.el")
;(add-to-list 'load-path "~/.emacs.d/vendor/jade-mode/jade-mode.el")

(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-bindings
                      starter-kit-eshell
                      starter-kit-js
                      clojure-mode
                      rainbow-delimiters
                      auctex
                      regex-tool
                      markdown-mode
                      ;js2-mode
                      flymake-jshint
                      org
                      ;sws-mode
                      ;jade-mode
                      )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))

;; set matlab files to default to octave mode
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; load color theme
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
