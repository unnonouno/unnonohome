(setq load-path (cons "~/site-lisp" load-path))

(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-h" 'delete-backward-char)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default case-fold-search t)

(show-paren-mode t)
(column-number-mode t)

(if (boundp 'tool-bar-mode)
    (tool-bar-mode 0))

(global-set-key "\C-x\C-b" 'electric-buffer-list)

(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 5)

(setq-default indent-tabs-mode nil)

(global-set-key "\C-x\C-r" 'revert-buffer)

;;; fullscreen
(set-frame-parameter nil 'fullscreen 'fullboth)
(defun fullscreen()
  (interactive)
  (set-frame-parameter nil 'fullscreen 'fullboth))

;;; redo
;;; http://www11.atwiki.jp/s-irie/pages/18.html
(require 'redo+)
(setq undo-no-redo t)
(global-set-key "\C-_" 'redo) 

;;; show spaces
(defface my-face-b-1 '((t (:background "medium blue"))) nil)
(defface my-face-b-2 '((t (:underline "medium blue"))) nil)

(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)

(defadvice font-lock-mode (before my-font-lock-mode())
  (font-lock-add-keywords
   major-mode
   '(("\t" 0 my-face-b-2 append)
     ("　" 0 myface-b-1 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)

;;; title bar
(setq frame-title-format (format "emacs@%s : %%f" (system-name)))

;;; hl line
(global-hl-line-mode t)

;;; ido
(require 'ido)
(ido-mode t)
(add-hook 'ido-define-mode-map-hook
          (lambda() (define-key map " " 'ido-complete)))

;;; yatex
(setq load-path (cons "/usr/local/lib/mule/site-lisp/yatex" load-path))

(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq YaTeX-kanji-code 4)

;;; google c/c++ style
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)


;;; python-mode for waf
(setq auto-mode-alist
      (cons (cons "wscript" 'python-mode) auto-mode-alist))

;;; fly-make
(require 'flymake)
(defun flymake-cc-init ()
 (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                      'flymake-create-temp-inplace))
        (local-file  (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
   (list "g++" (list "-pipe" "-I." "-I/usr/local/include" "-I/opt/local/include" "-Wall" "-fsyntax-only" local-file))))
(push '("\\.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)
(push '("\\.hpp$" flymake-cc-init) flymake-allowed-file-name-masks)
(add-hook 'c++-mode-hook
         '(lambda ()
            (flymake-mode t)))

(add-hook 'c++-mode-hook '(lambda ()
   (define-key c++-mode-map "\C-c\C-c"
     'flymake-display-err-menu-for-current-line)))


(custom-set-faces '(flymake-errline ((((class color)) (:bold t :underline t :background "firebrick"))))
                 '(flymake-warnline ((((class color)) (:underline t)))))


;; physical-line-mode
(load "physical-line")
(setq-default physical-line-mode t)

(setq truncate-partial-width-windows nil)

(when (eq window-system 'mac)
  ;; font
  ;; "-apple-migu 1m regular-medium-i-normal--0-0-0-0-m-0-mac-roman"
  (setq my-font "-*-*-medium-r-normal--12-*-*-*-*-*-fontset-hiramaru")
  (setq mac-allow-anti-aliasing t)
  (if (= emacs-major-version 22)
      (require 'carbon-font))
  (set-default-font my-font)
  (add-to-list 'default-frame-alist `(font . ,my-font))
  (when (= emacs-major-version 23)
    (set-fontset-font
     (frame-parameter nil 'font)
     'japanese-jisx0208
     '("Migu 1M" . "iso10646-1"))
    (setq face-font-rescale-alist
          '(("^-apple-migu 1m.*" . 1.2)
            (".*osaka-bold.*" . 1.2)
            (".*osaka-medium.*" . 1.2)
            (".*courier-bold-.*-mac-roman" . 1.0)
            (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
            (".*monaco-bold-.*-mac-roman" . 0.9)
            ("-cdac$" . 1.3)))))