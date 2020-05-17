(setq exec-path (append exec-path '("/usr/local/bin")))
; (add-to-list 'exec-path (expand-file-name "~/go/bin"))

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get-bundle cython-mode)
(el-get-bundle dockerfile-mode)
(el-get-bundle flycheck)
(el-get-bundle flycheck-pos-tip)
; (el-get-bundle groovy-mode)
(el-get-bundle smex)
(el-get-bundle redo+)
(el-get-bundle google-c-style)
(el-get-bundle go-mode)
; (el-get-bundle haskell-mode)
(el-get-bundle json-mode)
(el-get-bundle markdown-mode)
; (el-get-bundle tuareg)
(el-get-bundle yaml-mode)
(el-get-bundle ruby-mode)
(el-get-bundle ruby-block)
(el-get-bundle web-mode)
(el-get-bundle protobuf-mode)
; (el-get-bundle yatex)
(el-get-bundle flymake-python-pyflakes)
(el-get-bundle py-autopep8)

(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-h" 'delete-backward-char)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default case-fold-search t)

(show-paren-mode t)
(column-number-mode t)

(if window-system
    (tool-bar-mode 0))
(if window-system
    (menu-bar-mode 0))

(global-set-key "\C-x\C-b" 'electric-buffer-list)

(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 5)

(setq-default indent-tabs-mode nil)

(global-set-key "\C-x\C-r" 'revert-buffer)

;;; fullscreen
;(if (boundp 'set-frame-parameter)
    (progn
      (set-frame-parameter nil 'fullscreen 'fullboth)
      (defun fullscreen()
        (interactive)
        (set-frame-parameter nil 'fullscreen 'fullboth)));)

;;; redo
;;; http://www11.atwiki.jp/s-irie/pages/18.html
(require 'redo+)
(setq undo-no-redo t)
(global-set-key "\C-_" 'redo) 

;;; show spaces
(global-whitespace-mode 1)
(setq whitespace-space-regexp "\\(\u3000\\)")
(setq whitespace-style '(face tabs tab-mark spaces space-mark))
(setq whitespace-display-mappings ())
(set-face-foreground 'whitespace-tab "blue")
(set-face-underline  'whitespace-tab t)
(set-face-foreground 'whitespace-space "yellow")
(set-face-background 'whitespace-space "red")
(set-face-underline  'whitespace-space t)

;;; title bar
(setq frame-title-format (format "emacs@%s : %%f" (system-name)))

;;; hl line
(global-hl-line-mode)
(setq hl-line-face 'underline)

;;; ido
(require 'ido)
(ido-mode t)
(add-hook 'ido-define-mode-map-hook
          (lambda() (define-key map " " 'ido-complete)))
;; disable auto searching for files unless called explicitly
(setq ido-auto-merge-delay-time 99999)
(define-key ido-file-dir-completion-map (kbd "C-c C-s") 
  (lambda() 
    (interactive)
    (ido-initiate-auto-merge (current-buffer))))

;;; yatex
; (setq auto-mode-alist
;       (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
; (autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
; (setq YaTeX-kanji-code 4)
; (add-hook ' yatex-mode-hook
;             '(lambda () (auto-fill-mode -1)))

;;; google c/c++ style
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

;;; c++ 80 cheracter constraint
(add-hook 'c-mode-hook
  (lambda ()
    (font-lock-add-keywords nil
      '(("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face t)))))
(add-hook 'c++-mode-hook
  (lambda ()
    (font-lock-add-keywords nil
      '(("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face t)))))
(add-hook 'python-mode-hook
  (lambda ()
    (font-lock-add-keywords nil
      '(("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face t)))))

;;; java 80 cheracter constraint
(add-hook 'java-mode-hook
  (lambda ()
    (font-lock-add-keywords nil
      '(("^[^\n]\\{100\\}\\(.*\\)$" 1 font-lock-warning-face t)))))

;;; python-mode for waf
(setq auto-mode-alist
      (append '(("wscript" . python-mode)
                ("\\.pyi$" . python-mode)) auto-mode-alist))

;;; py-autopep8
(require 'py-autopep8)
(defun py-autopep8-ex ()
  (when (and (executable-find "autopep8")
             (string-suffix-p ".py" (buffer-name)))
    (py-autopep8-enable-on-save)))
(define-key python-mode-map (kbd "C-c C-f") 'py-autopep8-buffer)
;(add-hook 'python-mode-hook 'py-autopep8-ex)
(defun py-autopep8-disable ()
  (interactive)
  (remove-hook 'python-mode-hook 'py-autopep8-ex t))

;;; ruby-mode for gemspec
(setq auto-mode-alist
      (cons (cons "\\.gemspec$" 'ruby-mode) auto-mode-alist))

;;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(global-set-key "\C-cn" 'flycheck-next-error)
(global-set-key "\C-cp" 'flycheck-previous-error)

;(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

;; rst-mode
(require 'rst)
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))
;(setq frame-background-mode 'dark)
(add-hook 'rst-mode-hook '(lambda() (setq indent-tabs-mode nil)))

;; markdown-mode
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; tuareg-mode
(setq auto-mode-alist
  (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code." t)
(autoload 'camldebug "cameldeb" "Run the Caml debugger." t)

(setq truncate-partial-width-windows nil)

;; cscope
;(require 'xcscope)

;; for c/c++
(defun replace-regexp-case-ignore (pat replace str)
  (let ((def-case case-fold-search))
    (setq case-fold-search nil)
    (let ((res (replace-regexp-in-string pat replace str)))
      (setq case-fold-search def-case)
      res)))
(defun make-capital-var (s)
  (setq case-fold-search nil)
  (concat
   (upcase
    (replace-regexp-case-ignore
     "[_\\.]" "_" 
     (replace-regexp-case-ignore "\\([a-z]\\)\\([A-Z]\\)" "\\1_\\2" s)))
   "_"))
(defun insert-include (f)
  (interactive "b")
  (let* ((uuid (shell-command-to-string "uuidgen"))
         (no_new_line (replace-regexp-in-string "\n" "" uuid))
         (id (replace-regexp-in-string "-" "_" no_new_line))
         (var (make-capital-var (concat f "_" id))))
    (save-excursion
      (beginning-of-buffer)
      (insert (concat "#ifndef " var "\n"))
      (insert (concat "#define " var "\n")))
    (save-excursion
      (end-of-buffer)
      (insert (concat "#endif  // " var "\n")))))

(defun insert-namespace (n)
  (interactive "s")
  (if (eq n "")
      (insert "namespace {\n}  // namespace\n")
    (let* ((namespace (concat "namespace " n)))
      (insert (concat namespace " {\n"))
      (insert (concat "}  // " namespace "\n")))))

(add-hook
 'c++-mode-hook
 (lambda ()
   (local-set-key (kbd "C-c C-i") 'insert-include)
   (local-set-key (kbd "C-c C-n") 'insert-namespace)))

(require 'protobuf-mode)

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

(add-hook 'go-mode-hook
          '(lambda()
             (setq tab-width 2)))
(add-hook 'before-save-hook 'gofmt-before-save)

(when (and
       (eq window-system 'ns)
       (= emacs-major-version 24))
;  (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Migu 1M" :size 14))
;  (set-fontset-font nil 'ascii (font-spec :family "Menlo" :size 11)))
  (set-face-attribute 'default nil :family "Migu 1M" :height 140)
  (set-fontset-font "fontset-default" 'japanese-jisx0208 '("Migu 1M" . "iso10646-*")))
