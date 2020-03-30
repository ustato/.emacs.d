;;; init.el --- Spacemacs Initialization File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

;; Increase gc-cons-threshold, depending on your system you may set it back to a
;; lower value in your dotfile (function `dotspacemacs/user-config')
(setq gc-cons-threshold 100000000)

(defconst spacemacs-version         "0.200.13" "Spacemacs version.")
(defconst spacemacs-emacs-min-version   "24.4" "Minimal version of Emacs.")

(if (not (version<= spacemacs-emacs-min-version emacs-version))
    (error (concat "Your version of Emacs (%s) is too old. "
                   "Spacemacs requires Emacs version %s or above.")
           emacs-version spacemacs-emacs-min-version)
  (load-file (concat (file-name-directory load-file-name)
                     "core/core-load-paths.el"))
  (require 'core-spacemacs)
  (spacemacs/init)
  (configuration-layer/sync)
  (spacemacs-buffer/display-startup-note)
  (spacemacs/setup-startup-hook)
  (require 'server)
  (unless (server-running-p) (server-start)))


;; straight.el のインストール
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package
(straight-use-package 'use-package)

;; use-packageをstraight.elにフォールバックする
(setq straight-use-package-by-default t)


;; HTTPS 系のリポジトリ
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)

;; HTTP 系のリポジトリ
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/") t)

;; marmalade　は HTTP アクセスすると証明書エラーでフリーズするので注意
;; (add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)
;; (straight-normalize-all)


(use-package markdown-mode)
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

(setq markdown-command "pandoc")
;; (setq markdown-command "multimarkdown")

;; (autoload 'markdown-preview-mode "markdown-preview-mode.el" t)
(autoload 'markdown-live-preview-mode "markdown-preview-mode.el" t)


;; バックスラッシュ
(define-key global-map [165] [92]) ;; 165が¥（円マーク） , 92が\（バックスラッシュ）を表す


(defun set-alpha (alpha-num)
  "set frame parameter 'alpha"
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha (cons alpha-num '(90))))


;; キーバインド設定
(global-set-key (kbd "<M-left>")  'windmove-left)
(global-set-key (kbd "<M-down>")  'windmove-down)
(global-set-key (kbd "<M-up>")    'windmove-up)
(global-set-key (kbd "<M-right>") 'windmove-right)

(global-set-key (kbd "C-c h")  'windmove-left)
(global-set-key (kbd "C-c j")  'windmove-down)
(global-set-key (kbd "C-c k")    'windmove-up)
(global-set-key (kbd "C-c l") 'windmove-right)

(global-unset-key (kbd "C-<wheel-up>") )
(global-unset-key (kbd "C-<wheel-down>") )
(global-unset-key (kbd "C-<triple-wheel-up>") )
(global-unset-key (kbd "C-<triple-wheel-down>") )


;; use-package
;;; company
(use-package company
  :init
  (setq company-selection-wrap-around t)
  :bind
  (:map company-active-map
        ("M-n" . nil)
        ("M-p" . nil)
        ("C->" . company-select-next)
        ("C-<" . company-select-previous)
        ("C-h" . nil))
  :config
  (global-company-mode))
(use-package company-tabnine)
(use-package company-reftex)
(use-package company-bibtex)
(use-package company-shell)
;; (use-package company-jedi)

;;; Docker
(use-package dockerfile-mode)
(use-package docker-compose-mode)
(use-package docker-tramp)

;;; yatex
(use-package yatex)                ;; パッケージ読み込み
(add-to-list 'auto-mode-alist '("\\.tex\\'" . yatex)) ;;auto-mode-alistへの追加
(setq tex-command "platex")       ;; 自分の環境に合わせて""内を変えてください
(setq bibtex-command "pbibtex")    ;; 自分の環境に合わせて""内を変えてください
;;reftex-mode
(add-hook 'yatex-mode-hook
          #'(lambda ()
              (reftex-mode 1)
              (define-key reftex-mode-map
                (concat YaTeX-prefix ">") 'YaTeX-comment-region)
              (define-key reftex-mode-map
                (concat YaTeX-prefix "<") 'YaTeX-uncomment-region)))
(setq reftex-default-bibliography '("/Library/TeX/texbin/bibtex"))

;;; org-mode
(use-package org-doing)
(use-package ox-latex-subfigure)
(require 'ox-bibtex)

;;; LaTeX 形式のファイル PDF に変換するためのコマンド
(setq org-latex-pdf-process
      '("platex %f"
        "platex %f"
        "pbibtex %b"
        "platex %f"
        "platex %f"
        "dvipdfmx %b.dvi"
        "rm %b.bbl %b.dvi"))

;;; \hypersetup{...} を出力しない
(setq org-latex-with-hyperref nil)

;;; LaTeX 形式へ変換するスタイル
(add-to-list 'org-latex-classes
             '("thesis"
               "\\documentclass{jarticle}
               [NO-PACKAGES]
               [NO-DEFAULT-PACKAGES]
               \n\\usepackage[dvipdfmx]{graphicx}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("onecolumnsreport"
               "\\documentclass[10.5pt, aJ4]{jarticle}
               [NO-PACKAGES]
               [NO-DEFAULT-PACKAGES]\n
\\usepackage{ascmac}
\\usepackage{amsmath}
\\usepackage{$HOME/.emacs.d/latex/fancyhdr}
\\usepackage{$HOME/.emacs.d/latex/onecolumnsproceeding}
\\makeatletter
\\long\\def\\@makecaption#1#2{%
  \\vskip\\abovecaptionskip  \\iftdir\\sbox\\@tempboxa{#1\\hskip1zw#2}%
    \\else\\sbox\\@tempboxa{#1~ #2}%
  \\fi
  \\ifdim \\wd\\@tempboxa >\\hsize
    \\iftdir #1\\hskip1zw#2\\relax\\par
    \\else #1~ #2\\relax\\par\\fi
  \\else
  \\global \\@minipagefalse
  \\hbox to\\hsize{\\hfil\\box\\@tempboxa\\hfil}%
\\fi
\\vskip\\belowcaptionskip}
\\def\\WordCount#1{%
  \\@tempcnta\\z@
  \\@tfor \\@tempa:=#1\\do{\\ignorespaces \\advance\\@tempcnta\\@ne}%
  #1
  \\\\ \\hrulefill \\\\
  \\vspace{-8mm}
  \\begin{flushright}
    {\\bfseries 文字数： \\the\\@tempcnta 文字}
  \\end{flushright}
}
\\makeatother"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("twocolumnsreport"
               "\\documentclass[10.5pt, aJ4]{jarticle}
               [NO-PACKAGES]
               [NO-DEFAULT-PACKAGES]\n
\\usepackage{ascmac}
\\usepackage{amsmath}
\\usepackage{$HOME/.emacs.d/latex/fancyhdr}
\\usepackage{$HOME/.emacs.d/latex/twocolumnsproceeding}
\\makeatletter
\\long\\def\\@makecaption#1#2{%
  \\vskip\\abovecaptionskip  \\iftdir\\sbox\\@tempboxa{#1\\hskip1zw#2}%
    \\else\\sbox\\@tempboxa{#1~ #2}%
  \\fi
  \\ifdim \\wd\\@tempboxa >\\hsize
    \\iftdir #1\\hskip1zw#2\\relax\\par
    \\else #1~ #2\\relax\\par\\fi
  \\else
  \\global \\@minipagefalse
  \\hbox to\\hsize{\\hfil\\box\\@tempboxa\\hfil}%
\\fi
\\vskip\\belowcaptionskip}
\\def\\WordCount#1{%
  \\@tempcnta\\z@
  \\@tfor \\@tempa:=#1\\do{\\ignorespaces \\advance\\@tempcnta\\@ne}%
  #1
  \\\\ \\hrulefill \\\\
  \\vspace{-8mm}
  \\begin{flushright}
    {\\bfseries 文字数： \\the\\@tempcnta 文字}
  \\end{flushright}
}
\\makeatother"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


;;; magit
(use-package git)
(use-package magit)
(global-set-key (kbd "C-x g") 'magit-status)

;;; git-gutter+
(use-package git-gutter+)
(use-package git-gutter-fringe+)

;;; yaml
(use-package yaml-mode)

;;; yasnippet
(add-to-list 'load-path
             "~/.emacs.d/snippets")
(use-package yasnippet)
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/mySnippets" 
        "~/.emacs.d/snippets"
        ))
(shell-command "git clone --recursive https://github.com/joaotavora/yasnippet ~/.emacs.d/snippets")
;;;; yas起動
(yas-global-mode 1)
;;;; 既存スニペットを挿入する
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;;;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;;;; 既存スニペットを閲覧・編集する
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)

;;; jupyter
(use-package ein)
(use-package csv-mode)
(use-package jupyter)

;; julia
;; SLIME, the Superior Lisp Interaction Mode for Emacs

;;; (use-package slime
  ;;; :ensure t
  ;;; :config
  ;;; (setq slime-net-coding-system 'utf-8-unix)
  ;;; (setq slime-contribs '(slime-fancy slime-banner slime-media)))
;;; 
;;; (global-set-key "\C-cs" 'slime-selector)
;;; 
;;; (setq slime-lisp-implementations
      ;;; '((julia ("julia" "-i") :init julia-slime-init)
        ;;; (sbcl ("sbcl"))))
;;; 
;;; (use-package julia-mode
  ;;; :ensure t
  ;;; :config
  ;;; (add-hook 'julia-mode-hook 'julia-slime-mode)
  ;;; ;;(add-hook 'julia-mode-hook 'julia-repl-mode)
  ;;; )
;;; 
;;; (defun julia-slime-mode ()
  ;;; (slime-mode)
  ;;; (define-key slime-mode-map (kbd "C-x C-e")
    ;;; 'slime-julia-eval-last-expression))
;;; 
;;; (defun slime-julia-eval-last-expression ()
  ;;; (interactive)
  ;;; (save-excursion
    ;;; (let ((end (point)))
      ;;; (beginning-of-line)
      ;;; (let ((start (point)))
	;;; (slime-flash-region start end)
	;;; (slime-interactive-eval
	 ;;; (buffer-substring-no-properties start end))))))
;;; 
;;; (defun julia-slime ()
  ;;; (interactive)
  ;;; (slime 'julia))
;;; 
;;; (defvar swank-julia-port 4005
  ;;; "Port to use for communicating with the Julia swank server.")
;;; 
;;; (defun julia-slime-init (file _)
  ;;; (setq slime-protocol-version 'ignore)
  ;;; (setq slime-complete-symbol-function 'slime-simple-complete-symbol)
  ;;; (setq slime-description-autofocus t)
  ;;; ;;(add-hook 'slime-popup-buffer-mode-hook #'markdown-mode)
  ;;; (format "using Swank; createserver(port=%d, filename=\"%s\")\n" swank-julia-port file))


;; オリジナルコマンド
(defun my-find-file-init-el ()
  "init.elを開く"
  (interactive)
  (find-file "~/.emacs.d/init.el")
  )

(defun my-backup-config-to-repository (arg)
  "emacs設定のバックアップコマンド"
  (interactive "Dbackup repository path (/backup/HOME): ")
  (straight-freeze-versions)
  (shell-command (concat "cp ~/.emacs.d/init.el " (format "%S" arg) ".emacs.d/init.el"))
  (shell-command (concat "cp ~/.emacs.d/straight/build-cache.el " (format "%S" arg) ".emacs.d/straight/build-cache.el"))
  (shell-command (concat "cp ~/.spacemacs " (format "%S" arg) "/.spacemacs"))
  (shell-command (concat "cp ~/.viminfo " (format "%S" arg) "/.viminfo"))
  (shell-command (concat "cp -rf ~/.emacs.d/straight/versions " (format "%S" arg) ".emacs.d/straight/."))
  (shell-command (concat "cp -rf ~/.emacs.d/mySnippets " (format "%S" arg) ".emacs.d/."))
  (shell-command (concat "cp -rf ~/.emacs.d/latex " (format "%S" arg) ".emacs.d/."))

  (shell-command (concat "cd " (format "%S" arg) " && git submodule foreach git add -A && git submodule foreach git commit -m \"Updated config\" && git add -A && git commit -m \"Update emacs\" && git push && cd .emacs.d && git push"))
  )

(defun my-file-exists-p (arg)
  "ファイルが存在するか確認"
  (interactive "Ffile name: ")
  (message (format "%S" arg))
  (print (file-exists-p (format "%S" arg)))
  )

(defun my-command (arg)
  "コマンドを呼び出す関数"
  (interactive "Srun command: ")
  (shell-command "%S" arg)
  )

