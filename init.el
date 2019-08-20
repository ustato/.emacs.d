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
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)

;; HTTP 系のリポジトリ
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/") t)

;; marmalade　は HTTP アクセスすると証明書エラーでフリーズするので注意
;; (add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)


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
             '("report"
               "\\documentclass[10.5pt, aJ4]{jarticle}
               [NO-PACKAGES]
               [NO-DEFAULT-PACKAGES]\n
\\usepackage{$HOME/.emacs.d/latex/fancyhdr}
\\usepackage{$HOME/.emacs.d/latex/proceeding}
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


;; オリジナルコマンド
(defun my-find-file-init-el ()
  "init.elを開く"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun my-backup-config (arg)
  "emacs設定のバックアップコマンド"
  (interactive "Dto backup directory name: ")
  (straight-freeze-versions)
  (shell-command (concat "cp -rf ~/.emacs.d " (format "%S" arg)))
  (shell-command (concat "cp ~/.spacemacs " (concat (format "%S" arg) "/.spacemacs")))
  (shell-command (concat "cp ~/.viminfo " (concat (format "%S" arg) "/.viminfo"))))

(defun my-file-exists-p (arg)
  "ファイルが存在するか確認"
  (interactive "Ffile name: ")
  (message (format "%S" arg))
  (print (file-exists-p (format "%S" arg))))

(defun my-command (arg)
  "コマンドを呼び出す関数"
  (interactive "Srun command: ")
  (shell-command "%S" arg))
