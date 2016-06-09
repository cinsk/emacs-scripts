;; -*-emacs-lisp-*-

;;;
;;; Helm configuration
;;;

(global-set-key [(meta ?X)]
                (lookup-key (current-global-map) [(meta ?x)]))
(global-set-key [(control ?x) (control ?B)]
                (lookup-key (current-global-map) [(control ?x) (control ?b)]))
(global-set-key [(control ?x) (control ?F)]
                (lookup-key (current-global-map) [(control ?x) (control ?f)]))

(when (locate-library "helm")
  (require 'helm)
  (require 'helm-config))

(global-set-key [(control ?x) (control ?a)] 'helm-command-prefix)
;; (global-unset-key [(control ?x) ?c])

;; (substitute-key-definition [(meta ?x)]
;;                            [(meta ?X)]
;;                            (current-global-map))

(global-set-key [(meta ?x)] 'helm-M-x)

(global-set-key [(control ?x) (control ?b)] 'helm-buffers-list)

;; (substitute-key-definition [(control ?x) (control ?f)]
;;                            [(control ?x) (control ?F)]
;;                            (current-global-map))
(global-set-key [(control ?x) (control ?f)] 'helm-find-files)

(global-set-key [(meta ?Y)] 'helm-show-kill-ring)

;; See http://tuhdo.github.io/helm-intro.html

;; rebind tab to do persistent action
(define-key helm-map [(tab)] 'helm-execute-persistent-action)
;; make TAB works in terminal
(define-key helm-map [(control ?i)] 'helm-execute-persistent-action)

;; list actions using C-z
(define-key helm-map [(control ?z)] 'helm-select-action)

(when (executable-find "curl")
  (if (boundp 'helm-net-prefer-curl)
      (setq helm-net-prefer-curl t)
    (setq helm-google-suggest-use-curl-p t)))


(define-key helm-command-map [?\`] 'helm-resume)
(define-key helm-command-map [?R] 'helm-regexp)

(defun helm-recursive-grep ()
  "Call `helm-do-grep' with a prefix argument given."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'helm-do-grep)))

(define-key helm-command-map [?r] 'helm-recursive-grep)
(define-key helm-command-map [?g] 'helm-occur)

(setq
 ;; open helm buffer inside current window, not occupying whole other
 ;; window
 ;;
 ;; helm-split-window-in-side-p t

 ;; move to end or beginning of source when reaching top or
 ;; bottom of source.
 ;;
 ;; helm-move-to-line-cycle-in-source t

 ;; search for library in `require' and `declare-function'
 ;; sexp.
 helm-ff-search-library-in-sexp t

 ;; scroll 8 lines other window using M-<next>/M-<prior>
 helm-scroll-amount 8

 helm-ff-file-name-history-use-recentf t)
