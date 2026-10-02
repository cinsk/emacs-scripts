

(defmacro time-loop (n &rest body)
  "Return time before and after N iteration of BODY.

DO NOT USE THIS MACRO.  INSTEAD, USE `benchmark'."
  (declare (indent 1) (debug t))
  `(let ((t1 (current-time)))
     (dotimes (i ,n)
       ,@body)
     (time-subtract (current-time) t1)))


(defmacro save-font-excursion (face &rest body)
  "Save the :font property of given FACE during the execution of BODY."
  (declare (indent 1) (debug t))
  (let ((face-sym (gensym "face-"))
        (oldfont (gensym "oldfont-"))
        (ret (gensym "ret-")))
    `(let* ((,face-sym ,face)
            (,oldfont (face-attribute ,face-sym :font))
            ,ret)
       (setq ,ret (progn ,@body))
       (or (string= ,oldfont (face-attribute ,face-sym :font))
           (set-face-attribute ,face-sym nil :font ,oldfont))
       ,ret)))
