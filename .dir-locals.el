((erlang-mode .
  ((eval .
    (setq inferior-erlang-machine-options
     (let* ((d (dir-locals-find-file "."))
            (p (file-name-directory (if (stringp d) d (car d))))
           )
      (list
       "-pa" (expand-file-name "ebin" p)
       "-env" "ERL_LIBS" (expand-file-name "lib" p)
      )
     )
    )
   )
  )
 )
)
