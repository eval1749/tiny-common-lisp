(setq r (ed:make-range (ed:selection-buffer ed:*selection*))
      re (si::compile-regex "make-\\S+") )

