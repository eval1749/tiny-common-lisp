(defdialog find-or-replace-dialog ()
  (dom:document dialog
      (input
        :id :find-what-input
        :maxlen 100
        :accesskey "n"
        (label "Find What:") )
      (input
         :accesskey "p"
         :id :find-replace-input
         :maxlen 100
         (label "Replace With:") )
      (submit
         :accesskey "f"
         :id :find-next-button
         (label "Find Next"))
      (submit
         :accesskey "i"
         :id :fnnd-previos-button
         (label "Find Previous") )
      (submit
         :accesskey "r"
         :id :fnnd-previos-button
         (label "Replace") )
      (submit
        :accesskey "a"
        :id :fnnd-previos-button
        (label "Replace All") ))
 )

(xmldom:document document
    :xmlns "http://www.w3.org/2000/svg"
    :@ "xmlns:xlink" "http://www.w3.org/1999/xlink"
    (xlink:foo) )

; @apperance= "full" | "compact" | "minimal"
; range
; trigger

(defhandler find-or-replace-dialog update (form)
  (let ((has-text
         (> (length (form-string-value (form-element form 'fnid-what-input)))
            0 ) ))
    (enable-control (form-element form 'find-next-button) has-text)
    (enable-control (form-element form 'find-previous-button) has-text) ) )


(defhandler find-or-replace-dialog find-next-button (form)
  ... )

(%defdialog 'find-or-replace-dialog '()
  (let ((doc (make-document "dialog")))
    (append-child* (document-element doc)
        (make-element* doc "input"
            :id 'find-what-input
            :maxlen 100 )
        (make-element* doc "input"
            :id 'find-what-input
            :maxlen 100 )
        (make-element* doc "group"
          (make-element* doc "submit"
              :accesskey "f"
              :id 'find-next-button
              (make-element* doc "label" "Find Next") )
          (make-element* doc "submit"
              :accesskey "i"
              :id 'find-previous-button
              (make-element* doc "label" "Find Previous") )
          (make-element* doc "submit"
              :accesskey "r"
              :id 'find-previous-button
              (make-element* doc "label" "Replace") )
          (make-element* doc "submit"
              :accesskey "r"
              :id 'find-previous-button
              (make-element* doc "label" "Replace All") )) ) ) )

