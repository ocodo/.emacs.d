;;; tblui-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "tblui" "tblui.el" (22519 50651 450888 254000))
;;; Generated autoloads from tblui.el

(autoload 'tblui-define "tblui" "\
Define tabulated list UI easily.  Hereafter referred as tblui.
This macro defines functions and popups for the defined tblui.
User of this macro can focus on writing the logic for ui, let this
package handle the tabulated list buffer interaction part.

Each arguments are explained as follows:

 * `TBLUI-NAME` : the symbol name of defining tblui.  It will be used
                  as prefix for functions defined via this macro.
 * `ENTRIES-PROVIDER` : the function which provides tabulated-list-entries
 * `TABLE-LAYOUT` : the `tabulated-list-format` to be used for the tblui.
 * `POPUP-DEFINITIONS` : list of popup definition.
   A popup definition is an plist of
       `(:key KEY :name NAME :funcs FUNCTIONS)`.
   KEY is the key to be bound for the defined magit-popup.
   NAME is the name for defined magit-popup.
   FUNCTIONS is the list of action definition.
   Action definition is a list of 3 elements,
   which is `(ACTIONKEY DESCRIPTION FUNCTION)`.

   ACTIONKEY is the key to be used as action key in the magit-popup.
   DESCRIPTION is the description of the action.
   FUNCTION is the logic to be called for this UI.
   It is the elisp function which receives the IDs of tabulated-list entry,
    and do what ever operation.

With this macro `TBLUI-NAME-goto-ui` function is defined.
Calling this function will popup and switch to the tblui buffer.

\(fn TBLUI-NAME ENTRIES-PROVIDER TABLE-LAYOUT POPUP-DEFINITIONS)" nil t)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; tblui-autoloads.el ends here
