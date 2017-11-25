;;; xcode-project.el --- A package for reading Xcode project files.

;; Copyright (c) 2017 Olive Toast Software Ltd.

;; Author: John Buckley <john@olivetoast.com>
;; URL: https://github.com/nhojb/xcode-project.git
;; Version: 1.0
;; Keywords: languages, tools
;; Package-Requires: ((emacs "25"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package supports reading Xcode projects.
;;
;; Features:
;;
;; - Parse Xcode project file (project.pbxproj)
;; - Extract information about targets, build configurations, build settings and files etc.
;;
;; Usage:
;;
;; To obtain the parsed project object (alist):
;;
;; (`xcode-project-read' PATH-TO-XCODEPROJ)
;;
;; Helper to locate the project file for any given source file:
;; (`xcode-project-find-xcodeproj' PATH-TO-FILE)
;;
;; Then extract information such as targets, build phases, configurations and files.
;; Most functions return an object (alist), unless otherwise described (e.g. `xcode-project-target-names').
;;
;; Targets:
;; (`xcode-project-targets' PROJ)
;;
;; (`xcode-project-target-names' PROJ)
;;
;; Build Phases:
;; (`xcode-project-build-phases' PROJ TARGET-NAME)
;;
;; Build Configurations:
;; (`xcode-project-build-config-names' PROJ)
;;
;; (`xcode-project-build-config' PROJ CONFIG-NAME TARGET-NAME)
;;
;; Build Settings:
;; (`xcode-project-build-config-setings' CONFIG)
;;
;; Build files - as objects:
;; (`xcode-project-build-files' PROJ TARGET-NAME)
;;
;; - as paths (relative to project)
;; (`xcode-project-build-file-paths' PROJ TARGET-NAME)
;;

;;; Code:

(require 'xcode-parser)

;; Utilities

(defun xcode-project--keep (fn seq)
  "Return a new list of the non-nil results of applying FN to the items in SEQ.
Differ from dash -keep in that the input seq can be any sequence type."
  (if (not (listp seq))
      (setq seq (seq-into seq 'list)))
  (let (results)
    (dolist (elt seq results)
      (when-let (keep (funcall fn elt))
        (if results
            (setcdr (last results) (list keep))
          (setq results (list keep)))))))

(defun xcode-project--objects (project)
  "Return all objects for the specified PROJECT.
Private function."
  (alist-get 'objects project))

(defun xcode-project--objects-isa (project isa &optional keep-ref)
  "Return all objects found in PROJECT of type ISA.
ISA may be a string or a regex.

By default the object reference is discarded (the reference is
usually contained in the value alist), but if you want it included
specify KEEP-REF t.

Private function."
  (xcode-project--keep (lambda (obj)
                         (if (eq (string-match-p isa (alist-get 'isa obj)) 0)
                             (if keep-ref obj
                               ;; drop the reference from the result
                               (cdr obj))))
                       (xcode-project--objects project)))

(defun xcode-project--objects-keep (project pred &optional isa)
  "For each object in PROJECT apply PRED and return a list of non-nil results.
Optionally filter by object type ISA.

This method can be faster than using `xcode-project--objects-isa', then
performing additional filtering on the returned list.

Private function."
  (let ((local-pred (if isa
                        (lambda (obj)
                          (when (string-match-p isa (alist-get 'isa obj))
                            (funcall pred obj)))
                      pred)))
    (xcode-project--keep (lambda(obj) (funcall local-pred obj))
                         (xcode-project--objects project))))

(defun xcode-project--object-ref (project ref &optional keep-ref)
  "Return the object in PROJECT matching REF.
If KEEP-REF t sets ref as the car of the returned object.
Private function."
  (if (stringp ref)
      (setq ref (intern-soft ref)))
  (unless (symbolp ref)
    (error "Object ref must be a symbol"))
  (when-let ((obj (alist-get ref (xcode-project--objects project))))
    (if keep-ref
        (push ref obj)
      obj)))

;; Public

(defun xcode-project-read (xcodeproj-path)
  "Read the Xcode project at XCODEPROJ-PATH.
Returns the parsed Xcode project as a json object, or nil on error."
  (let ((pbxproj-path (if (equal (file-name-nondirectory xcodeproj-path) "project.pbxproj")
                          xcodeproj-path
                        (concat (file-name-as-directory xcodeproj-path) "project.pbxproj")))
        (plutil-path (executable-find "plutil")))
    (if (and (file-exists-p pbxproj-path) plutil-path)
        (when-let ((proj (xcode-parser-read-file (expand-file-name pbxproj-path))))
          ;; append path - used when building file-paths
          (setcdr (last proj) `((xcode-project-path . ,xcodeproj-path)))
          proj))))

(defun xcode-project-find-xcodeproj (directory-or-file)
  "Search DIRECTORY-OR-FILE and parent directories for an Xcode project file.
Returns the path to the Xcode project, or nil if not found."
  (if directory-or-file
      (let (xcodeproj
            (directory (if (file-directory-p directory-or-file)
                           directory-or-file
                         (file-name-directory directory-or-file))))
        (setq directory (expand-file-name directory))
        (while (and (not xcodeproj) (not (equal directory "/")))
          (setq xcodeproj (directory-files directory t ".*\.xcodeproj$" nil))
          (setq directory (file-name-directory (directory-file-name directory))))
        (car xcodeproj))))

(defun xcode-project-path (project)
  "Return the path of the parsed PROJECT."
  (alist-get 'xcode-project-path project))

;; Targets

(defun xcode-project-targets (project &optional key value)
  "Return a list of targets (alist) found for the specified PROJECT object.
Targets are filtered according to the optional KEY VALUE."
  (when-let (targets (xcode-project--objects-isa project "PBXNativeTarget"))
    (if key
        (progn
          (unless (symbolp key)
            (error "Invalid key (should be symbol): %s" key))
          (unless value
            (message "Value not specified"))
          (seq-filter (lambda (target) (equal (alist-get key target) value))
                      targets))
      targets)))

(defun xcode-project-target-names (project)
  "Return a list of target names found for the specified PROJECT object."
  (nreverse (seq-map (lambda (target) (alist-get 'name target))
                     (xcode-project-targets project))))

(defun xcode-project-targets-for-file (project file-name &optional phase-isa)
  "Return a list of targets in PROJECT which include FILE-NAME.
FILE-NAME may be a name or a relative path, but must match the
PBXFileReference path stored in the Xcode project.

Optionally filter via phase type PHASE-ISA if known (e.g. PBXSourcesBuildPhase).

Note: this function currently fails to resolve targets for localized files,
which are referenced (in the build phase) via their parent PBXVariantGroup."
  ;; file-name -> PBXFileReference
  (when-let (file-ref-id (symbol-name (car (car (xcode-project-file-references project file-name t)))))
    ;; PBXFileReference -> PBXBuildFile
    (when-let (build-file-ids (xcode-project--objects-keep project
                                                           (lambda (bf)
                                                             (if (equal (alist-get 'fileRef bf) file-ref-id)
                                                                 (symbol-name (car bf))))
                                                           "PBXBuildFile"))
      ;; Iterate over build phases, returning the associated target if the phase contains the a matching build-file.
      (let ((targets (xcode-project-targets project)))
        (xcode-project--objects-keep project
                                     (lambda (phase)
                                       (when (seq-intersection (alist-get 'files phase) build-file-ids)
                                         (seq-find (lambda (target)
                                                     (seq-contains (alist-get 'buildPhases target) (symbol-name (car phase))))
                                                   targets)))
                                     (or phase-isa "PBX.*BuildPhase"))))))

(defun xcode-project-target-names-for-file (project file-name &optional phase-isa)
  "Return a list of target names in PROJECT which include FILE-NAME.
FILE-NAME may be a name or a relative path, but must match the
PBXFileReference path stored in the Xcode project.
Optionally filter via phase type PHASE-ISA if known (e.g. PBXSourcesBuildPhase)."
  (nreverse (seq-map (lambda (target) (alist-get 'name target))
                     (xcode-project-targets-for-file project file-name phase-isa))))

(defun xcode-project-target-name (target)
  "Return the name of the specified TARGET."
  (alist-get 'name target))

(defun xcode-project-target-product-name (target)
  "Return the product name of the specified TARGET."
  (alist-get 'productName target))

(defun xcode-project-target-type (target)
  "Return the type of the specified TARGET."
  (alist-get 'productType target))

(defun xcode-project-target-ref (target)
  "Return the reference of the specified TARGET."
  (alist-get 'productReference target))

;; Build Configurations

(defun xcode-project--root-build-configs (project &optional name)
  "Return a list of top level (root) build configurations found in PROJECT.
Optionally filter by configuration NAME.
Private function."
  (let* ((pbxproj (car (xcode-project--objects-isa project "PBXProject")))
         (config-list (xcode-project--object-ref project (alist-get 'buildConfigurationList pbxproj))))
    (xcode-project--keep (lambda (ref)
                           (let ((config (xcode-project--object-ref project ref)))
                             (if (or (not name) (equal (alist-get 'name config) name))
                                 config)))
                         (alist-get 'buildConfigurations config-list))))

(defun xcode-project-build-config (project name target-name)
  "Return the build configuration in PROJECT matching NAME for TARGET-NAME."
  (let* ((target (car (xcode-project-targets project 'name target-name)))
         (build-config-list (xcode-project--object-ref project (alist-get 'buildConfigurationList target)))
         (target-build-config
          (seq-some (lambda (ref) (let ((config (xcode-project--object-ref project ref)))
                                    (if (equal (alist-get 'name config) name)
                                        ;; copy to avoid mutating the original
                                        (copy-alist config))))
                    (alist-get 'buildConfigurations build-config-list)))
         (root-settings (alist-get 'buildSettings (car (xcode-project--root-build-configs project name)))))
    (when target-build-config
        ;; merge root build settings
        (setf (alist-get 'buildSettings target-build-config)
              (append (alist-get 'buildSettings target-build-config) root-settings))
        target-build-config)))

(defun xcode-project-build-config-names (project)
  "Return a list of build configurations found in PROJECT.
Optionally filtered by NAME."
  (seq-map (lambda (config) (alist-get 'name config))
           (xcode-project--root-build-configs project)))

(defun xcode-project-build-config-name (config)
  "Return name for the build CONFIG."
  (alist-get 'name config))

(defun xcode-project-build-config-setings (config)
  "Return build settings for the build CONFIG."
  (alist-get 'buildSettings config))

;; Build Phases

(defun xcode-project-build-phases (project target-name &optional isa)
  "Return build phases in PROJECT for TARGET-NAME.
Optionally filter by phase ISA type."
  (let ((target (car (xcode-project-targets project 'name target-name))))
    (xcode-project--keep (lambda (ref)
                           (let ((phase (xcode-project--object-ref project ref)))
                             (when (or (not isa) (equal (alist-get 'isa phase) isa))
                               phase)))
                         (alist-get 'buildPhases target))))

;; Groups

(defun xcode-project-groups (project &optional keep-ref)
  "Return a list of all PBXGroup objects found in PROJECT.
Optionally set KEEP-REF t to keep the group reference in the car of
each group object."
  (when-let ((groups (xcode-project--objects-isa project "PBXGroup" keep-ref)))
    (setq groups (append (xcode-project--objects-isa project "PBXVariantGroup" keep-ref) groups))
    groups))

(defun xcode-project--group-children (project group parent-path &optional pred)
  "Return a list of children found in PROJECT for GROUP.
PARENT-PATH is the fully qualified path to the group (relative to the project).
Optionally filter files via the predicate PRED (FILE)."
  (let (results)
    (seq-do (lambda (ref)
              (let* ((child (copy-alist (xcode-project--object-ref project ref t)))
                     (child-path (alist-get 'path child))
                     (isa (alist-get 'isa child)))
                  (cond
                   ((equal isa "PBXGroup")
                    (setq child-path (if child-path (concat parent-path (file-name-as-directory child-path)) parent-path))
                    (setq results (append results (xcode-project--group-children project child child-path pred))))
                   ((equal isa "PBXVariantGroup")
                    ;; filter the PBXVariantGroup, not its children
                    (when (or (not pred) (funcall pred child))
                      (setq child-path (if child-path (concat parent-path (file-name-as-directory child-path)) parent-path))
                      ;; no need to pass the predicate to PBXVariantGroup children - we filtered via the group itself.
                      (setq results (append results (xcode-project--group-children project child child-path nil)))))
                   ((or (not pred) (funcall pred child))
                    (setf (alist-get 'path child) (concat parent-path child-path))
                  (setq results (append results (list (cdr child))))))))
            (alist-get 'children group))
    results))

;; Files

(defun xcode-project-file-references (project file-name &optional keep-ref)
  "Return a list of PBXFileReference objects in PROJECT matching FILE-NAME.
FILE-NAME may not be unique at the PBXFileReference level, so this method may
return one or more items in the result list.
If KEEP-REF t sets ref as the car of the returned object."
  (let* ((file-name-rel (string-remove-prefix
                         (file-name-directory (alist-get 'xcode-project-path project))
                         (expand-file-name file-name)))
         (file-refs (xcode-project--objects-isa project "PBXFileReference" keep-ref))
         (match (xcode-project--objects-keep project
                                             (lambda(obj)
                                               (when (equal (xcode-project-file-ref-path obj) file-name-rel)
                                                 (if keep-ref obj (cdr obj))))
                                             "PBXFileReference")))
    (unless match
      ;; More permissive search, on file-name only (ignoring directory).
      ;; This allows us to match the file-name regardless of the relative path.
      (setq match (xcode-project--objects-keep project
                                               (lambda (obj)
                                                 (when
                                                     (or (equal (file-name-nondirectory (xcode-project-file-ref-path obj))
                                                                file-name-rel)
                                                         (equal (xcode-project-file-ref-path obj)
                                                                (file-name-nondirectory file-name-rel)))
                                                   (if keep-ref obj (cdr obj))))
                                       "PBXFileReference")))
    match))

(defun xcode-project--file-list (project &optional pred)
  "Return complete list of files, as PBXFileReference objects, for the PROJECT.
Includes fully qualified paths (relative to the project's root directory).
Optionally filter files via predicate PRED (FILE-REF).

This method builds the file list recursively, starting at the root group.
It's much faster to build paths this way, than to start with a leaf node (file)
and work back up the group hierarchy."
  (let ((root-group (seq-some (lambda (grp)
                                (let ((name (alist-get 'name grp))
                                      (path (alist-get 'path grp)))
                                  (if (and (or (not name) (equal name "CustomTemplate")) (not path))
                                      grp)))
                              (xcode-project-groups project t))))
    (unless root-group
      (error "Unable to locate the root project group!"))
    (xcode-project--group-children project root-group "" pred)))

(defun xcode-project-build-files (project target-name &optional phase-isa pred)
  "Return the files, as PBXFileReference objects, in PROJECT for TARGET-NAME.
Optionally filter by PHASE-ISA type or predicate PRED (FILE-REF)."
  ;; Get a "whitelist" of file references for the target's build phases
  (let ((whitelist
         (seq-mapcat (lambda (phase)
                       ;; PBXBuildFile -> PBXFileReference
                       (seq-map (lambda (ref)
                                  (let ((build-file (xcode-project--object-ref project ref)))
                                    ;; string -> symbol
                                    ;; we create an alist - which is slightly faster to query than a plain list.
                                    (cons (intern-soft (alist-get 'fileRef build-file)) t)))
                                (alist-get 'files phase)))
                     (xcode-project-build-phases project target-name phase-isa))))
    ;; use local-pred to avoid capture of "pred" in `xcode-project--file-list'.
    (let* ((local-pred pred)
           (combined-pred (if local-pred
                              (lambda (file-with-ref)
                                (and (funcall local-pred file-with-ref) (alist-get (car file-with-ref) whitelist)))
                            (lambda (file-with-ref)
                              (alist-get (car file-with-ref) whitelist)))))
      (xcode-project--file-list project combined-pred))))

(defun xcode-project-build-file-paths (project target-name &optional phase-isa pred absolute)
  "Return file paths, relative to PROJECT's root, for TARGET-NAME.

Optionally filter by PHASE-ISA type or predicate PRED (FILE-REF).

If ABSOLUTE is non-nil then create absolute paths."
  (let ((parent-path (when absolute
                       (file-name-directory (alist-get 'xcode-project-path project)))))
    (seq-map (lambda (file) (concat parent-path (alist-get 'path file)))
             (xcode-project-build-files project target-name phase-isa pred))))

(defun xcode-project-file-ref-path (file-ref)
  "Return the file name for the FILE-REF object."
  (or (alist-get 'path file-ref) (alist-get 'name file-ref)))

(defun xcode-project-file-ref-extension (file-ref)
  "Return the file name extension for the FILE-REF object."
  (when-let (path-or-name (or (alist-get 'path file-ref)
                              (alist-get 'name file-ref)))
    (file-name-extension path-or-name)))

(defun xcode-project-file-ref-extension-p (file-ref ext)
  "Return t if the FILE-REF's extension is equal to EXT (case-insensitive)."
  (string-collate-equalp (xcode-project-file-ref-extension file-ref) ext nil t))

(defun xcode-project-serialize (project path)
  "Write the PROJECT to file at PATH."
  (with-temp-file path
    (prin1 project (current-buffer))))

(defun xcode-project-deserialize (path)
  "Return the de-serialized project found at PATH."
  (when (file-exists-p path)
         (with-temp-buffer
           (insert-file-contents path)
           (goto-char (point-min))
           (read (current-buffer)))))

(provide 'xcode-project)
;;; xcode-project.el ends here
