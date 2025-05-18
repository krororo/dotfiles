;; -*- lexical-binding: t; -*-

(require 'mozc)
(require 'popup)

(push '(popup
        (clean-up . mozc-cand-popup-clean-up)
        (clear . mozc-cand-popup-clear)
        (update . mozc-cand-popup-update))
      mozc-candidate-dispatch-table)

(defface mozc-cand-overlay-description-face
  '((t (:inherit mozc-cand-overlay-odd-face)))
  "Face for description part of overlay candidate window."
  :group 'mozc-faces)

(defvar mozc-cand-popup nil)
(make-variable-buffer-local 'mozc-cand-popup)

(defconst mozc-cand-popup-shortcut-spacer ". ")
(defconst mozc-cand-popup-description-space 3)

(defun mozc-cand-popup-draw (candidates)
  (let ((footer-label (mozc-protobuf-get candidates 'footer 'label))
        (focused-index (mozc-protobuf-get candidates 'focused-index))
        (sub-candidates (mozc-protobuf-get candidates 'subcandidates))
        (max-width 0))

    (when sub-candidates
      (setq footer-label
            (catch 'find-focused-value
              (dolist (candidate (mozc-protobuf-get candidates 'candidate))
                (let ((index (mozc-protobuf-get candidate 'index))
                      (value (mozc-protobuf-get candidate 'value))
                      (shortcut (mozc-protobuf-get candidate 'annotation 'shortcut)))
                  (when (eq index focused-index)
                    (throw 'find-focused-value
                           (concat (if shortcut (concat shortcut mozc-cand-popup-shortcut-spacer value)
                                     value))))))))
      (setq focused-index (mozc-protobuf-get sub-candidates 'focused-index))
      (setq candidates sub-candidates))

    (let ((candidates-size (mozc-protobuf-get candidates 'size))
          (index-visible (mozc-protobuf-get candidates 'footer 'index-visible))
          (items (mapcar
                  (lambda (candidate)
                    (let ((index (mozc-protobuf-get candidate 'index))
                          (value (mozc-protobuf-get candidate 'value))
                          (description (mozc-protobuf-get candidate 'annotation 'description))
                          (shortcut (mozc-protobuf-get candidate 'annotation 'shortcut)))
                      (setq max-width (max (+ (string-width value)
                                              (if shortcut
                                                  (+ (string-width
                                                      mozc-cand-popup-shortcut-spacer)
                                                     (string-width shortcut)) 0)
                                              (if description
                                                  (+ mozc-cand-popup-description-space
                                                     (string-width description)) 0))
                                           max-width))
                      (popup-make-item (if shortcut
                                           (concat shortcut
                                                   mozc-cand-popup-shortcut-spacer
                                                   value)
                                         value)
                                       :face (if (zerop (logand index 1))
                                                 'mozc-cand-overlay-even-face
                                               'mozc-cand-overlay-odd-face)
                                       :summary description
                                       )))
                  (mozc-protobuf-get candidates 'candidate))))

      (when (and index-visible focused-index candidates-size)
        (let ((index-label (format "%d/%d" (1+ focused-index) candidates-size)))
          (setq footer-label
                (format (concat "%" (number-to-string
                                     (max max-width (string-width index-label))) "s")
                        index-label))))

      (when footer-label
        (setq max-width (max max-width (string-width footer-label)))
        (add-to-list
         'items
         (popup-make-item footer-label :face 'mozc-cand-overlay-footer-face)
         t))

      (mozc-cand-popup-clear)
      (setq mozc-cand-popup (popup-create
                             mozc-preedit-point-origin
                             max-width (length items)
                             :around t
                             :margin-left 1
                             :margin-right 1
                             :selection-face (if focused-index
                                                 'mozc-cand-overlay-focused-face
                                               'mozc-cand-overlay-footer-face)
                             :summary-face 'mozc-cand-overlay-description-face))
      (popup-set-list mozc-cand-popup items)
      (if focused-index
          (popup-select mozc-cand-popup (% focused-index 9))
        ;; when not focused, select footer at once.
        (popup-select mozc-cand-popup (1- (length items))))
      )))

(defun mozc-cand-popup-update (candidates)
  (condition-case nil
      (mozc-with-buffer-modified-p-unchanged
       (mozc-cand-popup-draw candidates))
    (error
     (mozc-cand-popup-clear)
     ;; Fall back to the echo area version.
     (mozc-cand-echo-area-update candidates))))

(defun mozc-cand-popup-clear ()
  (popup-delete mozc-cand-popup))

(defun mozc-cand-popup-clean-up ()
  (mozc-cand-popup-clear))


(provide 'mozc-popup)
