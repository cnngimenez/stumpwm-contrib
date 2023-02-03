;;; keys.lisp

;;; Copyright 2023 Christian Gimenez
;;;

;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(in-package #:keys)

(defvar saved-keys nil
  "Keys pressed last time.
The `save-key-seq' function is called by the `*key-press-hook*' to
store here the last pressed keys.")

(defvar *keys-formatters-alist*
  '((#\k fmt-all-keys))
  "Function to call to fill the %k and other percent codes.")

(defvar *key-modeline-fmt* "[%k]"
  "How to display the keys at the modeline.

Up to now, these are the available percent codes:

@table @asis
@item %k
All keys as a string.
@end table
")

(defun save-key-seq (key seq val)
  "Save the key sequence to show it at the modeline.

Based on the snippet found at the Wiki here:
https://github.com/stumpwm/stumpwm/wiki/Tips-And-Tricks"
  ;; (message (print-key-seq (reverse seq))))
  (setq saved-keys (reverse seq))
  (stumpwm::update-all-mode-lines))

(defun fmt-all-keys ()
  (let ((keys saved-keys))
    (setq saved-keys nil)
    (stumpwm::print-key-seq keys)))

(defun keys-modeline (ml)
  (declare (ignore ml))
  (format-expand *keys-formatters-alist* *key-modeline-fmt*))

(add-hook *key-press-hook* 'save-key-seq)

(add-screen-mode-line-formatter #\K 'keys-modeline)
