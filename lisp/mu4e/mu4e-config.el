;;; mu4e-config.el --- configuration values -*- lexical-binding: t -*-

;; Copyright (C) 2023-2026 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; mu4e is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; mu4e is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mu4e.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Generic support for showing new-mail notifications.

;;; Code:

(defconst mu4e-mu-version "1.14.4"
  "Required mu binary version.
mu4e's version must agree with this.")

(defconst mu4e-doc-dir "/gnu/store/lpgfh23a1gb5b6aj508kx1877k68mhdx-mu-1.14.4/share/doc/mu"
  "Directory for mu4e documentation.")

(defconst mu4e-icon-path "/gnu/store/lpgfh23a1gb5b6aj508kx1877k68mhdx-mu-1.14.4/share/icons/hicolor/scalable/apps/mu-icon.svg"
  "Path to the mu/mu4e icon.")

(provide 'mu4e-config)
;;; mu4e-config.el.in ends here
