(defcustom php-manual-url "http://www.php.net/manual/en/"
  "URL at which to find PHP manual.
You can replace \"en\" with your ISO language code."
  :type 'string
  :group 'php+-mode)

(defcustom php-search-url "http://www.php.net/"
  "URL at which to search for documentation on a word."
  :type 'string
  :group 'php+-mode)

(defcustom php-manual-path ""
  "Path to the directory which contains the PHP manual."
  :type 'string
  :group 'php+-mode)

(defun php-search-documentation ()
  "Search PHP documentation for the word at point."
  (interactive)
  (browse-url (concat php-search-url (current-word t))))

(provide 'php-help)
