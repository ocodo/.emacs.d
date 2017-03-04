(defvar php-imenu-generic-expression
  '(
    ("Private Methods"
     "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?private\\s-+\\(?:static\\s-+\\)?function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1)
    ("Protected Methods"
     "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?protected\\s-+\\(?:static\\s-+\\)?function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1)
    ("Public Methods"
     "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?public\\s-+\\(?:static\\s-+\\)?function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1)
    ("Classes"
     "^\\s-*class\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*" 1)
    ("All Functions"
     "^\\s-*\\(?:\\(?:abstract\\|final\\|private\\|protected\\|public\\|static\\)\\s-+\\)*function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1)
    )
  "Imenu generic expression for PHP Mode.  See `imenu-generic-expression'."
  )

(defconst php-tags '("<?php" "?>" "<?" "<?="))
(defconst php-tags-key (regexp-opt php-tags))

(defconst php-block-stmt-1-kwds '("do" "else" "finally" "try" "catch"))
(defconst php-block-stmt-2-kwds
  '("for" "if" "while" "switch" "foreach" "elseif" "catch all"))

(defconst php-block-stmt-1-key
  (regexp-opt php-block-stmt-1-kwds))
(defconst php-block-stmt-2-key
  (regexp-opt php-block-stmt-2-kwds))

(defconst php-class-decl-kwds '("class" "interface" "trait"))

;; (defconst php-class-key
;;   (concat
;;    "\\(" (regexp-opt php-class-decl-kwds) "\\)\\s-+"
;;    (c-lang-const c-symbol-key c)                ;; Class name.
;;    "\\(\\s-+extends\\s-+" (c-lang-const c-symbol-key c) "\\)?" ;; Name of superclass.
;;    "\\(\\s-+implements\\s-+[^{]+{\\)?")) ;; List of any adopted protocols.

(defconst php-constants
  (eval-when-compile
    (regexp-opt
     '(;; core constants
       "__LINE__" "__FILE__"
       "__FUNCTION__" "__CLASS__" "__METHOD__"
       "PHP_OS" "PHP_VERSION"
       "TRUE" "FALSE" "NULL"
       "E_ERROR" "E_NOTICE" "E_PARSE" "E_WARNING" "E_ALL" "E_STRICT"
       "E_USER_ERROR" "E_USER_WARNING" "E_USER_NOTICE"
       "DEFAULT_INCLUDE_PATH" "PATH_SEPARATOR" "PEAR_INSTALL_DIR" 
       "PEAR_EXTENSION_DIR" "PHP_BINDIR" "PHP_LIBDIR" "PHP_DATADIR" 
       "PHP_SYSCONFDIR" "PHP_LOCALSTATEDIR" "PHP_CONFIG_FILE_PATH"
       "PHP_EOL"

       ;; date and time constants
       "DATE_ATOM" "DATE_COOKIE" "DATE_ISO8601"
       "DATE_RFC822" "DATE_RFC850" "DATE_RFC1036" "DATE_RFC1123"
       "DATE_RFC2822" "DATE_RFC3339"
       "DATE_RSS" "DATE_W3C"

       ;; from ext/standard:
       "EXTR_OVERWRITE" "EXTR_SKIP" "EXTR_PREFIX_SAME"
       "EXTR_PREFIX_ALL" "EXTR_PREFIX_INVALID" "SORT_ASC" "SORT_DESC"
       "SORT_REGULAR" "SORT_NUMERIC" "SORT_STRING" "ASSERT_ACTIVE"
       "ASSERT_CALLBACK" "ASSERT_BAIL" "ASSERT_WARNING"
       "ASSERT_QUIET_EVAL" "CONNECTION_ABORTED" "CONNECTION_NORMAL"
       "CONNECTION_TIMEOUT" "M_E" "M_LOG2E" "M_LOG10E" "M_LN2"
       "M_LN10" "M_PI" "M_PI_2" "M_PI_4" "M_1_PI" "M_2_PI"
       "M_2_SQRTPI" "M_SQRT2" "M_SQRT1_2" "CRYPT_SALT_LENGTH"
       "CRYPT_STD_DES" "CRYPT_EXT_DES" "CRYPT_MD5" "CRYPT_BLOWFISH"
       "DIRECTORY_SEPARATOR" "SEEK_SET" "SEEK_CUR" "SEEK_END"
       "LOCK_SH" "LOCK_EX" "LOCK_UN" "LOCK_NB" "HTML_SPECIALCHARS"
       "HTML_ENTITIES" "ENT_COMPAT" "ENT_QUOTES" "ENT_NOQUOTES"
       "INFO_GENERAL" "INFO_CREDITS" "INFO_CONFIGURATION"
       "INFO_ENVIRONMENT" "INFO_VARIABLES" "INFO_LICENSE" "INFO_ALL"
       "CREDITS_GROUP" "CREDITS_GENERAL" "CREDITS_SAPI"
       "CREDITS_MODULES" "CREDITS_DOCS" "CREDITS_FULLPAGE"
       "CREDITS_QA" "CREDITS_ALL" "PHP_OUTPUT_HANDLER_START"
       "PHP_OUTPUT_HANDLER_CONT" "PHP_OUTPUT_HANDLER_END"
       "STR_PAD_LEFT" "STR_PAD_RIGHT" "STR_PAD_BOTH"
       "PATHINFO_DIRNAME" "PATHINFO_BASENAME" "PATHINFO_EXTENSION"
       "CHAR_MAX" "LC_CTYPE" "LC_NUMERIC" "LC_TIME" "LC_COLLATE"
       "LC_MONETARY" "LC_ALL" "LC_MESSAGES" "LOG_EMERG" "LOG_ALERT"
       "LOG_CRIT" "LOG_ERR" "LOG_WARNING" "LOG_NOTICE" "LOG_INFO"
       "LOG_DEBUG" "LOG_KERN" "LOG_USER" "LOG_MAIL" "LOG_DAEMON"
       "LOG_AUTH" "LOG_SYSLOG" "LOG_LPR" "LOG_NEWS" "LOG_UUCP"
       "LOG_CRON" "LOG_AUTHPRIV" "LOG_LOCAL0" "LOG_LOCAL1"
       "LOG_LOCAL2" "LOG_LOCAL3" "LOG_LOCAL4" "LOG_LOCAL5"
       "LOG_LOCAL6" "LOG_LOCAL7" "LOG_PID" "LOG_CONS" "LOG_ODELAY"
       "LOG_NDELAY" "LOG_NOWAIT" "LOG_PERROR"

       ;; Disabled by default because they slow buffer loading
       ;; If you have use for them, uncomment the strings
       ;; that you want colored.
       ;; To compile, you may have to increase 'max-specpdl-size'

       ;; from other bundled extensions:
;        "CAL_EASTER_TO_xxx" "VT_NULL" "VT_EMPTY" "VT_UI1" "VT_I2"
;        "VT_I4" "VT_R4" "VT_R8" "VT_BOOL" "VT_ERROR" "VT_CY" "VT_DATE"
;        "VT_BSTR" "VT_DECIMAL" "VT_UNKNOWN" "VT_DISPATCH" "VT_VARIANT"
;        "VT_I1" "VT_UI2" "VT_UI4" "VT_INT" "VT_UINT" "VT_ARRAY"
;        "VT_BYREF" "CP_ACP" "CP_MACCP" "CP_OEMCP" "CP_SYMBOL"
;        "CP_THREAD_ACP" "CP_UTF7" "CP_UTF8" "CPDF_PM_NONE"
;        "CPDF_PM_OUTLINES" "CPDF_PM_THUMBS" "CPDF_PM_FULLSCREEN"
;        "CPDF_PL_SINGLE" "CPDF_PL_1COLUMN" "CPDF_PL_2LCOLUMN"
;        "CPDF_PL_2RCOLUMN" "CURLOPT_PORT" "CURLOPT_FILE"
;        "CURLOPT_INFILE" "CURLOPT_INFILESIZE" "CURLOPT_URL"
;        "CURLOPT_PROXY" "CURLOPT_VERBOSE" "CURLOPT_HEADER"
;        "CURLOPT_HTTPHEADER" "CURLOPT_NOPROGRESS" "CURLOPT_NOBODY"
;        "CURLOPT_FAILONERROR" "CURLOPT_UPLOAD" "CURLOPT_POST"
;        "CURLOPT_FTPLISTONLY" "CURLOPT_FTPAPPEND" "CURLOPT_NETRC"
;        "CURLOPT_FOLLOWLOCATION" "CURLOPT_FTPASCII" "CURLOPT_PUT"
;        "CURLOPT_MUTE" "CURLOPT_USERPWD" "CURLOPT_PROXYUSERPWD"
;        "CURLOPT_RANGE" "CURLOPT_TIMEOUT" "CURLOPT_POSTFIELDS"
;        "CURLOPT_REFERER" "CURLOPT_USERAGENT" "CURLOPT_FTPPORT"
;        "CURLOPT_LOW_SPEED_LIMIT" "CURLOPT_LOW_SPEED_TIME"
;        "CURLOPT_RESUME_FROM" "CURLOPT_COOKIE" "CURLOPT_SSLCERT"
;        "CURLOPT_SSLCERTPASSWD" "CURLOPT_WRITEHEADER"
;        "CURLOPT_COOKIEFILE" "CURLOPT_SSLVERSION"
;        "CURLOPT_TIMECONDITION" "CURLOPT_TIMEVALUE"
;        "CURLOPT_CUSTOMREQUEST" "CURLOPT_STDERR" "CURLOPT_TRANSFERTEXT"
;        "CURLOPT_RETURNTRANSFER" "CURLOPT_QUOTE" "CURLOPT_POSTQUOTE"
;        "CURLOPT_INTERFACE" "CURLOPT_KRB4LEVEL"
;        "CURLOPT_HTTPPROXYTUNNEL" "CURLOPT_FILETIME"
;        "CURLOPT_WRITEFUNCTION" "CURLOPT_READFUNCTION"
;        "CURLOPT_PASSWDFUNCTION" "CURLOPT_HEADERFUNCTION"
;        "CURLOPT_MAXREDIRS" "CURLOPT_MAXCONNECTS" "CURLOPT_CLOSEPOLICY"
;        "CURLOPT_FRESH_CONNECT" "CURLOPT_FORBID_REUSE"
;        "CURLOPT_RANDOM_FILE" "CURLOPT_EGDSOCKET"
;        "CURLOPT_CONNECTTIMEOUT" "CURLOPT_SSL_VERIFYPEER"
;        "CURLOPT_CAINFO" "CURLOPT_BINARYTRANSER"
;        "CURLCLOSEPOLICY_LEAST_RECENTLY_USED" "CURLCLOSEPOLICY_OLDEST"
;        "CURLINFO_EFFECTIVE_URL" "CURLINFO_HTTP_CODE"
;        "CURLINFO_HEADER_SIZE" "CURLINFO_REQUEST_SIZE"
;        "CURLINFO_TOTAL_TIME" "CURLINFO_NAMELOOKUP_TIME"
;        "CURLINFO_CONNECT_TIME" "CURLINFO_PRETRANSFER_TIME"
;        "CURLINFO_SIZE_UPLOAD" "CURLINFO_SIZE_DOWNLOAD"
;        "CURLINFO_SPEED_DOWNLOAD" "CURLINFO_SPEED_UPLOAD"
;        "CURLINFO_FILETIME" "CURLE_OK" "CURLE_UNSUPPORTED_PROTOCOL"
;        "CURLE_FAILED_INIT" "CURLE_URL_MALFORMAT"
;        "CURLE_URL_MALFORMAT_USER" "CURLE_COULDNT_RESOLVE_PROXY"
;        "CURLE_COULDNT_RESOLVE_HOST" "CURLE_COULDNT_CONNECT"
;        "CURLE_FTP_WEIRD_SERVER_REPLY" "CURLE_FTP_ACCESS_DENIED"
;        "CURLE_FTP_USER_PASSWORD_INCORRECT"
;        "CURLE_FTP_WEIRD_PASS_REPLY" "CURLE_FTP_WEIRD_USER_REPLY"
;        "CURLE_FTP_WEIRD_PASV_REPLY" "CURLE_FTP_WEIRD_227_FORMAT"
;        "CURLE_FTP_CANT_GET_HOST" "CURLE_FTP_CANT_RECONNECT"
;        "CURLE_FTP_COULDNT_SET_BINARY" "CURLE_PARTIAL_FILE"
;        "CURLE_FTP_COULDNT_RETR_FILE" "CURLE_FTP_WRITE_ERROR"
;        "CURLE_FTP_QUOTE_ERROR" "CURLE_HTTP_NOT_FOUND"
;        "CURLE_WRITE_ERROR" "CURLE_MALFORMAT_USER"
;        "CURLE_FTP_COULDNT_STOR_FILE" "CURLE_READ_ERROR"
;        "CURLE_OUT_OF_MEMORY" "CURLE_OPERATION_TIMEOUTED"
;        "CURLE_FTP_COULDNT_SET_ASCII" "CURLE_FTP_PORT_FAILED"
;        "CURLE_FTP_COULDNT_USE_REST" "CURLE_FTP_COULDNT_GET_SIZE"
;        "CURLE_HTTP_RANGE_ERROR" "CURLE_HTTP_POST_ERROR"
;        "CURLE_SSL_CONNECT_ERROR" "CURLE_FTP_BAD_DOWNLOAD_RESUME"
;        "CURLE_FILE_COULDNT_READ_FILE" "CURLE_LDAP_CANNOT_BIND"
;        "CURLE_LDAP_SEARCH_FAILED" "CURLE_LIBRARY_NOT_FOUND"
;        "CURLE_FUNCTION_NOT_FOUND" "CURLE_ABORTED_BY_CALLBACK"
;        "CURLE_BAD_FUNCTION_ARGUMENT" "CURLE_BAD_CALLING_ORDER"
;        "CURLE_HTTP_PORT_FAILED" "CURLE_BAD_PASSWORD_ENTERED"
;        "CURLE_TOO_MANY_REDIRECTS" "CURLE_UNKOWN_TELNET_OPTION"
;        "CURLE_TELNET_OPTION_SYNTAX" "CURLE_ALREADY_COMPLETE"
;        "DBX_MYSQL" "DBX_ODBC" "DBX_PGSQL" "DBX_MSSQL" "DBX_PERSISTENT"
;        "DBX_RESULT_INFO" "DBX_RESULT_INDEX" "DBX_RESULT_ASSOC"
;        "DBX_CMP_TEXT" "DBX_CMP_NUMBER" "XML_ELEMENT_NODE"
;        "XML_ATTRIBUTE_NODE" "XML_TEXT_NODE" "XML_CDATA_SECTION_NODE"
;        "XML_ENTITY_REF_NODE" "XML_ENTITY_NODE" "XML_PI_NODE"
;        "XML_COMMENT_NODE" "XML_DOCUMENT_NODE" "XML_DOCUMENT_TYPE_NODE"
;        "XML_DOCUMENT_FRAG_NODE" "XML_NOTATION_NODE"
;        "XML_HTML_DOCUMENT_NODE" "XML_DTD_NODE" "XML_ELEMENT_DECL_NODE"
;        "XML_ATTRIBUTE_DECL_NODE" "XML_ENTITY_DECL_NODE"
;        "XML_NAMESPACE_DECL_NODE" "XML_GLOBAL_NAMESPACE"
;        "XML_LOCAL_NAMESPACE" "XML_ATTRIBUTE_CDATA" "XML_ATTRIBUTE_ID"
;        "XML_ATTRIBUTE_IDREF" "XML_ATTRIBUTE_IDREFS"
;        "XML_ATTRIBUTE_ENTITY" "XML_ATTRIBUTE_NMTOKEN"
;        "XML_ATTRIBUTE_NMTOKENS" "XML_ATTRIBUTE_ENUMERATION"
;        "XML_ATTRIBUTE_NOTATION" "XPATH_UNDEFINED" "XPATH_NODESET"
;        "XPATH_BOOLEAN" "XPATH_NUMBER" "XPATH_STRING" "XPATH_POINT"
;        "XPATH_RANGE" "XPATH_LOCATIONSET" "XPATH_USERS" "FBSQL_ASSOC"
;        "FBSQL_NUM" "FBSQL_BOTH" "FDFValue" "FDFStatus" "FDFFile"
;        "FDFID" "FDFFf" "FDFSetFf" "FDFClearFf" "FDFFlags" "FDFSetF"
;        "FDFClrF" "FDFAP" "FDFAS" "FDFAction" "FDFAA" "FDFAPRef"
;        "FDFIF" "FDFEnter" "FDFExit" "FDFDown" "FDFUp" "FDFFormat"
;        "FDFValidate" "FDFKeystroke" "FDFCalculate"
;        "FRIBIDI_CHARSET_UTF8" "FRIBIDI_CHARSET_8859_6"
;        "FRIBIDI_CHARSET_8859_8" "FRIBIDI_CHARSET_CP1255"
;        "FRIBIDI_CHARSET_CP1256" "FRIBIDI_CHARSET_ISIRI_3342"
;        "FTP_ASCII" "FTP_BINARY" "FTP_IMAGE" "FTP_TEXT" "IMG_GIF"
;        "IMG_JPG" "IMG_JPEG" "IMG_PNG" "IMG_WBMP" "IMG_COLOR_TILED"
;        "IMG_COLOR_STYLED" "IMG_COLOR_BRUSHED"
;        "IMG_COLOR_STYLEDBRUSHED" "IMG_COLOR_TRANSPARENT"
;        "IMG_ARC_ROUNDED" "IMG_ARC_PIE" "IMG_ARC_CHORD"
;        "IMG_ARC_NOFILL" "IMG_ARC_EDGED" "GMP_ROUND_ZERO"
;        "GMP_ROUND_PLUSINF" "GMP_ROUND_MINUSINF" "HW_ATTR_LANG"
;        "HW_ATTR_NR" "HW_ATTR_NONE" "IIS_READ" "IIS_WRITE"
;        "IIS_EXECUTE" "IIS_SCRIPT" "IIS_ANONYMOUS" "IIS_BASIC"
;        "IIS_NTLM" "NIL" "OP_DEBUG" "OP_READONLY" "OP_ANONYMOUS"
;        "OP_SHORTCACHE" "OP_SILENT" "OP_PROTOTYPE" "OP_HALFOPEN"
;        "OP_EXPUNGE" "OP_SECURE" "CL_EXPUNGE" "FT_UID" "FT_PEEK"
;        "FT_NOT" "FT_INTERNAL" "FT_PREFETCHTEXT" "ST_UID" "ST_SILENT"
;        "ST_SET" "CP_UID" "CP_MOVE" "SE_UID" "SE_FREE" "SE_NOPREFETCH"
;        "SO_FREE" "SO_NOSERVER" "SA_MESSAGES" "SA_RECENT" "SA_UNSEEN"
;        "SA_UIDNEXT" "SA_UIDVALIDITY" "SA_ALL" "LATT_NOINFERIORS"
;        "LATT_NOSELECT" "LATT_MARKED" "LATT_UNMARKED" "SORTDATE"
;        "SORTARRIVAL" "SORTFROM" "SORTSUBJECT" "SORTTO" "SORTCC"
;        "SORTSIZE" "TYPETEXT" "TYPEMULTIPART" "TYPEMESSAGE"
;        "TYPEAPPLICATION" "TYPEAUDIO" "TYPEIMAGE" "TYPEVIDEO"
;        "TYPEOTHER" "ENC7BIT" "ENC8BIT" "ENCBINARY" "ENCBASE64"
;        "ENCQUOTEDPRINTABLE" "ENCOTHER" "INGRES_ASSOC" "INGRES_NUM"
;        "INGRES_BOTH" "IBASE_DEFAULT" "IBASE_TEXT" "IBASE_UNIXTIME"
;        "IBASE_READ" "IBASE_COMMITTED" "IBASE_CONSISTENCY"
;        "IBASE_NOWAIT" "IBASE_TIMESTAMP" "IBASE_DATE" "IBASE_TIME"
;        "LDAP_DEREF_NEVER" "LDAP_DEREF_SEARCHING" "LDAP_DEREF_FINDING"
;        "LDAP_DEREF_ALWAYS" "LDAP_OPT_DEREF" "LDAP_OPT_SIZELIMIT"
;        "LDAP_OPT_TIMELIMIT" "LDAP_OPT_PROTOCOL_VERSION"
;        "LDAP_OPT_ERROR_NUMBER" "LDAP_OPT_REFERRALS" "LDAP_OPT_RESTART"
;        "LDAP_OPT_HOST_NAME" "LDAP_OPT_ERROR_STRING"
;        "LDAP_OPT_MATCHED_DN" "LDAP_OPT_SERVER_CONTROLS"
;        "LDAP_OPT_CLIENT_CONTROLS" "GSLC_SSL_NO_AUTH"
;        "GSLC_SSL_ONEWAY_AUTH" "GSLC_SSL_TWOWAY_AUTH" "MCAL_SUNDAY"
;        "MCAL_MONDAY" "MCAL_TUESDAY" "MCAL_WEDNESDAY" "MCAL_THURSDAY"
;        "MCAL_FRIDAY" "MCAL_SATURDAY" "MCAL_JANUARY" "MCAL_FEBRUARY"
;        "MCAL_MARCH" "MCAL_APRIL" "MCAL_MAY" "MCAL_JUNE" "MCAL_JULY"
;        "MCAL_AUGUST" "MCAL_SEPTEMBER" "MCAL_OCTOBER" "MCAL_NOVEMBER"
;        "MCAL_RECUR_NONE" "MCAL_RECUR_DAILY" "MCAL_RECUR_WEEKLY"
;        "MCAL_RECUR_MONTHLY_MDAY" "MCAL_RECUR_MONTHLY_WDAY"
;        "MCAL_RECUR_YEARLY" "MCAL_M_SUNDAY" "MCAL_M_MONDAY"
;        "MCAL_M_TUESDAY" "MCAL_M_WEDNESDAY" "MCAL_M_THURSDAY"
;        "MCAL_M_FRIDAY" "MCAL_M_SATURDAY" "MCAL_M_WEEKDAYS"
;        "MCAL_M_WEEKEND" "MCAL_M_ALLDAYS" "MCRYPT_" "MCRYPT_"
;        "MCRYPT_ENCRYPT" "MCRYPT_DECRYPT" "MCRYPT_DEV_RANDOM"
;        "MCRYPT_DEV_URANDOM" "MCRYPT_RAND" "SWFBUTTON_HIT"
;        "SUNFUNCS_RET_STRING" "SUNFUNCS_RET_DOUBLE"
;        "SWFBUTTON_DOWN" "SWFBUTTON_OVER" "SWFBUTTON_UP"
;        "SWFBUTTON_MOUSEUPOUTSIDE" "SWFBUTTON_DRAGOVER"
;        "SWFBUTTON_DRAGOUT" "SWFBUTTON_MOUSEUP" "SWFBUTTON_MOUSEDOWN"
;        "SWFBUTTON_MOUSEOUT" "SWFBUTTON_MOUSEOVER"
;        "SWFFILL_RADIAL_GRADIENT" "SWFFILL_LINEAR_GRADIENT"
;        "SWFFILL_TILED_BITMAP" "SWFFILL_CLIPPED_BITMAP"
;        "SWFTEXTFIELD_HASLENGTH" "SWFTEXTFIELD_NOEDIT"
;        "SWFTEXTFIELD_PASSWORD" "SWFTEXTFIELD_MULTILINE"
;        "SWFTEXTFIELD_WORDWRAP" "SWFTEXTFIELD_DRAWBOX"
;        "SWFTEXTFIELD_NOSELECT" "SWFTEXTFIELD_HTML"
;        "SWFTEXTFIELD_ALIGN_LEFT" "SWFTEXTFIELD_ALIGN_RIGHT"
;        "SWFTEXTFIELD_ALIGN_CENTER" "SWFTEXTFIELD_ALIGN_JUSTIFY"
;        "UDM_FIELD_URLID" "UDM_FIELD_URL" "UDM_FIELD_CONTENT"
;        "UDM_FIELD_TITLE" "UDM_FIELD_KEYWORDS" "UDM_FIELD_DESC"
;        "UDM_FIELD_DESCRIPTION" "UDM_FIELD_TEXT" "UDM_FIELD_SIZE"
;        "UDM_FIELD_RATING" "UDM_FIELD_SCORE" "UDM_FIELD_MODIFIED"
;        "UDM_FIELD_ORDER" "UDM_FIELD_CRC" "UDM_FIELD_CATEGORY"
;        "UDM_PARAM_PAGE_SIZE" "UDM_PARAM_PAGE_NUM"
;        "UDM_PARAM_SEARCH_MODE" "UDM_PARAM_CACHE_MODE"
;        "UDM_PARAM_TRACK_MODE" "UDM_PARAM_PHRASE_MODE"
;        "UDM_PARAM_CHARSET" "UDM_PARAM_STOPTABLE"
;        "UDM_PARAM_STOP_TABLE" "UDM_PARAM_STOPFILE"
;        "UDM_PARAM_STOP_FILE" "UDM_PARAM_WEIGHT_FACTOR"
;        "UDM_PARAM_WORD_MATCH" "UDM_PARAM_MAX_WORD_LEN"
;        "UDM_PARAM_MAX_WORDLEN" "UDM_PARAM_MIN_WORD_LEN"
;        "UDM_PARAM_MIN_WORDLEN" "UDM_PARAM_ISPELL_PREFIXES"
;        "UDM_PARAM_ISPELL_PREFIX" "UDM_PARAM_PREFIXES"
;        "UDM_PARAM_PREFIX" "UDM_PARAM_CROSS_WORDS"
;        "UDM_PARAM_CROSSWORDS" "UDM_LIMIT_CAT" "UDM_LIMIT_URL"
;        "UDM_LIMIT_TAG" "UDM_LIMIT_LANG" "UDM_LIMIT_DATE"
;        "UDM_PARAM_FOUND" "UDM_PARAM_NUM_ROWS" "UDM_PARAM_WORDINFO"
;        "UDM_PARAM_WORD_INFO" "UDM_PARAM_SEARCHTIME"
;        "UDM_PARAM_SEARCH_TIME" "UDM_PARAM_FIRST_DOC"
;        "UDM_PARAM_LAST_DOC" "UDM_MODE_ALL" "UDM_MODE_ANY"
;        "UDM_MODE_BOOL" "UDM_MODE_PHRASE" "UDM_CACHE_ENABLED"
;        "UDM_CACHE_DISABLED" "UDM_TRACK_ENABLED" "UDM_TRACK_DISABLED"
;        "UDM_PHRASE_ENABLED" "UDM_PHRASE_DISABLED"
;        "UDM_CROSS_WORDS_ENABLED" "UDM_CROSSWORDS_ENABLED"
;        "UDM_CROSS_WORDS_DISABLED" "UDM_CROSSWORDS_DISABLED"
;        "UDM_PREFIXES_ENABLED" "UDM_PREFIX_ENABLED"
;        "UDM_ISPELL_PREFIXES_ENABLED" "UDM_ISPELL_PREFIX_ENABLED"
;        "UDM_PREFIXES_DISABLED" "UDM_PREFIX_DISABLED"
;        "UDM_ISPELL_PREFIXES_DISABLED" "UDM_ISPELL_PREFIX_DISABLED"
;        "UDM_ISPELL_TYPE_AFFIX" "UDM_ISPELL_TYPE_SPELL"
;        "UDM_ISPELL_TYPE_DB" "UDM_ISPELL_TYPE_SERVER" "UDM_MATCH_WORD"
;        "UDM_MATCH_BEGIN" "UDM_MATCH_SUBSTR" "UDM_MATCH_END"
;        "MSQL_ASSOC" "MSQL_NUM" "MSQL_BOTH" "MYSQL_ASSOC" "MYSQL_NUM"
;        "MYSQL_BOTH" "MYSQL_USE_RESULT" "MYSQL_STORE_RESULT"
;        "OCI_DEFAULT" "OCI_DESCRIBE_ONLY" "OCI_COMMIT_ON_SUCCESS"
;        "OCI_EXACT_FETCH" "SQLT_BFILEE" "SQLT_CFILEE" "SQLT_CLOB"
;        "SQLT_BLOB" "SQLT_RDD" "OCI_B_SQLT_NTY" "OCI_SYSDATE"
;        "OCI_B_BFILE" "OCI_B_CFILEE" "OCI_B_CLOB" "OCI_B_BLOB"
;        "OCI_B_ROWID" "OCI_B_CURSOR" "OCI_B_BIN" "OCI_ASSOC" "OCI_NUM"
;        "OCI_BOTH" "OCI_RETURN_NULLS" "OCI_RETURN_LOBS"
;        "OCI_DTYPE_FILE" "OCI_DTYPE_LOB" "OCI_DTYPE_ROWID" "OCI_D_FILE"
;        "OCI_D_LOB" "OCI_D_ROWID" "ODBC_TYPE" "ODBC_BINMODE_PASSTHRU"
;        "ODBC_BINMODE_RETURN" "ODBC_BINMODE_CONVERT" "SQL_ODBC_CURSORS"
;        "SQL_CUR_USE_DRIVER" "SQL_CUR_USE_IF_NEEDED" "SQL_CUR_USE_ODBC"
;        "SQL_CONCURRENCY" "SQL_CONCUR_READ_ONLY" "SQL_CONCUR_LOCK"
;        "SQL_CONCUR_ROWVER" "SQL_CONCUR_VALUES" "SQL_CURSOR_TYPE"
;        "SQL_CURSOR_FORWARD_ONLY" "SQL_CURSOR_KEYSET_DRIVEN"
;        "SQL_CURSOR_DYNAMIC" "SQL_CURSOR_STATIC" "SQL_KEYSET_SIZE"
;        "SQL_CHAR" "SQL_VARCHAR" "SQL_LONGVARCHAR" "SQL_DECIMAL"
;        "SQL_NUMERIC" "SQL_BIT" "SQL_TINYINT" "SQL_SMALLINT"
;        "SQL_INTEGER" "SQL_BIGINT" "SQL_REAL" "SQL_FLOAT" "SQL_DOUBLE"
;        "SQL_BINARY" "SQL_VARBINARY" "SQL_LONGVARBINARY" "SQL_DATE"
;        "SQL_TIME" "SQL_TIMESTAMP" "SQL_TYPE_DATE" "SQL_TYPE_TIME"
;        "SQL_TYPE_TIMESTAMP" "SQL_BEST_ROWID" "SQL_ROWVER"
;        "SQL_SCOPE_CURROW" "SQL_SCOPE_TRANSACTION" "SQL_SCOPE_SESSION"
;        "SQL_NO_NULLS" "SQL_NULLABLE" "SQL_INDEX_UNIQUE"
;        "SQL_INDEX_ALL" "SQL_ENSURE" "SQL_QUICK"
;        "X509_PURPOSE_SSL_CLIENT" "X509_PURPOSE_SSL_SERVER"
;        "X509_PURPOSE_NS_SSL_SERVER" "X509_PURPOSE_SMIME_SIGN"
;        "X509_PURPOSE_SMIME_ENCRYPT" "X509_PURPOSE_CRL_SIGN"
;        "X509_PURPOSE_ANY" "PKCS7_DETACHED" "PKCS7_TEXT"
;        "PKCS7_NOINTERN" "PKCS7_NOVERIFY" "PKCS7_NOCHAIN"
;        "PKCS7_NOCERTS" "PKCS7_NOATTR" "PKCS7_BINARY" "PKCS7_NOSIGS"
;        "OPENSSL_PKCS1_PADDING" "OPENSSL_SSLV23_PADDING"
;        "OPENSSL_NO_PADDING" "OPENSSL_PKCS1_OAEP_PADDING"
;        "ORA_BIND_INOUT" "ORA_BIND_IN" "ORA_BIND_OUT"
;        "ORA_FETCHINTO_ASSOC" "ORA_FETCHINTO_NULLS"
;        "PREG_PATTERN_ORDER" "PREG_SET_ORDER" "PREG_SPLIT_NO_EMPTY"
;        "PREG_SPLIT_DELIM_CAPTURE"
;        "PGSQL_ASSOC" "PGSQL_NUM" "PGSQL_BOTH"
;        "PRINTER_COPIES" "PRINTER_MODE" "PRINTER_TITLE"
;        "PRINTER_DEVICENAME" "PRINTER_DRIVERVERSION"
;        "PRINTER_RESOLUTION_Y" "PRINTER_RESOLUTION_X" "PRINTER_SCALE"
;        "PRINTER_BACKGROUND_COLOR" "PRINTER_PAPER_LENGTH"
;        "PRINTER_PAPER_WIDTH" "PRINTER_PAPER_FORMAT"
;        "PRINTER_FORMAT_CUSTOM" "PRINTER_FORMAT_LETTER"
;        "PRINTER_FORMAT_LEGAL" "PRINTER_FORMAT_A3" "PRINTER_FORMAT_A4"
;        "PRINTER_FORMAT_A5" "PRINTER_FORMAT_B4" "PRINTER_FORMAT_B5"
;        "PRINTER_FORMAT_FOLIO" "PRINTER_ORIENTATION"
;        "PRINTER_ORIENTATION_PORTRAIT" "PRINTER_ORIENTATION_LANDSCAPE"
;        "PRINTER_TEXT_COLOR" "PRINTER_TEXT_ALIGN" "PRINTER_TA_BASELINE"
;        "PRINTER_TA_BOTTOM" "PRINTER_TA_TOP" "PRINTER_TA_CENTER"
;        "PRINTER_TA_LEFT" "PRINTER_TA_RIGHT" "PRINTER_PEN_SOLID"
;        "PRINTER_PEN_DASH" "PRINTER_PEN_DOT" "PRINTER_PEN_DASHDOT"
;        "PRINTER_PEN_DASHDOTDOT" "PRINTER_PEN_INVISIBLE"
;        "PRINTER_BRUSH_SOLID" "PRINTER_BRUSH_CUSTOM"
;        "PRINTER_BRUSH_DIAGONAL" "PRINTER_BRUSH_CROSS"
;        "PRINTER_BRUSH_DIAGCROSS" "PRINTER_BRUSH_FDIAGONAL"
;        "PRINTER_BRUSH_HORIZONTAL" "PRINTER_BRUSH_VERTICAL"
;        "PRINTER_FW_THIN" "PRINTER_FW_ULTRALIGHT" "PRINTER_FW_LIGHT"
;        "PRINTER_FW_NORMAL" "PRINTER_FW_MEDIUM" "PRINTER_FW_BOLD"
;        "PRINTER_FW_ULTRABOLD" "PRINTER_FW_HEAVY" "PRINTER_ENUM_LOCAL"
;        "PRINTER_ENUM_NAME" "PRINTER_ENUM_SHARED"
;        "PRINTER_ENUM_DEFAULT" "PRINTER_ENUM_CONNECTIONS"
;        "PRINTER_ENUM_NETWORK" "PRINTER_ENUM_REMOTE" "PSPELL_FAST"
;        "PSPELL_NORMAL" "PSPELL_BAD_SPELLERS" "PSPELL_RUN_TOGETHER"
;        "SID" "SID" "AF_UNIX" "AF_INET" "SOCK_STREAM" "SOCK_DGRAM"
;        "SOCK_RAW" "SOCK_SEQPACKET" "SOCK_RDM" "MSG_OOB" "MSG_WAITALL"
;        "MSG_PEEK" "MSG_DONTROUTE" "SO_DEBUG" "SO_REUSEADDR"
;        "SO_KEEPALIVE" "SO_DONTROUTE" "SO_LINGER" "SO_BROADCAST"
;        "SO_OOBINLINE" "SO_SNDBUF" "SO_RCVBUF" "SO_SNDLOWAT"
;        "SO_RCVLOWAT" "SO_SNDTIMEO" "SO_RCVTIMEO" "SO_TYPE" "SO_ERROR"
;        "SOL_SOCKET" "PHP_NORMAL_READ" "PHP_BINARY_READ"
;        "PHP_SYSTEM_READ" "SOL_TCP" "SOL_UDP" "MOD_COLOR" "MOD_MATRIX"
;        "TYPE_PUSHBUTTON" "TYPE_MENUBUTTON" "BSHitTest" "BSDown"
;        "BSOver" "BSUp" "OverDowntoIdle" "IdletoOverDown"
;        "OutDowntoIdle" "OutDowntoOverDown" "OverDowntoOutDown"
;        "OverUptoOverDown" "OverUptoIdle" "IdletoOverUp" "ButtonEnter"
;        "ButtonExit" "MenuEnter" "MenuExit" "XML_ERROR_NONE"
;        "XML_ERROR_NO_MEMORY" "XML_ERROR_SYNTAX"
;        "XML_ERROR_NO_ELEMENTS" "XML_ERROR_INVALID_TOKEN"
;        "XML_ERROR_UNCLOSED_TOKEN" "XML_ERROR_PARTIAL_CHAR"
;        "XML_ERROR_TAG_MISMATCH" "XML_ERROR_DUPLICATE_ATTRIBUTE"
;        "XML_ERROR_JUNK_AFTER_DOC_ELEMENT" "XML_ERROR_PARAM_ENTITY_REF"
;        "XML_ERROR_UNDEFINED_ENTITY" "XML_ERROR_RECURSIVE_ENTITY_REF"
;        "XML_ERROR_ASYNC_ENTITY" "XML_ERROR_BAD_CHAR_REF"
;        "XML_ERROR_BINARY_ENTITY_REF"
;        "XML_ERROR_ATTRIBUTE_EXTERNAL_ENTITY_REF"
;        "XML_ERROR_MISPLACED_XML_PI" "XML_ERROR_UNKNOWN_ENCODING"
;        "XML_ERROR_INCORRECT_ENCODING"
;        "XML_ERROR_UNCLOSED_CDATA_SECTION"
;        "XML_ERROR_EXTERNAL_ENTITY_HANDLING" "XML_OPTION_CASE_FOLDING"
;        "XML_OPTION_TARGET_ENCODING" "XML_OPTION_SKIP_TAGSTART"
;        "XML_OPTION_SKIP_WHITE" "YPERR_BADARGS" "YPERR_BADDB"
;        "YPERR_BUSY" "YPERR_DOMAIN" "YPERR_KEY" "YPERR_MAP"
;        "YPERR_NODOM" "YPERR_NOMORE" "YPERR_PMAP" "YPERR_RESRC"
;        "YPERR_RPC" "YPERR_YPBIND" "YPERR_YPERR" "YPERR_YPSERV"
;        "YPERR_VERS" "FORCE_GZIP" "FORCE_DEFLATE"

       ;; PEAR constants
;        "PEAR_ERROR_RETURN" "PEAR_ERROR_PRINT" "PEAR_ERROR_TRIGGER"
;        "PEAR_ERROR_DIE" "PEAR_ERROR_CALLBACK" "OS_WINDOWS" "OS_UNIX"
;        "PEAR_OS" "DB_OK" "DB_ERROR" "DB_ERROR_SYNTAX"
;        "DB_ERROR_CONSTRAINT" "DB_ERROR_NOT_FOUND"
;        "DB_ERROR_ALREADY_EXISTS" "DB_ERROR_UNSUPPORTED"
;        "DB_ERROR_MISMATCH" "DB_ERROR_INVALID" "DB_ERROR_NOT_CAPABLE"
;        "DB_ERROR_TRUNCATED" "DB_ERROR_INVALID_NUMBER"
;        "DB_ERROR_INVALID_DATE" "DB_ERROR_DIVZERO"
;        "DB_ERROR_NODBSELECTED" "DB_ERROR_CANNOT_CREATE"
;        "DB_ERROR_CANNOT_DELETE" "DB_ERROR_CANNOT_DROP"
;        "DB_ERROR_NOSUCHTABLE" "DB_ERROR_NOSUCHFIELD"
;        "DB_ERROR_NEED_MORE_DATA" "DB_ERROR_NOT_LOCKED"
;        "DB_ERROR_VALUE_COUNT_ON_ROW" "DB_ERROR_INVALID_DSN"
;        "DB_ERROR_CONNECT_FAILED" "DB_WARNING" "DB_WARNING_READ_ONLY"
;        "DB_PARAM_SCALAR" "DB_PARAM_OPAQUE" "DB_BINMODE_PASSTHRU"
;        "DB_BINMODE_RETURN" "DB_BINMODE_CONVERT" "DB_FETCHMODE_DEFAULT"
;        "DB_FETCHMODE_ORDERED" "DB_FETCHMODE_ASSOC"
;        "DB_FETCHMODE_FLIPPED" "DB_GETMODE_ORDERED" "DB_GETMODE_ASSOC"
;        "DB_GETMODE_FLIPPED" "DB_TABLEINFO_ORDER"
;        "DB_TABLEINFO_ORDERTABLE" "DB_TABLEINFO_FULL"

       )))
  "PHP constants.")

(defconst php-keywords
  (eval-when-compile
    (regexp-opt
     ;; "class", "new" and "extends" get special treatment
     ;; "case" and "default" get special treatment elsewhere
     '("and" "as" "break" "continue" "declare" "do" "echo" "else" "elseif"
       "endfor" "endforeach" "endif" "endswitch" "endwhile" "exit"
       "extends" "for" "foreach" "global" "if" "include" "include_once"
       "next" "or" "require" "require_once" "return" "static" "switch"
       "then" "var" "while" "xor" "throw" "catch" "try"
       "instanceof" "catch all" "finally")))
  "PHP keywords.")

;; (defconst php-identifier
;;   (eval-when-compile
;;     '"[a-zA-Z\_\x7f-\xff][a-zA-Z0-9\_\x7f-\xff]*")
;;   "Characters in a PHP identifier.")

(defconst php-types
  (eval-when-compile
    (regexp-opt '("array" "bool" "boolean" "char" "const" "double" "float"
                  "int" "integer" "long" "mixed" "object" "real"
                  "string")))
  "PHP types.")

(defconst php-superglobals
  (eval-when-compile
    (regexp-opt '("_GET" "_POST" "_COOKIE" "_SESSION" "_ENV" "GLOBALS"
                  "_SERVER" "_FILES" "_REQUEST")))
  "PHP superglobal variables.")

;; Set up font locking
(defconst php-font-lock-keywords-1
  (list
   ;; Fontify constants
   (cons
    (concat "[^_$]?\\<\\(" php-constants "\\)\\>[^_]?")
    '(1 font-lock-constant-face))

   ;; Fontify keywords
   (cons
    (concat "[^_$]?\\<\\(" php-keywords "\\)\\>[^_]?")
    '(1 font-lock-keyword-face))

   ;; Fontify keywords and targets, and case default tags.
   '("\\<\\(default\\):" (1 font-lock-keyword-face))
   '("\\<\\(break\\|case\\|continue\\)\\>\\s-+\\(-?\\sw+\\)?"
     (1 font-lock-keyword-face) (2 font-lock-constant-face t t))
   ;; This must come after the one for keywords and targets.
   '(":" ("^\\s-+\\(\\sw+\\)\\s-+\\s-+$"
          (beginning-of-line) (end-of-line)
          (1 font-lock-constant-face)))

   ;; treat 'print' as keyword only when not used like a function name
   '("\\<\\(print\\|die\\)\\s-*(" (1 php+-default-face))
   '("\\<\\(print\\|die\\)\\>" (1 font-lock-keyword-face))

   ;; Fontify PHP tag
   (cons php-tags-key font-lock-preprocessor-face)

   ;; Fontify ASP-style tag
   '("<\\%\\(=\\)?" . font-lock-preprocessor-face)
   '("\\%>" . font-lock-preprocessor-face))
  "Subdued level highlighting for PHP mode.")

(defconst php-font-lock-keywords-2
  (append
   php-font-lock-keywords-1
   (list
    ;; namespace declarations/usage
    '("\\<\\(namespace\\|use\\)\\s-+\\(\\sw+\\)"
      (1 font-lock-keyword-face) 
      (2 font-lock-type-face nil t))

    '("\\\\\\(namespace \\|use \\)" (1 font-lock-type-face t t))
    '("\\\\\\([^$]\\(\\sw+\\)\\)" (1 font-lock-type-face nil t))

    ;; class declaration
    '("\\<\\(class\\|interface\\|trait\\)\\s-+\\(\\sw+\\)?"
      (1 font-lock-keyword-face) (2 font-lock-type-face nil t))
    ;; handle several words specially, to include following word,
    ;; thereby excluding it from unknown-symbol checks later
    '("\\<\\(new\\|extends\\|implements\\|instanceof\\)\\(?:\\s-\\|[\n]\\)+\\$?\\(\\(\\(\\sw\\|\\s_\\|\\\\\\)+\\(,\\(\\s-\\|[\n]\\)*\\)?\\)+\\)"
      (1 font-lock-keyword-face) (2 font-lock-type-face))

    ;; function declaration
    '("\\<\\(function\\)\\s-+&?\\(\\sw+\\)?\\s-*("
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t))

    ;; class hierarchy
    '("\\<\\(self\\|parent\\)\\>" (1 font-lock-constant-face nil nil))

    ;; method and variable features
    '("\\<\\(private\\|protected\\|public\\)\\s-+\\$?\\sw+"
      (1 font-lock-keyword-face))

    ;; method features
    '("^\\s-*\\(abstract\\|static\\|final\\)\\s-+\\$?\\sw+"
      (1 font-lock-keyword-face))

    ;; variable features
    '("^\\s-*\\(static\\|const\\)\\s-+\\$?\\(\\(\\sw\\|\\s_\\)+\\)"
      (1 font-lock-keyword-face))

    ;; constant declarations
    '("^\\s-*const\\s-+\\(\\(\\sw\\|\\s_\\)+\\)"
      (1 font-lock-constant-face))))
  "Medium level highlighting for PHP mode.")

(defconst php-font-lock-keywords-3
  (append
   php-font-lock-keywords-2
   (list

    ;; <word> or </word> for HTML
    ;;'("</?\\sw+[^> ]*>" . font-lock-constant-face)
    ;;'("</?\\sw+[^>]*" . font-lock-constant-face)
    ;;'("<!DOCTYPE" . font-lock-constant-face)

    ;; '("</?[a-z!:]+" . font-lock-constant-face)

    ;; ;; HTML >
    ;; '("<[^>]*\\(>\\)" (1 font-lock-constant-face))

    ;; ;; HTML tags
    ;; '("\\(<[a-z]+\\)[[:space:]]+\\([a-z:]+=\\)[^>]*?" (1 font-lock-constant-face) (2 font-lock-constant-face) )
    ;; '("\"[[:space:]]+\\([a-z:]+=\\)" (1 font-lock-constant-face))

    ;; HTML entities
    ;;'("&\\w+;" . font-lock-variable-name-face)

    ;; ;; warn about '$' immediately after ->
    ;; '("\\$\\sw+->\\s-*\\(\\$\\)\\(\\sw+\\)"
    ;;   (1 font-lock-warning-face) (2 php+-default-face))

    ;; warn about $word.word -- it could be a valid concatenation,
    ;; but without any spaces we'll assume $word->word was meant.
    '("\\$\\sw+\\(\\.\\)\\sw"
      1 font-lock-warning-face)

    ;; Warn about ==> instead of =>
    '("==+>" . font-lock-warning-face)

    ;; exclude casts from bare-word treatment (may contain spaces)
    `(,(concat "(\\s-*\\(" php-types "\\)\\s-*)")
      1 font-lock-type-face)

    ;; PHP5: function declarations may contain classes as parameters type
    `(,(concat "[(,]?\\s-*\\(\\sw+\\)\\s-+&?\\$\\sw+\\>")
      1 font-lock-type-face)

    ;; Fontify variables and function calls
    '("\\$\\(this\\|that\\)\\W" (1 font-lock-constant-face nil nil))
;;    `(,(concat "\\$\\(" php-superglobals "\\)\\W")
;;      (1 font-lock-constant-face nil nil)) ;; $_GET & co
    '("\\$\\(\\sw+\\)" (1 font-lock-variable-name-face)) ;; $variable
    '("\\$\\(\\$\\)" (1 php+-default-face t t)) ;; $$variable
    '("->\\(\\sw+\\)\\s-*(" . (1 php+-default-face)) ;; ->function_call
    '("->\\(\\sw+\\)" (1 font-lock-variable-name-face)) ;; ->variable
    '("\\(\\sw+\\)::\\sw+\\s-*(?" . (1 font-lock-type-face)) ;; class::member
    '("::\\(\\sw+\\>[^(]\\)" . (1 php+-default-face)) ;; class::constant
    '("\\<\\\\?[^$]\\sw+[[(]" . (0 php+-default-face t)) ;; word( or word[
    '("\\<[0-9]+" . php+-default-face) ;; number (also matches word)

    '("new \\\\?\\(\\sw+\\)(" (1 font-lock-type-face t t))

    ;; Warn on any words not already fontified
    ;; '("\\<\\sw+\\>" . font-lock-warning-face)

    ;; HTML
    ;; `(,(concat "\\(</?\\sw+\\(\\(?:\\s-\\|\n\\)+\\(?:\\sw\\|-\\)+"
    ;;            "\\(?:\\(?:\\s-\\|\n\\)*=\\(?:\\s-\\|\n\\)*"
    ;;            "\\(\\([\"']?\\).*?\\4\\)\\)?\\)*\\(?:\\s-\\|\n\\)*/?>\\)")
    ;;   (1 font-lock-builtin-name-face t t)
    ;;   (2 font-lock-variable-name-face t t)
    ;;   (3 font-lock-constant-face t t))
    ;; '("</?\\(?:\\sw\\|-\\)+" (0 font-lock-builtin-face t t))
    ;; '("/?>" (0 font-lock-builtin-face t t))

    ;; ;; CSS
    ;; '("[^\"']\\<\\(\\(?:\\sw\\|-\\)+\\)\\([:!#]\\)\\s-*\\([^<;,{]*\\)" 
    ;;   (1 font-lock-variable-name-face t t)
    ;;   (2 nil t t)
    ;;   (3 font-lock-constant-face t t))
    ;; '("[{};,]" (0 nil t t))
    
    ;; Everything else should be constants
    '("\\<\\sw+\\>" . font-lock-constant-face)
    ))
  "Gauchy level highlighting for PHP mode.")

(provide 'php-const)
