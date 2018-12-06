
skewer.reloadStylesheets = (function () {
    'use strict';

    // Constants.
    var QUERY_PARAM_NAME = 'skewer-reload-stylesheets-ts';

    // Local functions.
    var endsWith = function (str, suffix) {
        return str.indexOf(suffix, str.length - suffix.length) !== -1;
    };

    /**
     * Ensure `link`'s associated contents are refreshed from server.
     *
     * We do this by adding (or updating) a query parameter to the link tag's
     * href attribute.
     *
     * @param {Element} link - link tag to refresh from server.
     */
    var reloadLinkHref = function (link) {
        var href = link.getAttribute('href'),
            param_pos = href.indexOf(QUERY_PARAM_NAME),
            query_param = QUERY_PARAM_NAME + '=' + new Date().getTime(),
            new_href,
            query_pos;

        query_pos = href.indexOf('?');

        if (query_pos > -1) {
            if (param_pos > -1) {
                // Replace our param in the query string.
                new_href = href.replace(new RegExp(QUERY_PARAM_NAME + '=\\d+'),
                                               query_param);
            } else {
                // Add our param to a pre-existing query string.
                new_href = href + '&' + query_param;
            }
        } else {
            // Add a query string with our param.
            new_href = href + '?' + query_param;
        }

        link.setAttribute('href', new_href);
    };

    // Reload any stylesheet imported from `path` by adding (or updating) a
    // skewer_timestamp query param to its href attribute.
    // Return false if no stylesheet was found, or true if it was.
    var reloadStylesheet = function (path) {
        // Find the <link> tag importing the stylesheet at `path` (if any).
        var link_elts = document.getElementsByTagName('link');

        var target_link = null;
        var cur_href = null;
        var query_pos;
        var max_href_len = 0;

        // The link with the longest match for path's ending wins, to account
        // for the cases like the following:
        //
        //     <link href="/css/main.css" />
        //     <link href="/bazinator/plugins/bar/css/main.css" />
        for (var i = 0; i < link_elts.length; i++) {
            cur_href = link_elts[i].getAttribute('href');

            // Drop query string for file path matching.
            query_pos = cur_href.indexOf('?');
            if (query_pos > -1) {
                cur_href = cur_href.substring(0, query_pos);
            }

            if (endsWith(path, cur_href) && cur_href.length > max_href_len) {
                target_link = link_elts[i];
                max_href_len = cur_href.length;
            }
        }

        if (target_link === null) {
            return false;
        }

        reloadLinkHref(target_link);

        return true;
    };

    /**
     * Reload every linked stylesheet in the current document.
     */
    var reloadAllStylesheets = function () {
        var link_elts = document.getElementsByTagName('link');

        for (var i = 0; i < link_elts.length; i++) {
            reloadLinkHref(link_elts[i]);
        }
    };

    return {
        reload: reloadStylesheet,
        reloadAll: reloadAllStylesheets
    };
})();
