// Reload any stylesheet imported from `path` by adding (or updating) a
// skewer_timestamp query param to its href attribute.
// Return false if no stylesheet was found, or true if it was.
skewer.reloadStylesheet = function(path) {
    'use strict';

    // Constants.
    var QUERY_PARAM_NAME = 'skewer-reload-stylesheets-ts';

    // Local functions.
    var endsWith = function(str, suffix) {
        return str.indexOf(suffix, str.length - suffix.length) !== -1;
    }

    // Find the <link> tag importing the stylesheet at `path` (if any).
    var link_elts = document.getElementsByTagName('link');

    var target_link = null;
    var cur_href = null;
    var query_pos;
    var max_href_len = 0;

    // The link with the longest match for path's ending wins, to account for the
    // cases like the following:
    //
    //     <link href="/css/main.css" />
    //     <link href="/bazinator/plugins/bar/css/main.css" />
    for (var i = 0; i < link_elts.length; i++) {
        cur_href = link_elts[i].getAttribute('href');
        query_pos = cur_href.indexOf('?');

        // Drop query string for file path matching.
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

    // Update the query string to force a reload.
    //
    // Make sure we don't break the existing query string while doing that.
    var target_href = target_link.getAttribute('href'),
        param_pos = target_href.indexOf(QUERY_PARAM_NAME),
        query_param = QUERY_PARAM_NAME + '=' + new Date().getTime(),
        new_href;

    query_pos = target_href.indexOf('?');

    if (query_pos > -1) {
        if (param_pos > -1) {
            // Replace our param in the query string.
            new_href = target_href.replace(new RegExp(QUERY_PARAM_NAME + '=\\d+'),
                                           query_param);
        } else {
            // Add our param to a pre-existing query string.
            new_href = target_href + '&' + query_param;
        }
    } else {
        // Add a query string with our param.
        new_href = target_href + '?' + query_param;
    }

    target_link.setAttribute('href', new_href);

    return true;
}
