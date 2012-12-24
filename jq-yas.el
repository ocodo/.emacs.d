# -*- mode: snippet -*-
# -*- coding: utf-8 -*-
# name: jquery selector - id
# key: q
# group: jquery
# contributor: Jason Milkins <jasonm23@gmail.com>
# --
\$("#${1:id}")${2:.}

# -*- mode: snippet -*-
# -*- coding: utf-8 -*-
# contributor: Jason Milkins <jasonm23@gmail.com>
# group: jquery
# name: jquery selector class
# key: q
# --
\$(".${1:class}")${2: .}

# -*- mode: snippet -*-
# -*- coding: utf-8 -*-
# contributor: Jason Milkins <jasonm23@gmail.com>
# group: jquery
# key: q
# name: jquery selector - tag
# --
\$("${1:tag}")${2:.}

# -*- mode: snippet -*-
# -*- coding: utf-8 -*-
# contributor: Jason Milkins <jasonm23@gmail.com>
# group: jquery
# key: q
# name: jquery selector - new tag object
# --
\$("<${1:tag}></$1>")${2:.}

# -*- mode: snippet -*-
# -*- coding: utf-8 -*-
# contributor: Jason Milkins <jasonm23@gmail.com>
# group: jquery
# key: .e
# name: each function
# --
each( function() {\n\t$0\n\t});

# -*- mode: snippet -*-
# -*- coding: utf-8 -*-
# contributor: Jason Milkins <jasonm23@gmail.com>
# group: jquery
# key: f
# name: find something
# --
find("${1:what}").$0

# -*- mode: snippet -*-
# -*- coding: utf-8 -*-
# contributor: Jason Milkins <jasonm23@gmail.com>
# group: jquery
# key: t
# name: this
# --
\$(this).$0

# -*- mode: snippet -*-
# -*- coding: utf-8 -*-
# contributor: Jason Milkins <jasonm23@gmail.com>
# group: jquery
# key: ag
# name: attr - getter
# --
attr('${1:attr}')${2:.}

# -*- mode: snippet -*-
# -*- coding: utf-8 -*-
# contributor: Jason Milkins <jasonm23@gmail.com>
# group: jquery
# key: as
# name: attrs - setter
# --
attr('${1:attr}', '${2:value}')${3:.}

# -*- mode: snippet -*-
# -*- coding: utf-8 -*-
# contributor: Jason Milkins <jasonm23@gmail.com>
# group: jquery
# key: cg
# name: css get
# --
css('${1:key}')${2:.}

# -*- mode: snippet -*-
# -*- coding: utf-8 -*-
# contributor: Jason Milkins <jasonm23@gmail.com>
# group: jquery
# key: cs
# name: css set
# --
css('${1:key}', '${2:value}')${3:.}


# -*- mode: snippet -*-
# -*- coding: utf-8 -*-
# contributor: Jason Milkins <jasonm23@gmail.com>
# group: jquery
# key: ch
# name: children
# --
children(${1:""}).


# -*- mode: snippet -*-
# -*- coding: utf-8 -*-
# contributor: Jason Milkins <jasonm23@gmail.com>
# group: jquery
# key: ap
# name: append
# --
append( $0 )


# -*- mode: snippet -*-
# -*- coding: utf-8 -*-
# contributor: Jason Milkins <jasonm23@gmail.com>
# group: jquery
# key: at
# name: appendto
# --
appendTo( $0 )


# -*- mode: snippet -*-
# -*- coding: utf-8 -*-
# contributor: Jason Milkins <jasonm23@gmail.com>
# group: jquery
# key: tx
# name: text
# --
text( $0 )${1:.}


# -*- mode: snippet -*-
# -*- coding: utf-8 -*-
# contributor: Jason Milkins <jasonm23@gmail.com>
# group: jquery
# key: h
# name: html here
# --
html( $0 )${1:.}


# -*- mode: snippet -*-
# -*- coding: utf-8 -*-
# contributor: Jason Milkins <jasonm23@gmail.com>
# group: jquery
# key: cl
# name: click handler
# --
click( function() { \n\t$0\n\t});


# -*- mode: snippet -*-
# -*- coding: utf-8 -*-
# contributor: Jason Milkins <jasonm23@gmail.com>
# group: jquery
# key: hv
# name: hover handler
# --
hover( function() { \n\t$1\n\t},\n\tfunction() {\n\t$0\n\t}


# -*- mode: snippet -*-
# -*- coding: utf-8 -*-
# contributor: Jason Milkins <jasonm23@gmail.com>
# group: jquery
# key: dr
# name: document.ready
# --
\$(document).ready( function() {\n\t$0\n\t});








# -*- mode: snippet -*-
# -*- coding: utf-8 -*-
# contributor: Jason Milkins <jasonm23@gmail.com>
# group: jquery
# key: b
# name: add a block
# --
{\n\t$0\n\t}


# -*- mode: snippet -*-
# -*- coding: utf-8 -*-
# contributor: Jason Milkins <jasonm23@gmail.com>
# group: jquery
# key: v
# name: var
# --
var ${1:name} = $0;


