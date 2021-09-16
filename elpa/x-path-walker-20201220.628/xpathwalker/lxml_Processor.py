from xpathwalker.processor import Processor
import os
import sys
import lxml.etree
import lxml.html


class lxmlProcessor(Processor):
    """ [X/HT]ML processor
    """
    def list_paths(self, with_attrs=False):
        if(not self.parsed):
            raise ValueError("No data in parsed")
        parsed = self.parsed
        res = []
        for el in parsed.xpath("//*"):
            if(with_attrs):
                if el.keys() is None:
                    res.append("/%s | %s | %s" % (parsed.getelementpath(el), ",".join(el.keys())))
                else:
                    res.append("/%s | %s" % (parsed.getelementpath(el),
                                             ",".join(["%s: %s" % (attr, el.get(key=attr)) for attr in el.keys()])))
            else:
                res.append("/%s" % (parsed.getelementpath(el)))
        return(res)

    def handle_namespace_in_xpath(self, xpath):
        closinp = len(xpath.split("}"))
        if(closinp > 1 and closinp != len(xpath.split("}"))):
            return xpath
        import re
        import string
        insidep = r"(?<={)[^}]*"
        namesspaces = re.findall(insidep, xpath)
        nsdict = {}

        def get_letters(matchobj):
            get_letters.counter += 1
            id = string.ascii_letters[get_letters.counter]
            nsdict.update({id: namesspaces[get_letters.counter]})
            return id + ":"
        get_letters.counter = -1
        par = re.compile(r"{"+insidep+r"}")
        xpath = par.sub(get_letters, xpath)
        return (xpath, {"namespaces": nsdict})

    def get_path_line_number(self, xpath, with_attrs=False):
        if(not self.parsed):
            raise ValueError("No data in parsed")

        parsed = self.parsed
        xpath = xpath.split(" | ")[0] if (with_attrs is True or xpath.find(" | ") > -1) else xpath
        res = 0
        matchs = []

        if(xpath.find("{") > -1):  # Handle namespace cases
            xpath, args = self.handle_namespace_in_xpath(xpath)
            matchs = parsed.xpath(xpath, **args)
        else:
            matchs = parsed.xpath(xpath)
        if(len(matchs) > 0):
            res = matchs[0].sourceline
        return(res)

class HTML_Processor(lxmlProcessor):
    def parse_file(self, file):
        """HTML file parse from lxml

        :param file: file path (string)
        :returns: null
        :rtype:

        """
        self.parsed = lxml.html.parse(file)

class XML_Processor(lxmlProcessor):
    def parse_file(self, file):
        """HTML file parse from lxml

        :param file: file path (string)
        :returns: null
        :rtype:

        """
        self.parsed = lxml.etree.parse(file)
