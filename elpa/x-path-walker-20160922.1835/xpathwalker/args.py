from argparse import ArgumentParser
import os
import sys

exts_mode = {'.html': "HTML", '.html': 'HTML', '.xml': 'XML', '.json': 'JSON', '.yaml': 'YAML'}

def parse_args(argv):
    """parse command line arguments

    :param argv:
    :returns: Processor class
    :rtype:

    """
    default_separator = "."

    parser = ArgumentParser(description="Xpath helper")
    parser.add_argument("inputfile",
                        help="Input FILE", type=str)
    parser.add_argument("-x", "--xpath", dest='xpath',
                        help="Returns line number corresponding to xpath from FILE", type=str)
    parser.add_argument("-m", "--mode", dest='mode',
                        help="Language type", type=str, choices=('XML', 'HTML', 'JSON'))
    parser.add_argument("-a", dest='verbosity', action="store_true", default=False,
                        help="Increase verbosity")
    parser.add_argument("-M", dest='multiproc', action="store_true", default=False,
                        help="Use multiple process when possible")
    parser.add_argument("-j", dest='use_jq', action="store_true", default=False,
                        help="JSON: Use jq for processing")
    parser.add_argument("-F", dest='separator', default=default_separator, type=str,
                        help="Path elements separator")
    args = parser.parse_args(argv)

    inputfile = args.inputfile
    proc = False
    if(not os.path.isfile(inputfile)):
        sys.exit(1)

    ext = os.path.splitext(inputfile)[1].lower()

    if(args.mode is None):
        if ext not in exts_mode.keys():
            print("Unrecognized file format.")
            sys.exit(1)
        else:
            args.mode = exts_mode[ext]

    if(args.mode == 'HTML'):
        from xpathwalker.lxml_Processor import HTML_Processor
        proc = HTML_Processor()
        if args.xpath:
            args.xpath = "/"+args.xpath
    elif(args.mode == 'XML'):
        from xpathwalker.lxml_Processor import XML_Processor
        proc = XML_Processor()
        if args.xpath:
            args.xpath = "/"+args.xpath
    elif(args.mode == 'JSON'):
        if(args.use_jq):
            from xpathwalker.json_jq_Processor import JSON_jq_Processor
            proc = JSON_jq_Processor()
        else:
            from xpathwalker.json_Processor import JSON_Processor
            proc = JSON_Processor()
    else:
        print("Error in arg parsing")
        sys.exit(2)

    proc.separator = args.separator
    proc.parse_file(inputfile)

    if(args.xpath is not None):
        print(proc.get_path_line_number(args.xpath, args.verbosity))
    else:
        print("\n".join(proc.list_paths(args.verbosity)))
