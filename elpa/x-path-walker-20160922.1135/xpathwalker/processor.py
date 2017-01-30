

class Processor:
    """An abstract object class to handle the parsing/procesing of files

    """
    separator = "."

    def __init__(self):
        super(Processor, self).__init__()

    def list_paths(self, parsed, with_attrs=False):
        """Return array of path separated by the separator

        :param parsed: A python type object
        :param with_attrs: List additional attributes
        :returns: array of paths
        :rtype: array

        """
        pass

    def get_path_line_number(self, path, with_attrs=False):
        """Returns the line number from the file corresponding `path`

        :param path: Path seeked
        :param with_attrs: `True` if `path` is formatted with additional attributes
        :returns: line numer corresponding to path
        :rtype: int

        """
        pass

    def parse_file(self, file):
        """Translate the `file` into python object

        :param file: File path
        :returns: Nothing. insternal variable updated.
        :rtype:

        """
        pass
