import json
from collections.abc import MutableSequence, MutableMapping
from collections import OrderedDict
from xpathwalker.utils import list_compare_idx
from xpathwalker.processor import Processor
from xpathwalker.utils import traverse_and_remove_path,\
    get_dict_from_path, get_class

##########################################################################
# JSON / Python                                                          #
##########################################################################


class JSON_Processor(Processor):
    file_line_number = 0

    def isValidJsonShorthandPropertyName(self, key):
        if not key[0].isalpha() or not key.isalnum():
            return('["' + key + '"]')
        else:
            return(self.separator + key)


    def paths(self, obj, dirs=True, leaves=True, path='', skip=False, allow_empty_string_keys=False):
        """Yield all paths of the object.
        Arguments:
        obj -- An object to get paths from.
        Keyword Arguments:
        dirs -- Yield intermediate paths.
        leaves -- Yield the paths with leaf objects.
        path -- A list of keys representing the path.
        skip -- Skip special keys beginning with '+'.
        """
        if isinstance(obj, MutableMapping):
            # Python 3 support
            # if PY3:
            iteritems = obj.items()

            for (k, v) in iteritems:
                if issubclass(k.__class__, (str)):
                    if (not k) and (not allow_empty_string_keys):
                        print("Empty string keys not allowed without "
                              "dpath.options.ALLOW_EMPTY_STRING_KEYS=True")
                    elif (skip and k[0] == '+'):
                        continue
                newpath = path + self.isValidJsonShorthandPropertyName(str(k))
                # validate(newpath)
                if dirs:
                    yield {newpath: {"class": v.__class__}}
                for child in self.paths(v, dirs, leaves, newpath, skip):
                    yield child

        elif isinstance(obj, MutableSequence):
            for (i, v) in enumerate(obj):
                newpath = path + "[%s]" % i
                if issubclass(i.__class__, int):
                    newpath = path + "[%d]" % i
                if dirs:
                    yield {newpath: {"class": v.__class__}}
                for child in self.paths(obj[i], dirs, leaves, newpath, skip):
                    yield child
        elif leaves:
            yield path + [[obj, obj.__class__]]
        elif not dirs:
            yield path

    def serialize_json(self):
        res = OrderedDict()
        for x in self.paths(self.parsed, leaves=False):
            res.update(x)
        return(res)

    def list_paths(self, with_attrs=True):
        json_paths = self.serialize_json()
        if(with_attrs):
            return(["%6s | %s" % (get_class(json_paths[x]["class"]), x) for x in json_paths.keys()])
        else:
            return([x for x in json_paths.keys()])

    def get_path_line_number(self, path, with_attrs=False):
        path = path.split("| ")[1] if (with_attrs is True) else path

        orig = self.parsed
        path = get_dict_from_path(path, self.separator)
        res = traverse_and_remove_path(orig, path)
        orig = json.dumps(orig, indent=4).split("\n")

        if(len(orig) != self.file_line_number):
            raise ValueError("This json file formatting is not compatible. Please run `python -m json.tool file`")

        with_del = json.dumps(res, indent=4).split("\n")
        return(list_compare_idx(orig, with_del) + 1)

    def parse_file(self, file):
        with open(file) as f:
            self.file_line_number = len(f.readlines())
            f.seek(0)
            self.parsed = json.load(f, object_pairs_hook=OrderedDict)
