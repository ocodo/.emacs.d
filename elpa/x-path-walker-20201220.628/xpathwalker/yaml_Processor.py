##########################################################################
# YAML                                                                   #
##########################################################################
import yaml
from xpathwalker.utils import list_compare_idx
from xpathwalker.utils import traverse_and_remove_path, get_dict_from_path, get_class, paths
from collections import OrderedDict
import os

def parse_yaml(file):
    with open(file) as f:
        yaml_data = yaml.load(f)
        res = OrderedDict()
        for x in paths(yaml_data, leaves=False):
            res.update(x)
        return(res)

def get_yaml_paths(file, include_type=True):  # XXX: ordering is lost
    yaml_paths = parse_yaml(file)
    if(include_type is True):
        return(["%s | %s" % (get_class(yaml_paths[x]['class']), x) for x in yaml_paths.keys()])
    else:
        return(yaml_paths.keys())

def get_yaml_path_line_python(file, path, include_type=False):
    if(not os.path.isfile(file)):
        sys.exit(1)
    path = path.split("| ")[1] if (include_type is True) else path
    orig = ""
    with open(file) as f:
        orig = yaml.load(f)
    path = get_dict_from_path(path)
    res = traverse_and_remove_path(orig, path)
    yaml_dump_options = {"indent": 4, "default_flow_style": False, "explicit_start": False}
    orig = yaml.dump(orig, **yaml_dump_options).split("\n")
    with_del = yaml.dump(dict(res), **yaml_dump_options).split("\n")
    return(list_compare_idx(orig, with_del) + 1)
