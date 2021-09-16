from collections import OrderedDict

##########################################################################
# Utils                                                                  #
##########################################################################

def get_class(obj):
    name = obj.__name__
    if(name == "OrderedDict"):
        return("object")
    elif(name == "NoneType"):
        return("null")
    return(name)

def list_compare_idx(list1, list2):
    index = 0
    for idx, origline in enumerate(list1):
        if(list2[idx] != origline):
            if((list2[idx]+",") != origline):
                index = idx
                break
    return(index)

def split_path(path, separator, inside_special=False):
    first_match = '"]' if inside_special else '["'
    i = path.find(first_match)
    if i != -1 and inside_special is False:
        if(path[0:i] != ''):
            return (split_path(path[0:i],
                               separator=separator,
                               inside_special=False) +
                    split_path(path[i+len('["'):],
                               separator=separator,
                               inside_special=True))
        else:
            return (split_path(path[i+len('["'):],
                               separator=separator,
                               inside_special=True))
    if i != -1 and inside_special is True:
        if(path[i+len('["'):] != ''):
            return ([path[:i], "special"],) + split_path(path[i+len('["'):],
                                                         separator=separator,
                                                         inside_special=False)
        else:
            return ([path[:i], "special"],)
    else:
        i = path.find(separator)
        if i == -1:
            return (path,)
        else:
            if(i != 0):
                return (path[:i],) + split_path(path[i+1:], separator)
            else:
                return split_path(path[i+1:], separator)


def get_dict_from_path(path, separator):
    path = split_path(path, separator)

    res = []
    for x in path:
        if isinstance(x, str) and x.find("[") == -1:
            res.append(x)
        elif not isinstance(x, list):
            pos = x.find("[")
            while pos != -1:
                if pos == 0:
                    i1 = x.find("]")
                    x1 = int(x[0:i1].replace("[", "", 1).replace("]", "", 1))
                    x = x[i1+1:]
                    res.append(x1)
                else:
                    res.append(x[0:pos])
                    x = x[pos:]
                pos = x.find("[")
        else:
            res.append(x[0])
    return(res)

def traverse_and_remove_path(obj, path=None, match="First"):
    if path is None:
        path = []
    if isinstance(obj, dict):
        res = OrderedDict()
        for k, v in obj.items():
            cmatch = False
            if(match and len(path) > 0 and path[0] == k):
                cmatch = True
            res.update({k: traverse_and_remove_path(v, path=path[1:], match=cmatch)})
        if(len(path) == 1 and path[0] in res.keys() and match):
            del res[path[0]]
        return res
    elif isinstance(obj, list):
        res = []
        for i, elem in enumerate(obj):
            cmatch = False
            if(match and len(path) >= 1 and isinstance(path[0], int) and path[0] < len(obj) and i == path[0]):
                cmatch = True
            res.append(traverse_and_remove_path(elem, path=path[1:], match=cmatch))
        if(len(path) == 1 and isinstance(path[0], int) and path[0] < len(res) and match):
            res.pop(path[0])
        return res
    else:
        return obj  # no container, just values (str, int, float)
