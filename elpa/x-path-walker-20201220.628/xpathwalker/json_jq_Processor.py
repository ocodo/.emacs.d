import subprocess
import sys
import os
from tempfile import NamedTemporaryFile
from xpathwalker.utils import list_compare_idx
from xpathwalker.processor import Processor

##########################################################################
# JSON                                                                   #
##########################################################################

jq_command = """def safeLookupValue:
        if type != "string" then
                tostring
        else
                .
        end;
\
def arrayToLookup:
        map(safeLookupValue)
        | reduce .[] as $exists (
                {};
                . + {
                        ($exists): null
                }
        );
\
def lookup(value):
        (value | safeLookupValue) as $value
        | has($value);
\
def isWhitelisted(whitelist):
        whitelist as $whitelist
        | explode
        | map(
                . as $charCode
                | $whitelist
                | lookup($charCode)
        )
        | all;
\
def isNumeric:
        ("0123456789" | explode | arrayToLookup) as $digits
        | isWhitelisted($digits);
\
def isAlpha:
        ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" | explode | arrayToLookup) as $letters
        | isWhitelisted($letters);
\
def isAlphanumeric:
        ("0123456789" | explode | arrayToLookup) as $digits
        | ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" | explode | arrayToLookup) as $letters
        | ("_" | explode | arrayToLookup) as $special
        | ($digits + $letters + $special) as $alphanumeric
        | isWhitelisted($alphanumeric);
\
def isValidJsonShorthandPropertyName:
        (.[0:1] | isAlpha | not) or (isAlphanumeric | not);
\
[
        path(..)
        | map(
                if type == "number" then
                        ["[",(.|tostring),"]"]|join("")
                else
                        tostring
                        | if isValidJsonShorthandPropertyName then
                                "[\\"\\(.)\\"]"
                        else
                                .
                        end
                end
        )
        | join(".")
        | split("." + "[")
        | join("[")
]
| unique
| map("." + .)
| .[]"""

jq_exec = ["jq", "--raw-output"]

def jq_proc(args):
    with NamedTemporaryFile() as f:
        try:
            proc = subprocess.check_call(args,
                                         stdout=f,
                                         stderr=subprocess.STDOUT)
        except subprocess.CalledProcessError:
            print(proc.stderr.fseek(0).read().decode('utf-8'))
            sys.exit(1)
        f.seek(0)
        return f.read().decode('utf-8').split('\n')


class JSON_jq_Processor(Processor):
    file = ""

    def list_paths(self, with_attrs=True, multiproc=False):
        if(not os.path.isfile(self.file)):
            sys.exit(1)
        proc = jq_proc(args=jq_exec + [jq_command,
                                       self.file])
        paths = [x for x in proc if x != '']
        res = []
        if(with_attrs is False):
            res = paths
        elif with_attrs is True:
            if multiproc is False:
                for x in paths:
                    proc = jq_proc(args=jq_exec + ["%s|type" % x,
                                                   self.file])
                    type = proc[0]
                    res.append("%6s | %s" % (type[0:6], x))
            else:
                def get_types(q, x, file):
                    proc = jq_proc(args=jq_exec + ["%s|type" % x,
                                                   self.file])
                    type = proc[0]
                    q.put("%6s | %s" % (type[0:6], x))
                import queue
                import threading
                q = queue.PriorityQueue()
                for x in paths:
                    t = threading.Thread(target=get_types, args=(q, x, self.file))
                    t. start()
                while(q.qsize() > 0):
                    res.append(q.get())
        return(res)

    def get_path_line_number(self, path, with_attrs=False):
        path = path.split("| ")[1] if (with_attrs is True) else path
        proc = jq_proc(args=jq_exec + ['.',
                                       self.file])
        orig = proc
        proc = jq_proc(args=jq_exec + ['del(%s)' % path,
                                       self.file])
        with_del = proc
        return(list_compare_idx(orig, with_del) + 1)

    def parse_file(self, file):
        if(not os.path.isfile(file)):
            print("File %s: Not Found" % (file))
            sys.exit(1)
        self.file = file
