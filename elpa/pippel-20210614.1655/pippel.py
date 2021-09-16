import json
import os
import sys
from typing import Dict, List, Optional

from pip import __version__ as pipver

# For older versions of pip
try:
    from pip.commands.install import InstallCommand
    from pip.commands.list import ListCommand
    from pip.commands.show import search_packages_info
    from pip.commands.uninstall import UninstallCommand
    from pip.utils import get_installed_distributions

# For newer versions of pip
except ImportError:
    from pip._internal.commands.install import InstallCommand
    from pip._internal.commands.list import ListCommand
    from pip._internal.commands.show import search_packages_info
    from pip._internal.commands.uninstall import UninstallCommand
    from pip._internal.utils.misc import get_installed_distributions


class Server(object):
    def __init__(self, stdin=None, stdout=None):
        if stdin is None:
            self.stdin = sys.stdin
        else:
            self.stdin = stdin
        if stdout is None:
            self.stdout = sys.stdout
        else:
            self.stdout = stdout

    def write_json(self, result):
        self.stdout.write(json.dumps(result) + "\n")
        self.stdout.flush()

    def read_json(self):
        line = self.stdin.readline()
        if not line:
            raise EOFError()
        return json.loads(line)

    def handle_request(self):
        request = self.read_json()
        method = request["method"]
        params = request["params"]
        packages = request["packages"]
        try:
            method = getattr(self, method, None)
            if not packages:
                result = method()
                self.write_json(result)
            elif not params:
                method(packages)
            else:
                method(packages, params)
            self.stdout.write("Pip finished\n")
        except Exception as e:
            self.stdout.write("%s\n" % e)
            self.stdout.write("Pip error\n")

    def serve_forever(self):
        while True:
            try:
                self.handle_request()
            except (KeyboardInterrupt, EOFError, SystemExit):
                break


class PipBackend(Server):
    @staticmethod
    def in_virtual_env():
        # type: () -> bool
        """
        Returns TRUE if package is run while in a virtual environment
        """
        if hasattr(sys, "base_prefix"):
            return sys.base_prefix != sys.prefix
        elif hasattr(sys, "real_prefix"):
            return sys.real_prefix != sys.prefix
        else:
            return False

    def get_installed_packages(self):
        # type: () -> List[Dict[str, str]]
        try:
            get_list = ListCommand()
        except TypeError:
            get_list = ListCommand("Pippel",
                                   "Backend server for the Pippel service.")
        options, args = get_list.parse_args(["--outdated"])
        packages = [package for package in get_installed_distributions()
                    if package.key != "team"]
        final = [
            {"name": attributes.get("name"),
             "version": attributes.get("version"),
             "latest": str(getattr(package, "latest_version")),
             "summary": attributes.get("summary"),
             "home-page": attributes.get("home-page")
            }
            for package in get_list.iter_packages_latest_infos(packages, options)
            for attributes in search_packages_info([package.key])  # noqa
        ]
        # TODO: To profile performance speed of snippet above and below.
        # final = [
        #     {"name": attributes.get("name"),
        #      "version": attributes.get("version"),
        #      "latest": str(getattr(latest_info, "latest_version")),
        #      "summary": attributes.get("summary"),
        #      "home-page": attributes.get("home-page")
        #     }
        #     for latest_info, attributes in zip(
        #             [get_list.iter_packages_latest_infos(packages, options),
        #              search_packages_info([getattr(package, "key")
        #                                    for package in packages])]
        #     )
        # ]
        return final

    def install_package(self, packages, params=None):
        # type: (str, Optional[str]) -> int
        try:
            # For pip <= 19.1.1
            install = InstallCommand()
        except TypeError:
            # For pip > 19.1.1
            install = InstallCommand("Pippel",
                                     "Backend server for the Pippel service.")
        assert packages , "`packages` should not be an empty string."
        for package in packages.split():
            if self.in_virtual_env():
                args = [package, "--upgrade"]
            elif params:
                args = [package, "--upgrade", "--target", params]
            else:
                args = [package, "--upgrade", "--user"]
            k = install.main(args)
        return k

    def remove_package(self, packages):
        # type: (str) -> int
        packages = packages.split()
        assert packages , "`packages` should not be an empty string."
        try:
            # For pip <= 19.1.1
            uninstall = UninstallCommand()
        except TypeError:
            # For pip > 19.1.1
            uninstall = UninstallCommand("Pippel",
                                         "Backend server for the Pippel service.")
        k = uninstall.main(packages + ["--yes"])
        return k

if __name__ == "__main__":
    stdin = sys.stdin
    stdout = sys.stdout
    sys.stdout = sys.stderr = open(os.devnull, "w")
    stdout.flush()
    PipBackend(stdin, stdout).serve_forever()
