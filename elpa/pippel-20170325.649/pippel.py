import sys
import os
import json
from pip.commands.list import ListCommand
from pip.commands.install import InstallCommand
from pip.commands.uninstall import UninstallCommand
from pip.utils import get_installed_distributions
from pip.commands.show import search_packages_info 


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
        if line == '':
            raise EOFError()
        return json.loads(line)

    def handle_request(self):
        request = self.read_json()
        method = request['method']
        params = request['params']
        packages = request['packages']
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
        except:
            self.stdout.write("Pip error\n")

    def serve_forever(self):
        while True:
            try:
                self.handle_request()
            except (KeyboardInterrupt, EOFError, SystemExit):
                break


class PipBackend(Server):
    def get_installed_packages(self):
        final = []
        get_list = ListCommand()
        options,args = get_list.parse_args(["--outdated"])
        for package in get_installed_distributions():
            name = str(package).split(" ")[0]
            if name == "team": continue
            for pkg in get_list.iter_packages_latest_infos([package], options):
                latest = str(pkg.latest_version)
            for attributes in search_packages_info([name]):   
                result = {"name": attributes["name"],
                          "version": attributes["version"],
                          "latest": latest,
                          "summary": attributes["summary"],
                          "home-page": attributes["home-page"]}
            final.append(result)
        return final

    def install_package(self, packages, params=None):
        install = InstallCommand()
        pkg_list = packages.split(" ")
        for pkg in pkg_list:
            # virtualenv active ?
            if hasattr(sys, 'real_prefix'):
                options, args = install.parse_args([pkg, '--upgrade'])
            elif params:
                options, args = install.parse_args([pkg, '--upgrade', '--target', params])
            else:
                options, args = install.parse_args([pkg, '--upgrade', '--user'])
            install.run(options, args)

    def remove_package(self, packages):
        uninstall = UninstallCommand()
        pkg_list = packages.split(" ")
        for pkg in pkg_list:
            options, args = uninstall.parse_args([pkg, '--y'])
            uninstall.run(options, args)
    
if __name__ == '__main__':
    stdin = sys.stdin
    stdout = sys.stdout
    sys.stdout = sys.stderr = open(os.devnull, "w")
    stdout.flush()
    PipBackend(stdin, stdout).serve_forever()
