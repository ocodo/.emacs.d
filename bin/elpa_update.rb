=begin

EmacsElpa Packages GitCommitter
===============================

This module is used to simplify updating Emacs ELPA packages.

In this Emacs config, unlike most, I maintain a repository history of
the ELPA packages I install. Usually (99% of the time over many years
so far) I have no issues with this at all.  Of course, hypothetically,
it's quite possible that any feature of any package could be broken at
any time.

In case a breakage occurs, it's possible to roll back to a previous
version of that package, and it's easy to do when we can identify the
package. We can simply cherry-pick it from a previous commit, or just
grab it from it's canonical source.

However, if the breakage manifests as a side-effect, identifying
the problem package is much harder.

To this end, I wrote this script to make each package have it's own
commit history, (starting today!)

In a nutshell, this script will accept the name of a package and
locate it from the folders in the current git staged/unstaged changes.

It will then create a single commit for the new version being added
and the old version(s) being removed (if it/they exists.)

Usage:
======

    git-elpa package-name ["optional commit message"]

If no commit message is provided the following automatic commit
message pattern will be used:

    "[package-name upgraded] to NEW_VERSION from OLD_VERSION"

=end

require 'git'

module EmacsElpa
  module Packages

    EMACS_D = File.join(Dir.home, ".emacs.d")

    class GitCommitter

      attr_accessor :g

      def initialize
        @g = Git.open EMACS_D
      end

      def updatable_packages
        elpa_rx = Regexp.new("^elpa/(.*?)/")
        package_name = Regexp.new("^(.*)-.*?$")
        updatable = (@g.status.untracked.map(&:first) + @g.status.deleted.map(&:first)).collect {|f| elpa_rx.match(f)[1] if elpa_rx.match(f) }.compact.uniq.sort
        updatable.map {|u| package_name.match(u)[1] if package_name.match(u) }.uniq
      end

      def commit_all_updatable_packages
        puts "Committing packages, current commit (for reset)"
        puts `git log --pretty=oneline -1`
        updatable_packages.each do |p|
          puts "Committing updated package: #{p}"
          update_package p
        end
      end

      def update_package(package, do_commit = true)
        @package = package
        @rx = Regexp.new("(^elpa/)(#{@package})-([^-]*?)/(.*)")
        commit if do_commit
      end

      def commit_message
        old = old_versions
        ver = new_version

        raise "No new version found for #{@package}" if ver == nil

        return "[Updating #{@package}] version: #{ver} [removing old versions: #{old.join(',')}]" if old.length > 1
        return "[Updating #{@package}] version: #{ver} [removing old version: #{old.first}]"      if old.length == 1
        return "[Adding #{@package}] version: #{ver}"                                             if old.length == 0
      end

      def add_to_index
        @g.add new_version_files
        @g.remove old_version_files
      end

      def commit(message = commit_message, add = true)
        add_to_index if add
        @g.commit message
      end

      def new_version
        versions = new_version_files.map{|f| @rx.match(f)[3] }.uniq
        if versions.length > 1
          raise "There are more than one new versions of #{@package}"
        end
        versions.first
      end

      def old_versions
        old_version_files.map{|f| @rx.match(f)[3] }.uniq
      end

      def new_version_files
        (@g.status.untracked.map(&:first) + @g.status.added.map(&:first)).select{|f| @rx.match(f) }
      end

      def old_version_files
        @g.status.deleted.map(&:first).select{|f| @rx.match(f) }
      end
    end
  end
end
