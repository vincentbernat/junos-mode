#!/usr/bin/env python3

"""Commit configuration snippets to Juniper devices.

This program multiplexes sessions to Juniper devices to commit/commit
confirm/rollback configuration on Juniper devices. One instance of
this program is able to manage several Juniper devices
simultaneously.

The protocol used is pretty simple and similar to IMAP (RFC3501). A
connection consists of an initial greeting from the server followed by
client/server interactions. These client/server interactions consist
of a client command, server data and a server completion result
response.

All interactions transmitted by the client and the server are in the
form of lines. The client command begins an operation. Each client
command is prefixed with an identifier (typically a short alphanumeric
string, e.g., A0001, A0002, etc.) called a tag. A different tag is
generated by the client for each command (this is not checked but is
important for normal operation). The server will answer requests with
the same tag::

    S: * junos.py service ready
    C: a001+load switch1.example.com
    C: a001>system {
    C: a001>  host-name switch1.example.com;
    C: a001>  time-zone Europe/Zurich;
    C: a001>}
    C: a001.
    S: a001 ok
    C: a002 diff switch1.example.com
    S: a002+ok
    S: a002>[edit system]
    S: a002>+host-name switch1.example.com;
    S: a002>+time-zone Europe/Zurich
    S: a002.
    C: a003 rollback switch1.example.com
    S: a003 ok

Some commands and answers come with a body. In this case, the tag is
followed by ``+``. Each line of the body will be prefixed by the tag
and a ``>``. The body will end with the tag and a ``.``.

There are several commands a client can issue. Each command is
directly followed by the equipment it should apply to.

 - ``load`` to load a configuration. The configuration should be
   provided as a body. The server will answer with ``ok`` or
   ``error``. In the later case, a body containing an error will be
   transmitted.

 - ``diff`` will return a configuration diff.

 - ``check`` will check the configuration for any errors. A body with
   errors is returned if any.

 - ``rollback`` will restore the configuration. Optionally, a rollback
   ID can be provided (after the name of the equipment).

 - ``commit`` will commit the configuration. Optionally, a duration in
   minutes can be provided (after the name of the equipment). In this
   case, a commit-confirm command is issued.

 - ``run`` will run a command (in a shell). It will return ``ok`` and
   the output of the command.

A host should use one of the following form:

 - ``host.example.com`` (default user from ``~/.ssh/config`` and
   authentication with SSH key)

 - ``user@host.example.com`` (authentication with SSH key)

 - ``user:password@host.example.com`` (authentication with password)

"""

from __future__ import print_function

import sys
import re
import threading
import collections
import inspect
import functools
import contextlib
import traceback
import pprint

from jnpr.junos import Device
from jnpr.junos.utils.config import Config
from jnpr.junos.exception import CommitError, ConfigLoadError

output_lock = threading.Lock()
device_lock = threading.Lock()
device_list = {}
connect_re = re.compile(r'(?:(?P<user>[^@:]*)(?::(?P<password>.*))?@)?'
                        r'(?P<host>.*)')
input_re = re.compile(r'(?P<tag>[a-zA-Z0-9_-]+)'
                      r'(?P<sep>[ >+.])'
                      r'(?P<line>.*)')


def output(tag, lines):
    """Output several lines to client."""
    if isinstance(lines, str):
        lines = [lines]
    lines = functools.reduce(lambda x, y: x + y.splitlines(), lines, [])
    with output_lock:
        for idx, line in enumerate(lines):
            print("{tag}{sep}{line}".format(
                tag=tag,
                line=line,
                sep=(len(lines) == 1 and " " or
                     idx == 0 and "+" or
                     ">")))
        if len(lines) > 1:
            print("{tag}.".format(tag=tag))


def input():
    """Get a line from user.

    Return a tag, a separator character and the actual line. On error,
    trigger an exception. On EOF, trigger EOFError.

    """
    line = sys.stdin.readline()
    if line == "":
        raise EOFError("end of input")
    line = line.rstrip()
    if line == "":
        return None, None, None
    mo = input_re.match(line)
    if not mo:
        raise ValueError("unparsable input")
    return mo.group("tag"), mo.group("sep"), mo.group("line")


def background(fn):
    """Run a function in a dedicated thread."""
    def _fn(*args, **kwargs):
        try:
            fn(*args, **kwargs)
        except:                 # noqa: E722
            exc_type, exc_value, exc_tb = sys.exc_info()[:3]
            output(args[0], ["error"] +
                   traceback.format_exception(exc_type, exc_value, exc_tb))

    @functools.wraps(fn)
    def run(*args, **kwargs):
        t = threading.Thread(target=_fn, args=args, kwargs=kwargs)
        t.setDaemon(True)
        t.start()

    return run


@contextlib.contextmanager
def device(connect_string):
    """Return a connected device.

    This should be run as a context. Devices will be kept open and
    reused. If a device is already in use, entering the context will
    block until the device is ready.

    """
    with device_lock:
        device, lock = device_list.get(connect_string, (None, None))
        if device is None:
            # Parse the connect string
            mo = connect_re.match(connect_string)
            if not mo:
                raise ValueError("unparsable host string")
            args = {k: v for k, v in mo.groupdict().items()
                    if v is not None}
            device = Device(**args)
            lock = threading.Lock()
            device_list[connect_string] = device, lock
    with lock:
        if not device.connected:
            device.open(gather_facts=False, attempts=3)
            device.timeout = 60
        yield device


def do(tag, lines):

    """Do new work for a given tag.

    We could check if another tag is already working, but we won't do
    that.

    """
    command = re.split(r"\s+", lines[0])
    lines = lines[1:]
    fn = "do_{}".format(command[0])
    args = command[1:]
    match = [f[1]
             for f in inspect.getmembers(sys.modules[__name__],
                                         inspect.isfunction)
             if f[0] == fn]
    if not match:
        raise ValueError("unknown command")
    match[0](tag, args, lines)


def do_ping(tag, args, lines):
    """Answer with pong."""
    if len(args) != 0:
        raise TypeError("ping doesn't accept any argument")
    output(tag, "pong")


@background
def do_load(tag, args, lines):
    """Load a new configuration."""
    if len(args) != 1:
        raise TypeError("load expects a unique argument")
    with device(args[0]) as dev:
        with Config(dev) as cu:
            try:
                cu.load("\n".join(lines))
            except ConfigLoadError as ce:
                errs = [pprint.pformat({k: v for k, v in err.items()
                                        if v is not None})
                        for err in ce.errs]
                output(tag, ["error"] + errs)
            else:
                output(tag, "ok")


@background
def do_diff(tag, args, lines):
    """Diff the candidate config and the running config."""
    if len(args) != 1:
        raise TypeError("diff expects a unique argument")
    with device(args[0]) as dev:
        with Config(dev) as cu:
            diff = cu.diff()
            output(tag, ["ok", (diff and diff.strip()) or ""])


@background
def do_check(tag, args, lines):
    """Check the candidate configuration."""
    if len(args) != 1:
        raise TypeError("check expects a unique argument")
    with device(args[0]) as dev:
        with Config(dev) as cu:
            try:
                cu.commit_check()
            except CommitError as ce:
                errs = [pprint.pformat({k: v for k, v in err.items()
                                        if v is not None})
                        for err in ce.errs]
                output(tag, ["error"] + errs)
            else:
                output(tag, "ok")


@background
def do_rollback(tag, args, lines):
    """Rollback the candidate config to the running config."""
    if len(args) not in (1, 2):
        raise TypeError("rollback expects a unique argument")
    rid = 0
    if len(args) == 2:
        rid = int(args[1])
    with device(args[0]) as dev:
        with Config(dev) as cu:
            cu.rollback(rid)
            output(tag, "ok")


@background
def do_commit(tag, args, lines):
    """Commit the candidate config."""
    if len(args) not in (1, 2):
        raise TypeError("commit expects one or two arguments")
    with device(args[0]) as dev:
        with Config(dev) as cu:
            try:
                if len(args) == 2:
                    cu.commit(confirm=int(args[1]))
                else:
                    cu.commit()
            except CommitError as ce:
                errs = [pprint.pformat({k: v for k, v in err.items()
                                        if v is not None})
                        for err in ce.errs]
                output(tag, ["error"] + errs)
            output(tag, "ok")


@background
def do_run(tag, args, lines):
    """Run a shell command."""
    if len(args) != 1:
        raise TypeError("run expects a unique argument")
    if len(lines) != 1:
        raise TypeError("run expects a unique line of command")
    with device(args[0]) as dev:
        result = dev.cli(lines[0], warning=False)
        output(tag, ["ok", result.strip()])


def main():
    output("*", "junos.py service ready")
    inputs = collections.defaultdict(list)
    while True:
        try:
            # Parse a line of input
            tag = None
            try:
                tag, sep, line = input()
            except EOFError:
                break

            if tag is None:
                continue
            if sep == " " and tag in inputs:
                del inputs[tag]
                raise ValueError("non-continuation separator while previous "
                                 "input present")
            if sep == "+" and tag in inputs:
                del inputs[tag]
                raise ValueError("start separator while previous input "
                                 "present")
            if sep == ">" and tag not in inputs:
                raise ValueError("continuation separator while previous "
                                 "input not present")
            if sep == "." and tag not in inputs:
                raise ValueError("termination separator while previous "
                                 "input not present")
            if sep == "." and line != "":
                raise ValueError("termination separator with content")

            if sep == " ":
                do(tag, [line])
            elif sep == "+" or sep == ">":
                inputs[tag].append(line)
            elif sep == ".":
                do(tag, inputs[tag])
                del inputs[tag]
        except:                 # noqa: E722
            exc_type, exc_value, exc_tb = sys.exc_info()[:3]
            output(tag or "*",
                   ["error"] +
                   traceback.format_exception(exc_type, exc_value, exc_tb))
            del exc_tb
    output("*", "bye")


if __name__ == "__main__":
    main()
