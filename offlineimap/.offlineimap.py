#! /usr/bin/env python
from subprocess import check_output

def get_pass(account):
    return check_output(
        "op read \"op://private/" + account + "/password\"",
        shell=True
    ).splitlines()[0]
