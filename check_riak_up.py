#!/usr/bin/python

import sys, os
def _nagios(hdr, msg, code):
    print '%s: %s' % (hdr, msg)
    return code

def critical(msg): return _nagios('CRITICAL', msg, 2)
def warning(msg): return _nagios('WARNING', msg, 1)
def okay(msg): return _nagios('OKAY', msg, 0)

def main(args):
  status = os.popen('riak ping').read()
  if status == "pong\n":
    okay("It's up!")
  else:
    critical("It's down!")
  
if __name__ == '__main__':
  sys.exit(main(sys.argv[0:]))