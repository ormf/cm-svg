#! /usr/bin/env python
# -*- coding: utf-8 -*-

"""
Inkscape extension to play selected objects using lisp/incudine
"""

import os
import re
import inkex
import socket
import tempfile
import subprocess
import lxml.etree as et
import warnings
import time
warnings.filterwarnings("ignore")
from pythonosc.udp_client import SimpleUDPClient

from inkex import (Group, Anchor, Switch, NamedView, Defs, Metadata, ForeignObject, ClipPath, Use, SvgDocumentElement,)

ip = "127.0.0.1"
port = 1337
client = SimpleUDPClient(ip, port)  # Create client

class PlaySelection(inkex.Effect):

  def __init__(self):
    inkex.Effect.__init__(self)

  def run_command(self, args):
    try:
      with subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE) as proc:
        proc.wait(timeout=300)
    except OSError:
      inkex.errormsg('Program "%s" is not installed!' % args[0])
      exit()

  def export_elem(self, elem, file1):
    if isinstance(elem, (SvgDocumentElement, Anchor, Group)):
      for child in elem.iterchildren():
        self.export_elem(child, file1)
    else:
      self.export_path(elem, file1)

  def export_path(self, elem, file1):
    pp=elem.path.to_absolute() #remove transformation matrix
    elem.path = re.sub(r"Z","",str(pp))
    style = elem.get('style')
    color = re.sub(r".*stroke:([#0-9a-fA-F]+).*$","\"\\1\"", style)
    if re.search("^opacity:", style):
      opacity = re.sub(r"opacity:([0-9\.]+).*$","\\1", style)
    else:
      opacity = re.sub(r".*[^-]opacity:([0-9\.]+).*$","\\1", style)
    if elem.get('attributes'):
      attributes = re.sub(r":lsample ([^ ]+) ",":lsample \"\\1\" ", elem.get('attributes'))
      attributes = re.sub(r":type ([^ ]+) ",":type cm:\\1 ", attributes)
    else:
      attributes = False
    path = elem.get('d')
    if re.search("M ([0-9\.]+) ([0-9\.]+) L ([0-9\.]+) ([0-9\.]+)", path):
      coords = re.sub(r"M ([0-9\.]+) ([0-9\.]+) L ([0-9\.]+) ([0-9\.]+)",":x1 \\1 :y1 \\2 :x2 \\3 :y2 \\4", path)
    else:
      coords = re.sub(r"M ([0-9\.]+) ([0-9\.]+) H ([0-9\.]+)",":x1 \\1 :y1 \\2 :x2 \\3 :y2 \\2", path)    
    file1.write('(')
    file1.write(coords)
    file1.write("\n    :color ")
    file1.write(color)
    file1.write("\n    :opacity ")
    file1.write(opacity)
    # file1.write(\n    :style \"")
    # file1.write(style)
    if attributes:
      file1.write('\n    :attributes (')
      file1.write(attributes)
    file1.write('))\n   ')

  def write_tempfile(self):
    # svg.selection.filter(\*types).values()
    # desc, tmpfile = tempfile.mkstemp(suffix='.lisp', prefix='selected-export-')
    # fd = os.open('/tmp/incudine-export.lisp', os.O_RDWR|os.O_CREAT)
    with open('/tmp/incudine-export.lisp', 'w') as file1:
      file1.write("(in-package :cm)\n(sprout\n (inkscape-export->cm\n   '(")
      for elem in self.svg.selected:
        self.export_elem(elem, file1)
      file1.write(")))\n")
    return '/tmp/export-test.lisp'

  def effect(self):

    if len(self.options.ids) < 1:
      inkex.errormsg('Please select at least 1 object.')
      exit()

    self.write_tempfile()
    client.send_message("/inkscape/play", [])   # Send message

if __name__ == '__main__':
  exporter = PlaySelection()
  exporter.run()
