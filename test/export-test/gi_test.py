#!/usr/bin/env python3
"""Use the D library through its generated GIR (typelib) from Python (PyGObject): constructors, methods, functions,
properties, signals, virtual methods overridden in Python, interfaces, GError and finalization of the D objects.
Run with GI_TYPELIB_PATH and LD_LIBRARY_PATH set (see the run-test-gir target of the Makefile)."""
import ctypes
import gc
import sys
import xml.etree.ElementTree as ET

import gi
gi.require_version('GidgenExport', '1.0')
gi.require_version('GidgenFeat', '1.0')
from gi.repository import GLib, GObject, GidgenExport, GidgenFeat

lib = ctypes.CDLL('libgidgen-export.so')

# Every C function and get_type function in the GIR is exported by the library
ns = {'c': 'http://www.gtk.org/introspection/c/1.0', 'glib': 'http://www.gtk.org/introspection/glib/1.0'}
symbols = []
for e in ET.parse('GidgenExport-1.0.gir').iter():
    for attr in ('{%s}identifier' % ns['c'], '{%s}get-type' % ns['glib']):
        if attr in e.attrib:
            symbols.append(e.attrib[attr])
missing = [s for s in symbols if not hasattr(lib, s)]
assert len(symbols) > 20 and not missing, missing

lib.gidgen_export_live_dobjects.restype = ctypes.c_int
start = lib.gidgen_export_live_dobjects()

# Class derived from a C class of another namespace: properties, signals, D overrides of C vfuncs
obj = GidgenExport.Obj()
assert isinstance(obj, GidgenFeat.Object)
assert obj.props.count == 1 and obj.props.title == 'hi' and obj.props.construct_val == 10
notified = []
obj.connect('notify::count', lambda o, p: notified.append(p.name))
obj.props.count = 5
obj.props.count = 5  # Same value: no notify
assert obj.props.count == 5 and notified == ['count']
bumps = []
obj.connect('bumped', lambda o, amount, who: bumps.append((amount, who)))
obj.emit('bumped', 2, 'py')
assert bumps == [(2, 'py')] and obj.props.bump_total == 2  # The D class handler ran too
assert obj.emit('greet', 'bob') == 'hello bob'
obj.emit('reset')  # Action signal
assert obj.props.bump_total == 0
assert obj.compute(5) == 5 * 3 + 77  # D override of a C vfunc

child = GidgenExport.Child.new()
assert isinstance(child, GidgenExport.Obj) and child.compute(5) == 5 * 3 + 77 + 1000
events = []
child.connect('child-event', lambda o, x: events.append(x))
child.emit('child-event', 7)
assert events == [7]

# Constructors, methods, functions, out parameters, GError
v = GidgenExport.Virt.new_with_title('t')
assert v.props.title == 't'
assert v.describe(None) == 't' and v.describe('x') == 'x t'
assert v.divide(7, 2) == 3
try:
    v.divide(1, 0)
    assert False
except GLib.Error as e:
    assert e.domain == 'gidgen-export' and e.code == 3 and e.message == 'division by zero'
try:
    v.divide(1, -1)
    assert False
except GLib.Error as e:
    assert e.domain == 'gid-d-exception-quark' and e.code == 0 and e.message == 'negative divisor'
length, upper, chars, same = v.stats('abc')
assert length == 3 and upper == 'ABC' and chars == ['a', 'b', 'c'] and same == v
assert GidgenExport.Virt.abi_version() == 3

# New virtual methods: D implementation, D subclass, Python subclass
assert v.scale(5) == 10
v.join('a', 'b')
assert v.props.title == 'ab'
assert GidgenExport.VirtChild().scale(5) == 11


class PyVirt(GidgenExport.Virt):
    def do_scale(self, x):
        return x * 100

    def do_join(self, a, b):
        self.joined = a + '+' + b


p = PyVirt()
assert p.scale(5) == 500  # The exported invoker calls the Python override
p.join('x', 'y')
assert p.joined == 'x+y'

# Interface defined in D: D implementation and Python implementation
assert isinstance(v, GidgenExport.Shape) and v.sides() == 4
v.label('z')
assert v.props.title == 'label z'


class PyShape(GObject.Object, GidgenExport.Shape):
    def do_sides(self):
        return 3

    def do_label(self, text):
        self.text = text


s = PyShape()
assert s.sides() == 3  # Invoker through the interface struct
s.label('q')
assert s.text == 'q'

# GInitiallyUnowned (floating reference), auto-exported members
f = GidgenExport.Floating.new()
assert isinstance(f, GObject.InitiallyUnowned)
a = GidgenExport.Auto()
a.props.level = 4
assert a.add(1) == 5 and GidgenExport.Auto.twice(4) == 8

# All D objects are finalized once Python drops them
del obj, child, v, p, s, f, a, same
gc.collect()
for i in range(10):
    if lib.gidgen_export_live_dobjects() == start:
        break
    lib.gidgen_export_collect()
assert lib.gidgen_export_live_dobjects() == start, (lib.gidgen_export_live_dobjects(), start)

print('gi test passed (%d symbols)' % len(symbols))
