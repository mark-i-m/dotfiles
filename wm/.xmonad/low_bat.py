from sys import argv, exit
from gi.repository import Gtk

def show(rem):
    dialog = Gtk.MessageDialog(None, 0, Gtk.MessageType.ERROR,
                Gtk.ButtonsType.OK, "Low Battery")
    dialog.format_secondary_text("Battery is at %d%%" % rem)
    dialog.run()

    dialog.destroy()

if len(argv) < 2:
    exit(1)

show(int(argv[1]))
