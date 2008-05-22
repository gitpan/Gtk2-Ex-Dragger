#!/usr/bin/perl

# Copyright 2008 Kevin Ryde

# This file is part of Gtk2-Ex-Dragger.
#
# Gtk2-Ex-Dragger is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3, or (at your option) any later
# version.
#
# Gtk2-Ex-Dragger is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License along
# with Gtk2-Ex-Dragger.  If not, see <http://www.gnu.org/licenses/>.


# This example adds draggability to a Gtk2::TextView widget with
# Control-Button1.  Plain Button1 is left as the text selection.
#
# The "confine" option is set on the dragger, as an example of how it
# restricts mouse pointer movement to the actual scrollable range.  This is
# partly a matter of personal preference, but it's normally a pretty good
# way to show you you've hit the scrollable limit.
#
# The $heading_eventbox copes with a perversity of Gtk 2.12.9.  A no-window
# widget like Gtk2::Label in the heading area doesn't get expose events
# propagated to it from the heading window during scrolls.  That's something
# containers are normally meant to do, and if GtkTextView used some of the
# ordinary GtkContainer it'd get it for free.  But at any rate using a
# Gtk2::EventBox forces a subwindow and gets exposes (in that subwindow) to
# the label.
#

use strict;
use warnings;
use Gtk2 '-init';
use Gtk2::Ex::Dragger;

my $toplevel = Gtk2::Window->new('toplevel');
$toplevel->signal_connect (destroy => sub { Gtk2->main_quit });
$toplevel->set_default_size (300, 300);

my $scrolled = Gtk2::ScrolledWindow->new;
$scrolled->set(hscrollbar_policy => 'always',
               vscrollbar_policy => 'always');
$toplevel->add ($scrolled);

my $textview = Gtk2::TextView->new;
$scrolled->add ($textview);

my $heading_eventbox = Gtk2::EventBox->new;
my $heading = Gtk2::Label->new ("\nDrag with Control-Button1\n");
$heading_eventbox->add ($heading);
my $req = $heading_eventbox->size_request;
$textview->set_border_window_size ('top', $req->height);
$textview->add_child_in_window ($heading_eventbox, 'top', 20,0);

my $textbuf = Gtk2::TextBuffer->new;
$textview->set_buffer ($textbuf);

{ # textbuf contents from this file itself
  open (my $fh, '<', __FILE__) or die $!;
  local $/; # slurp
  my $str = <$fh>;
  close $fh or die;
  $textbuf->set_text ($str);
}

my $dragger = Gtk2::Ex::Dragger->new
  (widget      => $textview,
   hadjustment => $scrolled->get_hadjustment,
   vadjustment => $scrolled->get_vadjustment,
   cursor      => 'fleur',
   confine     => 1);

$textview->signal_connect
  (button_press_event => sub {
     my ($textview, $event) = @_;
     if ($event->button == 1 && $event->state == 'control-mask') {
       print __FILE__.": start drag in textview\n";
       $dragger->start ($event);
       return 1; # don't propagate event

     } else {
       return 0; # do propagate event
     }
   });

$toplevel->show_all;
Gtk2->main;
