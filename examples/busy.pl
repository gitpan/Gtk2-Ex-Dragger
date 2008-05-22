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


# This example shows the interaction between the WidgetCursor "busy" and a
# drag.
#
# Basically the busy cursor gets shown, but otherwise the state isn't lost
# and the cursor put back when unbusy - which is pretty much what you want
# if a scroll provokes some time consuming activity.
#

use strict;
use warnings;
use Time::HiRes;
use Gtk2 '-init';
use Gtk2::Ex::WidgetCursor;
use Gtk2::Ex::Dragger;

my $toplevel = Gtk2::Window->new('toplevel');
$toplevel->signal_connect (destroy => sub { Gtk2->main_quit });

my $viewport = Gtk2::Viewport->new;
$toplevel->add ($viewport);

my $label = Gtk2::Label->new
  ("
Drag
with
mouse
button-1
to move
this
up and
down.
The
busy
indication
comes and
goes on
a timer
and if
you
keep
moving
the
mouse
you can
extend
the
busy
since
it doesn't
get
back to
idle
state
to be
turned
off.
");
$viewport->add ($label);

# unspecified width to get from label, but fixed lesser height
$viewport->set_size_request (-1, 100);

my $dragger = Gtk2::Ex::Dragger->new
  (widget => $viewport,
   vadjustment => $viewport->get_vadjustment,
   confine => 1);

$viewport->signal_connect
  (button_press_event => sub {
     my ($viewport, $event) = @_;
     if ($event->button == 1) {
       $dragger->start ($event);
       return 1; # don't propagate
     } else {
       return 0; # do propagate
     }
   });


sub busy {
  Gtk2::Ex::WidgetCursor->busy;
  Time::HiRes::usleep (200_000);   # 200 milliseconds
  return 1; # continue timer
}
Glib::Timeout->add (800, \&busy);  # 800 milliseconds

$toplevel->show_all;
Gtk2->main;
exit 0;
