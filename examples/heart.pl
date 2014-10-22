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


# This example has a label in a viewport scrolled horizontally only.
#
# As a bit of fun a heart cursor is turned on and off with a timer.  It's
# got a positive priority so overrides the cursor in the dragger call.  You
# can in general choose whether you want the dragger or the base cursor to
# have precedence.  Notice the grab inheritance means the cursor continues
# to apply as the mouse drag moves outside the normal window (a normal X
# feature).
#

use strict;
use warnings;
use Gtk2 '-init';
use Gtk2::Ex::Dragger;
use Gtk2::Ex::WidgetCursor;

my $toplevel = Gtk2::Window->new('toplevel');
$toplevel->signal_connect (destroy => sub { Gtk2->main_quit });

my $viewport = Gtk2::Viewport->new;
$toplevel->add ($viewport);

# a fixed width but -1 to inherit the label's preferred height
$viewport->set_size_request (300, -1);

my $label = Gtk2::Label->new
  ("Drag with mouse button 1 to move this long line to the left and right.  There's no scrollbar to show where you're up to, but that's ok, a scrollbar is only ever an extra visual indication; you can perfectly well use a dragger without one, it's the adjustment and scrollable  window which are the keys.");
$viewport->add ($label);

my $dragger = Gtk2::Ex::Dragger->new
  (widget      => $viewport,
   hadjustment => $viewport->get_hadjustment,
   cursor      => 'sb-h-double-arrow');

$viewport->signal_connect
  (button_press_event => sub {
     my ($viewport, $event) = @_;
     if ($event->button == 1) {
       print __FILE__.": start button $viewport\n";
       $dragger->start ($event);
       return 1; # don't propagate
     } else {
       return 0; # do propagate
     }
   });

my $heart = Gtk2::Ex::WidgetCursor->new (widget => $viewport,
                                         cursor => 'heart',
                                         priority => 10);
sub beat {
  $heart->active (! $heart->active);   # toggle
  return 1; # continue timer
}
Glib::Timeout->add (800, \&beat);  # 800 milliseconds

$toplevel->show_all;
Gtk2->main;
exit 0;
