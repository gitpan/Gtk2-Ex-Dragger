#!/usr/bin/perl

# Copyright 2007, 2008 Kevin Ryde

# This file is part of Gtk2-Ex-Dragger.
#
# Gtk2-Ex-Dragger is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 3, or (at your option) any later
# version.
#
# Gtk2-Ex-Dragger is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License along
# with Gtk2-Ex-Dragger.  If not, see <http://www.gnu.org/licenses/>.


use strict;
use warnings;
use Gtk2::Ex::Dragger;
use Test::More tests => 4;

ok ($Gtk2::Ex::Dragger::VERSION >= 4);
ok (Gtk2::Ex::Dragger->VERSION  >= 4);

SKIP: {
  require Gtk2;
  if (! Gtk2->init_check) { skip 'due to no DISPLAY available', 2; }

  {
    my $widget = Gtk2::DrawingArea->new;
    my $adj = Gtk2::Adjustment->new (100, -100, 1000, 10, 100, 800);
    my $dragger = Gtk2::Ex::Dragger->new (widget => $widget,
                                          hadjustment => $adj);

    require Scalar::Util;
    Scalar::Util::weaken ($dragger);
    is (defined $dragger ? 'defined' : 'not defined',
        'not defined',
        'garbage collect when weakened');
  }

  {
    my $widget = Gtk2::DrawingArea->new;
    my $adj = Gtk2::Adjustment->new (100, -100, 1000, 10, 100, 800);
    my $dragger = Gtk2::Ex::Dragger->new (widget => $widget,
                                          hadjustment => $adj);

    Scalar::Util::weaken ($widget);
    is (defined $widget ? 'defined' : 'not defined',
        'not defined',
        'attached widget garbage collect when weakened');
  }
}

exit 0;
