#!/usr/bin/perl -w

# Copyright 2007, 2008, 2009, 2010 Kevin Ryde

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

use 5.008;
use strict;
use warnings;
use Test::More tests => 9;

use lib 't';
use MyTestHelpers;
BEGIN { MyTestHelpers::nowarnings(); }

require Gtk2::Ex::Dragger;

my $want_version = 8;
{
  is ($Gtk2::Ex::Dragger::VERSION, $want_version, 'VERSION variable');
  is (Gtk2::Ex::Dragger->VERSION,  $want_version, 'VERSION class method');
  ok (eval { Gtk2::Ex::Dragger->VERSION($want_version); 1 },
      "VERSION class check $want_version");

  my $check_version = $want_version + 1000;
  ok (! eval { Gtk2::Ex::Dragger->VERSION($check_version); 1 },
      "VERSION class check $check_version");
}

require Gtk2;
MyTestHelpers::glib_gtk_versions();

{
  my $widget = Gtk2::DrawingArea->new;
  my $adj = Gtk2::Adjustment->new (100, -100, 1000, 10, 100, 800);
  my $dragger = Gtk2::Ex::Dragger->new (widget => $widget,
                                        hadjustment => $adj);

  ok ($dragger->VERSION >= $want_version, 'VERSION object method');
  ok (eval { $dragger->VERSION($want_version); 1 },
      "VERSION object check $want_version");
  my $check_version = $want_version + 1000;
  ok (! eval { $dragger->VERSION($check_version); 1 },
      "VERSION object check $check_version");

  require Scalar::Util;
  Scalar::Util::weaken ($dragger);
  is ($dragger, undef, 'garbage collect when weakened');
}

{
  my $widget = Gtk2::DrawingArea->new;
  my $adj = Gtk2::Adjustment->new (100, -100, 1000, 10, 100, 800);
  my $dragger = Gtk2::Ex::Dragger->new (widget => $widget,
                                        hadjustment => $adj);

  Scalar::Util::weaken ($widget);
  is ($widget, undef, 'attached widget garbage collect when weakened');
}

exit 0;
